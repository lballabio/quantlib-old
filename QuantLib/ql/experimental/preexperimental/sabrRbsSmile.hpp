/*! \file sabrRbsSmile.hpp
    \brief swaption smile which uses sabr interpolation "near" atm and an rbs parametric smile for extrapolation
	atm of underlying cube is always exactly reproduced
	vol spreads are interpolated linearly in option and swap maturity direction, flat extrapolation is used
	these spreads are added to base cube atm vol
	Peter Caspers 
*/

#include <ql/quantlib.hpp>
#include <sabrModel.hpp>
#include <RbsSmile.hpp>
#include <iostream>
#include <fstream>
#include <boost/property_tree/detail/rapidxml.hpp>
#include <map>

#ifndef quantlib_sabrRbsSmile_hpp
#define quantlib_sabrRbsSmile_hpp

#define MINSTRIKE 0.0010   // left from this strike, this strike is used always
//#define MINDISTATM 0.00001 // below this difference strike is treated as atm

using namespace boost;
using namespace std;

namespace QuantLib {

	/*! sabr/parametric swaption smile with the purpose to match to cms market */
	
	class SabrRbsSmile {
		public:	
			/*! smile is set up by
			- swaption vol cube (as the basis)
			- left and right end point (relative to atm) defining the interpolation area, outside extrapolation is used
			- option tenors, swap tenors w.r.t. sabr / parametric smiles are calibrated
			- start values for sabr calibration (alpha, nu, rho. beta is taken from time termstructure for beta below)
			- option, underlying cms calibration termstructure (given as pillars) for parameters mu, nu (parametric) and beta (sabr)
			- matrix for mu, nu, beta corresponding to cms calibration termstructure
			- strike spreads (relative to atm) defining the strikes on which SABR models are calibrated
			- vegaWeighted = true then difference in implied vols is weighted by vega
			- acceptRmse rmse for sabr calibration below which sabr smile is accepted
			- rejectRmse rmse for sabr calibration above which fall back smile is used insted. between these two, model is accepted (but more start values are tried to find a better fit)
			- haltonIterations the give start values plus a halton sequence of this length is tried to achieve acceptRmse
			- optimizer, endcriteria are the optimizer and the end criteria for optimization
			- BLACKACCURACY = accuracy to convert price to vol
			- hDiff is used to compute numerically first and second derivatives
			*/
			SabrRbsSmile(boost::shared_ptr<SwaptionVolatilityCube>& volCube,
				const vector<Period>& optionTenors, const vector<Period>& swapTenors,
				const Matrix& alpha, const Matrix& nu, const Matrix& rho,
				double leftBound, double rightBound, 
				const vector<Period>& optionPillars, const vector<Period>& underlyingPillars,
				const Matrix& pMu, const Matrix& pNu, const Matrix& beta,
				vector<double>& strikeSpreads, bool vegaWeighted=true, double acceptRmse=0.0020, double rejectRmse=0.02,
				int haltonIterations=150,
				const boost::shared_ptr<EndCriteria>& endCriteria=boost::shared_ptr<EndCriteria>(),
                const boost::shared_ptr<OptimizationMethod>& optMethod=boost::shared_ptr<OptimizationMethod>(),
				double BLACKACCURACY=1.0E-5,
				double hDiff=1.0E-4);
			
			/*! get volatility from sabr / parametric smile.  
				if spread true then strike=atm+spread is calculated, otherwise strike must be  given 
				if market is true then the vol from the base cube is returned  
				if sabr calibration is rejected (rmse>rejectRmse) also the vol from the base cube is returned as fall back 
				bilinear interpolation (with flat extrapolation) is used if option / swap tenor does not fall on pillar */
			double volatility(const Date& fixing, const Period& tenor, const double& strike0, bool spread=false, bool market=false);
			double volatility(const Period& option, const Period& tenor, const double& strike0, bool spread=false, bool market=false);
			
			/*! preset sabr / parametric model to get volatility more efficiently */
			bool setFastVolatility(const Date& fixing, const Period& tenor);
			/*! get volatility from preset sabr / parametric smile model 
			    note that spread mode = true and market = true is not available here*/
			double getFastVolatility(const double& strike);

			/*! get option price (not discounted) from sabr / parametric smile (only on defined pillars) 
				if forceSabr is true the sabr model (or fall back cube) is always used (never rbs), needed for rbs smile setup */
			double optionPrice(const Date& fixing, const Period& tenor, const double& strike0, const Option::Type type, bool spread=false, bool market=false, bool forceSabr=false);
			double optionPrice(const Period& option, const Period& tenor, const double& strike0, const Option::Type type, bool spread=false, bool market=false, bool forceSabr=false);

			/*! get matrix of sabr parameters and rmse
			    rmse is -1.0 if calibration failed
				best rmse is returned even if calibration is rejected
				in both cases (=no valid sabr model) the start values for alpha, nu, rho are returned */
			Matrix sabrAlpha() { return alpha_; }
			Matrix sabrNu() { return nu_; }
			Matrix sabrRho() { return rho_; }
			Matrix sabrRmse() { return sabrRmse_; }

			/*! get matrix of sabr beta, rbs smile mu and nu on option / swap pillars
			    if a sabr model is not valid, -1 is returned for beta*/
			Matrix sabrBeta();
			Matrix rbsMu();
			Matrix rbsNu();

			/*! get matrix of sabr beta, rbs smile mu and nu on cms calibration termstructure */
			Matrix sabrBetaTs() { return beta_; }
			Matrix rbsMuTs() { return pMu_; }
			Matrix rbsNuTs() { return pNu_; }

			/*! get underlying vol cube */
			boost::shared_ptr<SwaptionVolatilityCube> volCube() { return volCube_; }

			/*! get option pillar times */
			vector<double> optionPillarTimes()  { return optTimes_; }

			/*! set parameter mu at a pillar */
			bool setPillarMu(int optPillar, int undPillar, double mu);
			/*! set parameter nu at a pillar */
			bool setPillarNu(int optPillar, int undPillar, double nu);
			/*! set parameter beta at a pillar */
			bool setPillarBeta(int optPillar, int undPillar, double beta);
			/*! get pillar mu */
			double pillarMu(int optPillar, int undPillar) { return pMu_[undPillar][optPillar]; }
			/*! get pillar nu */
			double pillarNu(int optPillar, int undPillar) { return pNu_[undPillar][optPillar]; }
			/*! get pillar beta */
			double pillarBeta(int optPillar, int undPillar) { return beta_[undPillar][optPillar]; }
			
			/*! get mu, nu, beta for given option time and swap time */
			double mu(double optMaturity, double undMaturity) { return muInterpol_(optMaturity,undMaturity,true); }
			double nu(double optMaturity, double undMaturity) { return nuInterpol_(optMaturity,undMaturity,true); }
			double beta(double optMaturity, double undMaturity) { return betaInterpol_(optMaturity,undMaturity,true); }
			
			/*! get SABR parameters for (fixing, tenor) */
			vector<double> sabrParameters(const Date& fixing, const Period& tenor);
			vector<double> sabrParameters(const Period& option, const Period& tenor);

			/*! get RBS Smile parameters for (fixing, tenor) */
			vector<double> rbsParameters(const Date& fixing, const Period& tenor);
			vector<double> rbsParameters(const Period& option, const Period& tenor);
			
			/*! write murex xml file. if inPath is given, atms in this xml file are used */
			bool writeMurexFile(string outPath, string nickname, string date, string swap, vector<string>& optionNames, vector<string>& swapNames, 
				vector<Period>& optionTenors,vector<Period>& swapTenors,
				vector<double> strikeSpreads, string inPath="");

			/*! recalibrate models on pillar pair (sabr and rbs smile) w.r.t. given 
				termstructure of beta, mu, nu (as set by methods setPillarMu ...)
			    returns true if succesful, false otherwise (no changes are made then in the model, see sabrBeta(), rbsMu(), rbsNu(),
				though given termstructure of mu, nu, beta returned by rbsMuTs(), rbsNuTs(), sabrBetaTs() ... remains same) 
				if calSabr false only the rbs smiles are recalibrated (e.g. if only mu, nu was changed, not beta */
			bool recalibrate(const Period& option, const Period& tenor, bool calSabr);

			/*! recalibrate all models, returns false if one recalibration is not succesfull
				if calSabr false only the rbs smiles are recalibrated (e.g. if only mu, nu was changed, not beta */
			bool recalibrate(bool calSabr);

			/*! recalibrate all models with option tenor leq given option pillar and underlying equal to given underlying pillar */
			bool recalibrate(const int maxOptPillar, const int undPillar, bool calSabr);

			/*! return fixing dates of cube */
			vector<Date> fixingDates() { return fixings_; }
		
		private:

			/*! set up all sabr models */
			void setupSabrModels();

			/*! set up all rbs smiles */
			void setupRbsSmiles();
			
			/*! get time from referenceDate w.r.t. day counter of input cube */
			double time(const Date& date);

			/*! get sabr model for given (fixing date, swap tenor) */
			boost::shared_ptr<SabrModel> sabrModel(const Period& option, const Period& swap);
			boost::shared_ptr<SabrModel> sabrModel(const Date& fixing, const Period& swap);

			/*! get rbs smile for given (fixing date, swap Tenor) */
			boost::shared_ptr<RbsSmile> rbsSmile(const Period& option, const Period& swap);
			boost::shared_ptr<RbsSmile> rbsSmile(const Date& fixing, const Period& swap);
			
			boost::shared_ptr<SwaptionVolatilityCube> volCube_; // base cube
			vector<Period> optionTenors_,swapTenors_; // model pillars (sabr / rbs smile)
			vector<Date> fixings_; // fixing dates corresponding to option tenors
			Matrix alpha_,nu_,rho_; // matrix of sabr parameters
			Matrix sabrRmse_; // matrix of sabr rmse (calibration error)
			
			vector<Period> optPillars_, undPillars_; // term structure of beta, mu, nu
			vector<double> optTimes_, undTimes_; // corresponding times (for interpolation)
			Matrix pMu_,pNu_,beta_; // corresponding pillar parameters

			double leftBound_, rightBound_,h_; // sabr region, step for numerical differentation
			vector<double> strikeSpreads_; // smile spreads (relative to atm) on which sabr is calibrated
			
			bool vegaWeighted_;
			double acceptRmse_,rejectRmse_; // sabr optimization parameters
			int haltonIterations_;
			const boost::shared_ptr<EndCriteria>& endCriteria_;
            const boost::shared_ptr<OptimizationMethod>& optMethod_;

			Interpolation2D muInterpol_,nuInterpol_,betaInterpol_; // linear interpolation for beta, mu, nu

			Date referenceDate_; // underlying cube data
			Calendar calendar_;
			BusinessDayConvention bdc_;
			DayCounter dc_;
			
			map<pair<Date,Period>,bool> sabrValid_; // maps fixing, underlying tenor to true (sabr is well calibrated) or false (sabr could not be calibrated)
			map<pair<Date,Period>,bool>::iterator sabrValidIter_; // iterator
			map<pair<Date,Period>,boost::shared_ptr<SabrModel>> sabrModels_; // maps fixing, underlying tenor to sabr model
			map<pair<Date,Period>,boost::shared_ptr<SabrModel>>::iterator sabrModelsIter_; // iterator

			map<pair<Date,Period>,boost::shared_ptr<RbsSmile>> rbsSmiles_; // maps fixing, underlying tenor to rbs parametric smiles
			map<pair<Date,Period>,boost::shared_ptr<RbsSmile>>::iterator rbsSmilesIter_; // iterator

			double BLACKACCURACY, OPTACCURACY;
			
			// member variable for setFastVolatility(), getFastVolatility()
			double fastAtm11_,fastAtm12_,fastAtm21_,fastAtm22_,fastAtm_;
			double fastAtmVol11_,fastAtmVol12_,fastAtmVol21_,fastAtmVol22_;
			double fastOptionTime_,fastOptionTime1_,fastOptionTime2_;
			double fastSwapTime_,fastSwapTime1_,fastSwapTime2_;
			boost::shared_ptr<SabrModel> fastSabrModel11_,fastSabrModel12_,fastSabrModel21_,fastSabrModel22_;
			boost::shared_ptr<RbsSmile> fastRbsModel11_,fastRbsModel12_,fastRbsModel21_,fastRbsModel22_;
			bool fastOptEq_,fastSwapEq_;
			Date fastFixing_;
			Period fastTenor_;
			bool fastValid_;

	};

}


#endif


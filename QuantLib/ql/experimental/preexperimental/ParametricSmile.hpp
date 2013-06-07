/*! \file parametricSmile.hpp
    \brief parametric swaption smile for extrapolation with match to cms market
	... this class needs to be tidied up, because the code is duplicated at different places ....
	Peter Caspers 
*/

#include <ql/quantlib.hpp>
#include <iostream>
#include <fstream>

#ifndef quantlib_parametricSmile_hpp
#define quantlib_parametricSmile_hpp

#define IMPLACC 1.0E-6   // accuracy when converting price to vol
#define MINSTRIKE 0.0010 // left from this strike, this strike is used always


using namespace boost;
using namespace std;

namespace QuantLib {

	/*! parametric swaption smile for extrapolation with match to cms market */
	
	class ParametricSmile {
		public:	
			/*! parametric smile is set up by
			- swaption vol cube
			- left and right end point (relative to atm) where extrapolation should start
			- hDiff is used to compute numerically first and second derivatives
			*/
			ParametricSmile(boost::shared_ptr<SwaptionVolatilityCube> volCube,
				double leftBound, double rightBound, vector<double> maturities,
				vector<double> mu, vector<double> nu, double hDiff=1.0E-4);
			
			/*! get volatility from parametric smile. Inside the boundaries, just the vol
			    from the volCube is returned. Outside the parametric extrapolation is used 
				if spread true then strike=atm+spread is calculated, otherwise strike must be  given */
			double volatility(const Date& fixing, const Period& tenor, const double& strike0, bool spread=false);
			double volatility(const Period& option, const Period& tenor, const double& strike0, bool spread=false);

			/*! get call (strike>atm) / put (strike<atm) price from parametric smile (without discounting) */
			double callPutPrice(const Date& fixing, const Period& tenor, const double& strike0, bool spread=false);

			/*! get underlying vol cube */
			boost::shared_ptr<SwaptionVolatilityCube> volCube() { return volCube_; }

			/*! set fixing and tenor for getFastVol() */
			bool setFastVol(const Date& fixing, const Period& tenor);
			/*! get vol for parameters set by setFastVol() */
			double getFastVol(const double& strike0);

			/*! set parameter mu at a pillar */
			bool setPillarMu(int pillar, double mu);
			/*! set parameter nu at a pillar */
			bool setPillarNu(int pillar, double nu);
			/*! get pillar mu */
			double pillarMu(int pillar) { return mu_[pillar]; }
			/*! get pillar nu */
			double pillarNu(int pillar) { return nu_[pillar]; }

			double mu(double maturity) { return muInterpol_(maturity,true); }
			double nu(double maturity) { return nuInterpol_(maturity,true); }

			bool writeMurexFile(string path,vector<Period>& optionTenors,vector<Period>& swapTenors,
				vector<double> strikeSpreads);
		
		private:
			boost::shared_ptr<SwaptionVolatilityCube> volCube_;
			double leftBound_, rightBound_,h_;
			vector<double> maturities_,mu_,nu_;
			Interpolation muInterpol_,nuInterpol_;
			Date fastFixing_;
			Period fastTenor_;
			double fastTfix_,fastAtm_,fastMu_,fastNu_,fastLA_,fastLB_,fastLC_,fastRA_,fastRB_,fastRC_;
			double fastLv0_,fastRv0_;

	};

}


#endif


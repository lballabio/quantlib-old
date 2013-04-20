/*! \file sabrModel.hpp
    \brief sabr model for single smile
	Peter Caspers 
*/

#include <ql/quantlib.hpp>
#include <complexErf.hpp>
#include <stdio.h>

#ifndef quantlib_sabrModel_hpp
#define quantlib_sabrModel_hpp

using namespace boost;
using namespace std;

namespace QuantLib {

	/*! sabr model to value european options */
	
	class SabrModel {
		public:	
			/*! sabr model, prices are converted to black vols with accuracy blackAccuracy,
				if the distance of the strike to atm is below minDistAtm the classic formula treats the case as atm,
			    if a given strike is below minStrike volatility is extrapolated flat
			*/
			SabrModel(Real alpha, Real beta, Real nu, Real rho,
				double minStrike=0.0001,double blackAccuracy=1.0E-5, double minDistAtm=1.0E-5);
			
			/*! get undiscounted option price */
			Real optionPrice(Real forward, Real strike, Real maturity,Option::Type type=Option::Call);

			/*! implied black vola */
			Real impliedVola(Real forward, Real strike, Real maturity); 
			
			/*! get monte carlo price (divided by annuity) for call option */
			// Real mcPrice(Real forward, Real strike, Real maturity, Size timestepsPerYear=24, Size numberOfPaths=10000); 
			
			/*! recalibrate the models parameters */
			void recalibrate(Real alpha, Real beta, Real nu, Real rho);

			/*! recalibrate the models parameters without checking them (for calibration) */
			void recalibrateWithoutCheck(Real alpha, Real beta, Real nu, Real rho);
			
			/*! calibrate the model to a smile given by the forward atm level, option maturity, strikes,
			    implied black volas for the given strikes. model parameters can be excluded from calibration
				by fixing them to their current value.
				calibration is done w.r.t. implied vol differences, unweighted or weighted by vega.
				Besides given start value a maximum of halton iterations is tried as start value to reach accept rmse (=valid calibration). 
				The RMSE of a valid calibration is returned or -1.0 if no valid calibration could be reached. */
			Real calibrate(Real forward, Real maturity, std::vector<Real> strikes, std::vector<Real> blackVolas,
				bool alphaFixed, bool betaFixed, bool nuFixed, bool rhoFixed, bool vegaWeighted=false,
				double acceptRmse=0.0020, int haltonIterations=150,
				const boost::shared_ptr<EndCriteria>& endCriteria=boost::shared_ptr<EndCriteria>(),
                const boost::shared_ptr<OptimizationMethod>& optMethod=boost::shared_ptr<OptimizationMethod>());

			/*! return model parameters */
			Real alpha();
			Real beta();
			Real nu();
			Real rho();

			/*! returns a vector containing the model parameters (alpha,beta,nu,rho) */
			std::vector<Real> modelParameters();
		
		private:
			Real alpha_,beta_,nu_,rho_;
			Real blackAccuracy_, minDistAtm_, minStrike_;
	};

	/*! Cost Function for calibration method */
	class SabrModelCostFunction : public CostFunction {
		public:
			SabrModelCostFunction(SabrModel* model,Real forward,Real maturity,
										std::vector<Real> strikes,std::vector<Real> blackVolas,
										bool alphaFixed,bool betaFixed,bool nuFixed,bool rhoFixed,bool vegaWeighted);
			Real value(const QuantLib::Array& x) const;
			Disposable<Array> values(const QuantLib::Array& x) const;

		private:
			SabrModel* model_;
			Real forward_,maturity_;
			std::vector<Real> strikes_,blackVolas_,weights_;
			bool alphaFixed_,betaFixed_,nuFixed_,rhoFixed_,vegaWeighted_;
			Size n_;
	};
	
}

#endif


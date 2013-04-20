/*! \file simpleSVModel.hpp
    \brief stoch vol model with constant parameters
	Peter Caspers 
*/

#include <ql/quantlib.hpp>
#include <simulatedAnnealing.hpp>
//#include <gsl_integration.h>
//#include <gsl_errno.h> 

#ifndef quantlib_simpleSVModel_hpp
#define quantlib_simpleSVModel_hpp

#define FOURIERACCURACY 1.0E-5   	  // accuracy for the price from the Fourier Integral calculation
#define MINFOURIERACC 1.0E-5          // minimal accuracy for the fourier integral itself
//#define MAXINTEGRATIONSIZE1 256       // max size in qawo integration
//#define MAXINTEGRATIONSIZE2 512       // max size in qawf integration
#define FOURIERBOUND1 500.0			  // integration bound for Gauss Labatto
#define MAXGLITERATIONS 1000		  // max Gauss Labatto Iterations
#define MAXFOURIERPARTS 5			  //  max Gauss Labatto Intervals
//#define BOUNDEPS 1.0E-2			      // transformed integration is done from 0.0 to 1.0-BOUNDEPS

#define BLACKACCURACY 1.0E-4	 	  // accuracy for the Black implied vola calculation
#define OPTACCURACY 1.0E-8            // accuracy for optimization

using namespace boost;
using namespace std;


namespace QuantLib {

	//int initGslQawo(Size qawoSize, Size qawfSize);
	//int freeGslQawo();

	/*! simple stochastic volatility model to value european options, dynamic is
	   dS = lambda * ( b*S + (1-b)*forward )*sqrt(V) dB
	   dV = theta * (  omega - V ) dt + eta*sqrt(V) dW
	   dB dW = 0 */
	
	
	class SimpleSVModel {
		public:	
			/*! simple stoch vol model is set up by
			- lambda volatility
			- b skew
			- theta mean reversion
			- eta volvol
			- omega long term value for variance process (normally 1.0)
			- varianceStartValue start value for variance process (normally 1.0)
			*/
			SimpleSVModel(Real lambda, Real b, Real theta, Real eta, Real omega, Real varianceStartValue);
			
			/*! get call option price divided by annuity (!) for a given
			- forward
			- strike
			- maturity
			*/
			Real callOptionPrice(Real forward, Real strike, Real maturity/*, Real upperBound=FOURIERBOUND*/);

			/*! get implied black vola for a given
			- forward
			- strike
			- maturity
			*/
			Real impliedVola(Real forward, Real strike, Real maturity/*, Real upperBound=FOURIERBOUND*/); 
			
			/*! get monte carlo price (divided by annuity) for call option */
			Real mcPrice(Real forward, Real strike, Real maturity, Size timestepsPerYear=24, Size numberOfPaths=10000); // returns model mc price for call option divided by annuity
			
			/*! recalibrate the models parameters
			- lambda volatility
			- b skew
			- theta mean reversion
			- eta volvol
			- omega long term value for variance process (normally 1.0)
			*/
			void recalibrate(Real lambda, Real b, Real theta, Real eta, Real omega);
			
			/*! calibrate the model to a given smile
			- forward the forward atm level
			- maturity the maturity of the option
			- strikes the strike levels of the smile
			- blackVolas the implied vols for the given strike levels
			- lambdaFixed if true volatility is not calibrated
			- bFixed if true skew is not calibrated
			- thetaFixed if true mean reversion is not calibrated
			- etaFixed if true volvol is not calibrated 
			- useImpliedVols if true calibration is done on implied vols, otherwise on prices (divided by annuity)
			- dontCalibrate if true the model is not calibrated, only rmse is returned
			The return value is the RMSE calibration result, followed by market implied vols and model implied vols */
			Real calibrate(Real forward, Real maturity, std::vector<Real> strikes, std::vector<Real> blackVolas,
				bool lambdaFixed, bool bFixed, bool thetaFixed, bool etaFixed, bool useImpliedVols=false,bool dontCalibrate=false);
			
			/*! return models volatility */
			Real lambda();
			/*! return models skew */
			Real b();
			/*! return models theta (mean reversion) */
			Real theta();
			/*! return models eta (volvol) */
			Real eta();
			/*! return models omega (long term value for variance process, normally 1.0) */
			Real omega();

			/*! return market implied vols (after calibration) */
			std::vector<Real> marketImpliedVols();
			/*! return model implied vols (after calibration) */
			std::vector<Real> modelImpliedVols();

			/*! returns a vector containing the model parameters (vol, skew, reversion, volvol, varianceStartValue, longTermVariance) */
			std::vector<Real> modelParameters();
		
		private:
			Real lambda_, b_, theta_, eta_, omega_, varianceStartValue_;
			std::vector<double> marketVols_, modelVols_;
			//boost::shared_ptr<GaussLegendreIntegration> legendre_; // for computation of fourier integral

	};

	/*! Helper class for price calculation in simple stoch vol model */
	class SimpleSVModelIntegrand {
		public:
			SimpleSVModelIntegrand(Real omega,Real theta,Real eta,Real varianceStartValue,Real forward,Real strike,
				Real maturity,Real t,Real omegaTilde,Real thetaHat);
			Real operator()(Real s) const;
			Real valueWithoutCosineTerm(Real s) const;

		private:
			Real omega_,theta_,eta_,varianceStartValue_,forward_,strike_,maturity_,t_,omegaTilde_,thetaHat_;
			Real factor_;
	};

	/*! Cost Function for calibration method */
	class SimpleSVModelCostFunction : public CostFunction {
		public:
			SimpleSVModelCostFunction(SimpleSVModel* model,Real forward,Real maturity,
										std::vector<Real> strikes,std::vector<Real> blackVolas,
										bool lambdaFixed,bool bFixed,bool thetaFixed,bool etaFixed, bool useImpliedVols);
			Real value(const QuantLib::Array& x) const;
			Disposable<Array> values(const QuantLib::Array& x) const;

		private:
			SimpleSVModel* model_;
			Real forward_,maturity_;
			std::vector<Real> strikes_,blackVolas_,blackPrices_;
			bool lambdaFixed_,bFixed_,thetaFixed_,etaFixed_,useImpliedVols_;
			Size n_;

	};

	/*! Trivial constraint class for calibration of simple stoch vol model */
	class SimpleSVModelConstraint : public Constraint {
            private:
            class Impl : public QuantLib::Constraint::Impl {
                public:
                bool test(const QuantLib::Array& params) const {
					return true; // the constraints are checked in the cost function
                }
            };
            public:
            SimpleSVModelConstraint()
				: Constraint(boost::shared_ptr<Constraint::Impl>(new Impl)) {}
	};

}

#endif


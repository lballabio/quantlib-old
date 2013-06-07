/*! \file effectiveParametersSVLfm.hpp
\brief Class that represents the functionalities for calibration of the stoch vol LMM
Peter Caspers
*/

#include <ql/quantlib.hpp>
#include <adaptiveRungeKutta.hpp>
#include <sigmaParametrization.hpp>
#include <betaParametrization.hpp>
#include <simulatedAnnealing.hpp>
#include <wgz_utilities.hpp>
#include <simpleSVModel.hpp>
#include <liborCurve.hpp>
#include <hestonCharacteristicFunction.hpp>
#include <effectiveParametersHelper.hpp>
#include <complexSimpson.hpp>

//#include <gsl_integration.h>

#ifndef quantlib_effectiveParametersSVLfm_hpp
#define quantlib_effectiveParametersSVLfm_hpp

// spread option approximation Antonov
//#define INTEGRALACCURACY 1.0E-4			// accuracy for the inner laplace integral (dxi2)
//#define OUTERINTEGRALACCURACY 1.0		// accuracy for the outer laplace integral (dxi1)
//#define INTEGRATIONBOUND 50.0		// 1 dim integrationb bound (zero strike case)
//#define INTEGRATIONBOUNDU 2.0	    // laplace integral bound for u 
//#define INTEGRATIONBOUNDV 10.0		// laplace integral bound for v (u,v) -> (xi1=u,xi2=alpha*u+v)
//#define MAXINTEGRATIONSIZE 10		// the size parameter for gsl qawo integration
//#define LAPLACEINTEGRALPOINTS 32	// the number of points used in legendre integral for inverse laplace outer integral

// spread option approximation Lutz
#define HERMITEPOINTS 64 // number of points for hermite integration (inner cmsso integral, lutz)
#define HSTEP 1.0E-4 // step to numerically compute the derivative of h (for newton algorithm)
#define HACC 1.0E-4 // accuracy for the zero of h
#define MAXEVALNEWTON 500 // max evaluation steps for newton algorithm (search zero of h)
#define DENSITYACCURACY 1.0E-8 // accuracy for computing gauss labatto integral for density f(v)
#define DENSITYLOWERINTEGRALBOUND 1.0E-6 // lower integration bound for computing density integral
#define INTVACCURACY 1.0E-6 // Accuracy for outer integral (by integrated variance)
#define INTVLOWERBOUND 0.1  // lower bound (is not (!) scaled with T) for outer integral
#define INTVUPPERBOUND 10.0  // upper bound (is scaled with T, i.e. value for T=1 here) for outer integral
#define MAXEVALGL 10000 // max function evaluations gauss labatto (density integral, outer integral (by int. variance)

using namespace boost;
using namespace std;

namespace QuantLib {

	/*! The stoch vol lmm has the following dynamics \f[
	dF(i,t) = \lambda(i,t) * ( b(i,t)*F(i,t) + (1-b(i,t))*F(i,0) ) * \sqrt{V} dB
	dV = \theta * (  \omega - V ) dt + \eta*\sqrt{V} dW
	dB dW = 0 
	\f]
	*/


	class EffectiveParametersSVLfm {
	public:
		/* Constructor of the model has the following inputs
		- sigma the volatility parametrization of the libors
		- beta the skew parametrization of the libors
		- rateTimes the libor time grid
		- initialRates the initial forward rates
		- omega the long term value for the variance process, should be normally set to 1.0
		- theta the mean reversion of the variance process
		- eta the volatility of the variance process
		- v0 the start value of the variance process, should be normally set to 1.0
		- fixingIndices the indices where the calibrating swaptions fix, each swaption represents a whole swaption smile
		- swapLengths the length of the calibrating swaptions (in time grid steps)
		- payFreqs the payment frequencies of the calibrating swaptions (expressed in time grid steps)
		- marketSkews the market effective skew parameters of the calibration swaption smiles
		- marketVolas the market effective volatilities of the calibration swaption smiles
		- cmssoFixings the indices where the calibrating cms spread options fix
		- cmssoPaymentDelays the payment delays of the calibrating cms spread options (expressed in time grid steps)
		- cmssoLengths1 cms spread options underlying swap rate 1 length (expressed in time grid steps)
		- cmssoLengths2 cms spread options underlying swap rate 2 length (expressed in time grid steps)
		- cmssoPayFreqs the payment frequencies of the underlying swap rates of the cms spread options
		- cmssoFlavors the flavor (1 call / cap, -1 put / floor) of the cms spread options
		- cmssoStrikes the strikes of the cms spread options
		- cmssoGroupStarts the cms spread options are grouped into quoted price sets. This is the first index of each group. Must start with 0.
		- cmssoGroupPremia the premia for the cms spread option groups
		*/
		EffectiveParametersSVLfm(boost::shared_ptr<AbcdVolRatioCorrSigma> sigma, boost::shared_ptr<PiecewiseConstantBeta> beta, // model parametrization
			std::vector<Time>& rateTimes,
			std::vector<Rate>& initialRates,
			Real meanLevel, // omega
			Real reversionSpeed, // theta
			Real volVar, // eta
			Real v0, // start value for variance process
			std::vector<Size>& fixingIndices, // fixing indices of calibrating swaptions
			std::vector<Size>& swapLengths, // swap lengths of calibrating swaptions
			std::vector<Size>& payFreqs, // payment frequencies of calibrating swaptions
			std::vector<Real>& marketSkews, // market skew parameters
			std::vector<Real>& marketVolas,  // market vola parameters------------------------------
			std::vector<Size>& cmssoFixings, // spread option fixing indices
			std::vector<Size>& cmssoPaymentDelays, // payment delays of spread options
			std::vector<Size>& cmssoLengths1, // swap rate 1 length for cmsso
			std::vector<Size>& cmssoLengths2, // swap rate 2 length for cmsso
			std::vector<Size>& cmssoPayFreqs, // payment frequencies of cmsso underlying swap rates
			std::vector<int>& cmssoFlavors, // flavour of cmsso (+1 call -1 put)
			std::vector<double>& cmssoStrikes, // strike of cmsso
			std::vector<Size>& cmssoGroupStarts, // defines first index for each group of optionlets must start with 0
			std::vector<double>& cmssoGroupPremia // market premia for each optionlet group
			); 

		/*! returns number of instruments (swaptions) */
		Size numberOfSwaptions() { return numberOfSwaptions_; };

		/*! returns number of cms spread options */
		Size numberOfCmsso() { return numberOfCmsso_; };

		/*! returns number of cms spread option groups */
		Size numberOfCmssoGroups() { return numberOfCmssoGroups_; };

		/*! return atm level of instrument (swaption) with given index */
		Real atm(Size instrument);

		/*! calibrate volatility, effective skew is taken from actual calibration status or
		(if no calibrateSkew() has been done so far) from market skews. Here we make the assumption
		that the market skews will be calibrated well in a later call of calibrateSkew() */ 
		Real calibrateVolatility();
		/*! returns effective volatility for given effective skew beta for an instrument */
		Real effectiveVolatility(Size instrument, Real effectivebeta);
		/*! returns effective volatility for cmsso (first=0 or second=1) underlying rate */
		Real effectiveVolatility(Size instrument, Size rate, Real effectivebeta);
		/*! recalibrates volatility parametrization and returns error vector */
		std::vector<Real> recalibrateVolatility(std::vector<Real> parameters);

		/*! calibrate skew */
		Real calibrateSkew();
		/*! precomputes the weights to compute the effective skew. Must be called before effectiveSkew() 
		whenever sigma_ is updated. */
		void precomputeSkewWeights();
		/*! returns effective skew for an instrument (swaption) */
		Real effectiveSkew(Size instrument);
		/*! returns effective skew for cmsso (first=0 or second=1 rate, for S (true) or for R (false) process)
		S means swap rate process, R means measury change process in the sense of the Antonov paper
		*/
		Real effectiveSkew(Size instrument, Size rate, bool sProcess);
		/*! recalibrates skew parametrization and returns error vector */
		std::vector<Real> recalibrateSkew(std::vector<Real> parameters);

		/*! return spread option price T-approximation (Lutz paper) (discounted expectation) */
		double spreadOptionPriceT(Size instrument);
		
		/*! return black vol of instrument (swaption) at strike atm+strikeoffset and price (divided by swap annuity!)
		(at index 0 and 1 of returned vector) */
		Real impliedBlackVolatility(Size instrument,double strikeOffset);
		/*! return mc price (divided by swap annuity) computed by mc in simple sv model (swaption) */
		double mcPrice(Size instrument,double strikeOffset);
		/*! return convexity adjusted swap rate for instrument (cmsso) for first (0) or second (1) swap rate */
		double adjustedSwapRate(Size instrument, Size rate);

	private:
		Real gPrimePrimeDivGPrime(double z, double b, double S0);

		boost::shared_ptr<AbcdVolRatioCorrSigma> sigma_;
		boost::shared_ptr<PiecewiseConstantBeta> beta_;
		std::vector<Time>& rateTimes_;
		std::vector<Rate>& initialRates_;
		Real omega_,theta_,eta_,v0_;

		std::vector<Size>& fixingIndices_;
		std::vector<Size>& swapLengths_;
		std::vector<Size>& payFreqs_;
		std::vector<Real>& marketSkews_;
		std::vector<Real>& marketVolas_;
		std::vector<Size>& cmssoFixings_;
		std::vector<Size>& cmssoPaymentDelays_;
		std::vector<Size>& cmssoLengths1_;
		std::vector<Size>& cmssoLengths2_;
		std::vector<Size>& cmssoPayFreqs_;
		std::vector<int>& cmssoFlavors_;
		std::vector<double>& cmssoStrikes_;
		std::vector<Size>& cmssoGroupStarts_;
		std::vector<double>& cmssoGroupPremia_;

		Size numberOfSwaptions_, numberOfCmsso_, numberOfCmssoGroups_;
		std::vector<Real> forwardSwapRates_; // forward swap rates at initial time for each instrument (swaptions)
		std::vector<Real> cmssoS1forwardRates_, cmssoS2forwardRates_, cmssoR1forwardRates_, cmssoR2forwardRates_; // forwards for cmsso
		std::vector<boost::shared_ptr<FunctionSigma>> sigmaNm_; // sigmaNm function for each instrument (swaptions)
		std::vector<boost::shared_ptr<FunctionSigma>> cmssoS1Sigma_, cmssoS2Sigma_, cmssoR1Sigma_, cmssoR2Sigma_; // sigmaNm functions for cmsso
		std::vector<boost::shared_ptr<FunctionVsSs>> vSsS_; //v^2*s^2 - function for each instrument (swaptions)
		std::vector<boost::shared_ptr<FunctionVsSs>> cmssoS1vSsS_,cmssoS2vSsS_,cmssoR1vSsS_,cmssoR2vSsS_; //v^2*s^2 - functions (cmsso)               
		std::vector<boost::shared_ptr<FunctionP>> pNm_; // pNm function for each instrument  (swaptions)
		std::vector<boost::shared_ptr<FunctionP>> cmssoS1pNm_, cmssoS2pNm_, cmssoR1pNm_, cmssoR2pNm_; // pNm functions (cmsso)
		std::vector<boost::shared_ptr<SigmaCovariance>> cmssoCov_;

		boost::shared_ptr<GaussLegendreIntegration> legendre_; // legendre2_; // for computation of effective skew integral, cmsso approximation
		Array legendreRoots_; //legendreRoots2_;
		Array legendreWeights_; //legendreWeights2_;
		boost::shared_ptr<GaussHermiteIntegration> hermite_; // for computing the inner cmsso integral (Lutz)
		Array hermiteRoots_,hermiteWeights_;
		vector<vector<vector<Real>>> alpha_; // precomputed alpha(l,i;n,m) for swaptions
		vector<vector<vector<Real>>> cmssoS1alpha_, cmssoS2alpha_, cmssoR1alpha_, cmssoR2alpha_; // precomputed alpha(l,i) for cmsso
		std::vector<Real> alphaDenom_; // precomputed integral over weights (swaptions)
		std::vector<Real> cmssoS1alphaDenom_, cmssoS2alphaDenom_, cmssoR1alphaDenom_, cmssoR2alphaDenom_; // precomputed integral over weights (cmsso)

		std::vector<Real> calibratedEffectiveSkews_; // initially = market skews, updated with every skew calibration (for swaptions)
		std::vector<Real> calibratedEffectiveVols_; // initially = 0.0, updated with every vol calibration (for cmsso)
	};

	/*! Cost Function for volatility calibration */
	class VolatilityCostFunction : public CostFunction {
	public:
		/*! the class must be initialized with the model */
		VolatilityCostFunction(EffectiveParametersSVLfm* model) : model_(model) {
			n_=model->numberOfSwaptions();
			np_=model->numberOfCmssoGroups();
		}
		/*! given generic parameters for volatility, this function computes the RMSE for these  parameters and returns it */
		Real value(const QuantLib::Array& x) const {
			Array res = values(x);
			double sum=0.0;
			for(int i=0;i<n_;i++) { // swaption loop
				sum+=res[i]*res[i];
			}
			for(int i=0;i<np_;i++) { // cmsso loop
				sum+=res[i+n_]*res[i+n_];
			}
			return sqrt(sum/(n_+np_));
		}
		/*! given generic parameters for volatility parametrization of the model,
		this function returns a vector with errors for each calibration instrument */
		Disposable<Array> values(const QuantLib::Array& x) const {
			// recalibrate model and compute error
			return vector2Array(model_->recalibrateVolatility(array2Vector(x)));
		}
	private:
		EffectiveParametersSVLfm* model_;
		Size n_; // number of swaptions
		Size np_; // number of cmsso groups
	};

	/*! This class is a trivial constraint for volatility calibration.
	The real check is made via penalty functions */
	class VolatilityConstraint : public Constraint {
	private:
		class Impl : public QuantLib::Constraint::Impl {
		public:
			bool test(const QuantLib::Array& params) const {
				return true; // the constraints are checked in the cost function
			}
		};
	public:
		VolatilityConstraint()
			: Constraint(boost::shared_ptr<Constraint::Impl>(new Impl)) {}
	};

	/*! Cost Function for skew calibration */
	class SkewCostFunction : public CostFunction {
	public:
		/*! the class must be initialized with the model */
		SkewCostFunction(EffectiveParametersSVLfm* model) : model_(model) {
			n_=model->numberOfSwaptions();
		}
		/*! given generic parameters for volatility, this function computes the RMSE for these  parameters and returns it */
		Real value(const QuantLib::Array& x) const {
			Array res = values(x);
			double sum=0.0;
			for(int i=0;i<n_;i++) {
				sum+=res[i]*res[i];
			}
			return sqrt(sum/n_);
		}
		/*! given generic parameters for volatility parametrization of the model,
		this function returns a vector with errors for each calibration instrument */
		Disposable<Array> values(const QuantLib::Array& x) const {
			// recalibrate model and compute error
			return vector2Array(model_->recalibrateSkew(array2Vector(x)));
		}
	private:
		EffectiveParametersSVLfm* model_;
		Size n_;
	};

	/*! This class is a trivial constraint for skew calibration.
	The real check is made via penalty functions */
	class SkewConstraint : public Constraint {
	private:
		class Impl : public QuantLib::Constraint::Impl {
		public:
			bool test(const QuantLib::Array& params) const {
				return true; // the constraints are checked in the cost function
			}
		};
	public:
		SkewConstraint()
			: Constraint(boost::shared_ptr<Constraint::Impl>(new Impl)) {}
	};



	/*! This function defines the ODE from Piterbarg, appdx E, (E.1), (E.2) */
	class FunctionPhiODEHelper : public FunctionRnR {
	public:
		/*! the constructor takes the stoch vol parameters
		- theta mean reversion of variance process
		- eta volvol
		- v0 start value of variance process (should be 1.0 normally)
		and the parameters
		- T maturity of option
		- mu see Piterbarg, Apdx E
		- sigma volatility approximation of swap rate
		*/
		FunctionPhiODEHelper(Real theta, Real eta, Real v0, Real T, Real mu, FunctionSigma& sigma) :
		  theta_(theta), eta_(eta), v0_(v0), T_(T), mu_(mu), sigma_(sigma) { }
		  /*! returns the definition of the ODE */
		  std::vector<Real> operator()(Real t, std::vector<Real> y) const {
			  std::vector<Real> res(2,0.0);
			  res[0] = -theta_*v0_*y[1];
			  res[1] = -theta_*y[1]-0.5*eta_*eta_*y[1]*y[1]+mu_*sigma_.squareValue(T_-t);
			  //printf("%f;%f\n",t,sigma_.squareValue(T_-t));
			  return res;
		  }
	private:
		Real theta_,eta_,v0_,T_,mu_;
		FunctionSigma& sigma_;
	};

	// same with sigma = 1 (for test purposes for phi_0)
	/*class FunctionPhiODEHelper2 : public FunctionRnR {
	public:
	FunctionPhiODEHelper2(Real theta, Real eta, Real v0, Real T, Real mu) :
	theta_(theta), eta_(eta), v0_(v0), T_(T), mu_(mu) { }
	std::vector<Real> operator()(Real t, std::vector<Real> y) const {
	std::vector<Real> res(2,0.0);
	res[0] = -theta_*v0_*y[1];
	res[1] = -theta_*y[1]-0.5*eta_*eta_*y[1]*y[1]+mu_;
	return res;
	}
	private:
	Real theta_,eta_,v0_,T_,mu_;
	};*/

	/*! function \f$ \phi_0 \f$, Piterbarg E.3. A right hand side can be given so that root finders can be used directly */
	class FunctionPhi0 {
	public:
		/*! the constructor takes the stoch vol parameters
		- theta mean reversion of variance process
		- eta volvol
		- v0 start value of variance process (should be 1.0 normally)
		and the parameters
		- T maturity of option
		- rhs right hand side, so a root finder can be used directly, cf. Piterbarg Apdx E
		*/
		FunctionPhi0(Real theta, Real eta, Real v0, Real T, Real rhs=0.0) : theta_(theta), eta_(eta), v0_(v0), T_(T), rhs_(rhs) { }
    	/*! returns the difference \f$ \phi_0(\mu) - rhs \f$ */
		Real operator()(Real mu) const {
			Real gamma = sqrt(theta_*theta_+2.0*eta_*eta_*mu);
			Real A = 2.0*theta_*v0_/(eta_*eta_)*log(2.0*gamma/((theta_+gamma)*(1.0-exp(-gamma*T_))+2.0*gamma*exp(-gamma*T_)))-
				2.0*theta_*v0_*mu/(theta_+gamma)*T_;
			Real B = 2.0*mu*(1.0-exp(-gamma*T_))/((theta_+gamma)*(1.0-exp(-gamma*T_))+2.0*gamma*exp(-gamma*T_));
			// compute A and B with ODE
			/*std::vector<Real> initialValue(2,0.0);
			FunctionPhiODEHelper2 odeFct(theta_,eta_,v0_,T_,mu);
			AdaptiveRungeKutta odeSolver(initialValue,odeFct,0.0,T_);
			std::vector<Real> odeSolution = odeSolver.solution();
			double A=odeSolution[0];
			double B=odeSolution[1];
			//*/
			return exp(A-v0_*B) - rhs_;
		}
	private:
		Real theta_,eta_,v0_,T_,rhs_;
	};

	/*! function \f$ \phi \f$, Piterbarg Appdx E */
	class FunctionPhi {
	public:
		/*! the constructor takes the stoch vol parameters
		- theta mean reversion of variance process
		- eta volvol
		- v0 start value of variance process (should be 1.0 normally)
		and the parameters
		- T maturity of option
		- sigma volatility approximation of swap rate
		*/
		FunctionPhi(Real theta, Real eta, Real v0, Real T, boost::shared_ptr<FunctionSigma> sigma) : theta_(theta), eta_(eta), v0_(v0), T_(T), sigma_(sigma) { }
		/*! returns \f$ \phi(\mu) \f$ */
		Real operator()(Real mu) const {
			//solve ODE to obtain A and B
			std::vector<Real> initialValue(2,0.0);
			FunctionPhiODEHelper odeFct(theta_,eta_,v0_,T_,mu,*sigma_);
			/*std::vector<double> points=sigma_->sigma_->piecewiseSmoothIntervals();
			Size i=0;
			bool done=false;
			double t0=0.0;
			do {
			double p=points[i];
			if(p>=T_) { done=true; p=T_; };
			AdaptiveRungeKutta odeSolver(initialValue,odeFct,t0,p);
			initialValue = odeSolver.solution();
			} while(!done);*/
			/*for(int i=0;i<=100;i++) {
			double t=i*0.1;
			printf("%f;%f\n",t,sigma_->squareValue(t));
			}*/
			AdaptiveRungeKutta odeSolver(initialValue,odeFct,0.0,T_,ODEEPS,ODEHSTART,0.0);
			initialValue = odeSolver.solution();
			double A=initialValue[0];
			double B=initialValue[1];
			return exp(A-v0_*B);
		}
	private:
		Real theta_,eta_,v0_,T_;
		boost::shared_ptr<FunctionSigma> sigma_;
	};

	// function L (Antonov, 6.2.2)
	/*class FunctionL {
	public:
	FunctionL(double c1, double c2, double k) : c1_(c1), c2_(c2), k_(k) {}
	complex<double> operator()(complex<double> xi1,complex<double> xi2) const {
	//complex<double> res=-1.0/(4.0*M_PI*M_PI)*c1_/(xi1*(xi1-1.0))*pow(k_/c1_,-(xi1-1.0))*pow(k_/c2_,-xi2)*
	//	exp( gammaLn(-xi2)+gammaLn(xi1+xi2-1.0)-gammaLn(xi1-1.0) );
	complex<double> res=1.0/(xi1*(xi1-1.0))*exp(log(k_/c1_)*(-(xi1-1.0)))*exp(log(k_/c2_)*(-xi2))*
	exp( gammaLn(-xi2)+gammaLn(xi1+xi2-1.0)-gammaLn(xi1-1.0) );
	return res;
	}
	private:
	double c1_, c2_, k_;
	};*/

	// helper function inner integrand (Antonov, 6.2.2)
	/*class FunctionSpreadInnerH : public FunctionC {
	public:

	FunctionSpreadInnerH(CharFctHeston& cFct, FunctionL& lFct) : cFct_(cFct), lFct_(lFct) {}

	complex<double> operator()(double x) {
	complex<double> xi2(-1.0,x);
	complex<double> res=cFct_(*xi1_,xi2)*lFct_(*xi1_,xi2);
	//cout << "eval inner fct at " << xi2 << " = " << res << endl;
	return res;
	}

	boost::shared_ptr<complex<double>> xi1_;

	private:
	CharFctHeston& cFct_;
	FunctionL& lFct_;
	};*/

	// helper function inner integral (Antonov, 6.2.2)
	/*class FunctionSpreadInnerI : public FunctionC {
	public:
	FunctionSpreadInnerI(CharFctHeston& cFct, FunctionL& lFct) : hFct_(cFct,lFct), simpson_(INTEGRALACCURACY,100) {}
	complex<double> operator()(double x) {
	boost::shared_ptr<complex<double>> xi1(new complex<double>(3.0,x));
	hFct_.xi1_ = xi1;				
	complex<double> res=simpson_(hFct_,-INTEGRATIONBOUND,INTEGRATIONBOUND);
	cout << "eval inner integral at " << (*xi1) << " = " << res << endl;
	return res;
	}

	private:
	FunctionSpreadInnerH hFct_;
	ComplexSimpsonIntegral simpson_;
	};*/
	
	/*class CmssoFunction {
	public:
		CmssoFunction(const double swapRate1,const double swapRate2,const double adjustedSwapRate1,const double adjustedSwapRate2,
			const double vol1,const double vol2,const double rho,const double strike,const double t,
			const double mult1,const double mult2, const int flavor);
		double operator()(const double& x) const;
	private:
		double swapRate1_,swapRate2_;
		double adjustedSwapRate1_,adjustedSwapRate2_;
		double vol1_,vol2_;
		double rho_;
		double strike_;
		double t_;
		double mult1_,mult2_;
		int flavor_;
	};*/

	/*! This is the Branch Cut corrected Laplace Transform from the Lutz paper */
	class BranchCutCorrLaplace {
	public:
		/*! The class is constructed with the stoch vol parameters
		- theta mean reversion
		- eta volvol
		- t maturity */
		BranchCutCorrLaplace(double theta, double eta, double t) : theta_(theta), eta_(eta), t_(t) {}

		/*! returns the exponent from the laplace transform */
		complex<double> exponent(complex<double> mu) {
			complex<double> gamma = sqrt(theta_*theta_+2.0*mu*eta_*eta_);
			//QL_ENSURE(gamma.imag()>=0.0 && gamma.real()>=0.0,"gamma " << gamma << " has negative re or im, theta=" << theta_ << " eta=" << eta_ << " mu=" << mu);
			double n = floor((std::arg(gamma)-t_/2.0*abs(gamma)*sin(std::arg(gamma))+M_PI)/(2.0*M_PI));
			complex<double> A = log(abs(2.0*gamma*exp(-gamma*t_/2.0)))+complex<double>(0.0,std::arg(2.0*gamma*exp(-gamma*t_/2.0))+2.0*M_PI*n)
				-log(gamma+theta_+(gamma-theta_)*exp(-gamma*t_));
			complex<double> B = theta_*theta_*t_/(eta_*eta_)+(2.0*gamma*exp(-gamma*t_)/(gamma+theta_+(gamma-theta_)*exp(-gamma*t_))-1.0)*(gamma-theta_)/(eta_*eta_);
			complex<double> res = 2.0*theta_/(eta_*eta_)*A + B;
			return res;
		}

		/*! returns the laplace transform itself at \f$ \mu \f$ */
		complex<double> value(complex<double> mu) {
			return exp(exponent(mu));
		}

		/*! returns the function \f$ h(u,x) \f$ (Lutz paper, p9) */
		double h(double u, double x) {
			double res=x*u+exponent(complex<double>(0.0,u)).imag()-M_PI/2.0;				
			//cout << "   h(" << u << "," << x << ") = " << res << endl;
			return res;
		}

		/*! returns the zero of h (Lutz paper, p9) */
		double zeroH(double x) {
			Real root_=1000.0; // start value
			Real froot, dfroot, dx;
			Size evaluationNumber_=0;
			bool found=false;

			/*Real u=1E-6;
			printf("************************\n");
			for(int i=0;i<1000;i++) {
			printf("%1.12f;%1.12f\n",u,h(u,x));
			u+=1E-1;
			}
			printf("************************\n");
			*/

			froot = h(root_,x);
			dfroot = (h(root_+HSTEP,x)-h(root_,x))/HSTEP;
			evaluationNumber_++;
			while (!found && evaluationNumber_<=MAXEVALNEWTON) {
				dx=froot/dfroot;
				root_ -= dx;
				if(root_<0.0) root_=HSTEP; // root can not be < 0
				if (std::fabs(dx) < HACC) {
					found=true;
				}
				if(!found) {
					froot = h(root_,x);
					dfroot = (h(root_+HSTEP,x)-h(root_,x))/HSTEP;
					evaluationNumber_++;
				}
			}
			if(!found) {
				printf("zeroH did not find a zero with requested accuracy for x=%f. Returning last value h(%1.12f)=%1.12f\n",x,root_,h(root_,x)); 					
			}//QL_FAIL("zeroH failrd: maximum number of function evaluations (" << MAXEVALNEWTON << ") exceeded for x=" << x);
			return root_;
		}

	private:
		double theta_, eta_, t_;
	};
	
	/*! this class represents the integrand in (13) of lutz paper for computation of density */
	class LutzDensityIntegrand {
	public:
		/*! the constructor needs parameters
		- x the value at wich the density should be evaluated
		- a, b the parameters defining the contour over which the integration is done
		- laplace the branch corrected laplace transform */
		LutzDensityIntegrand(double x, double a, double b, BranchCutCorrLaplace* laplace): x_(x), a_(a), b_(b), laplace_(laplace) { /*printf("Integral Density: x=%f, a=%f, b=%f\n",x,a,b);*/ }
		/*! return the integrand at u */
		double operator()(double u) {
			complex<double> sTilde = a_-3.0*log(u)*complex<double>(-a_,b_);
			complex<double> sTildePrime = complex<double>(3.0*a_/u,-3.0*b_/u);
			double res = (exp(x_*sTilde)*laplace_->value(sTilde)*sTildePrime).imag();
			//printf("Integrand;%f;%f\n",u,res);
			return -res;
		}

	private:
		double a_,b_,x_;
		BranchCutCorrLaplace* laplace_;

	};
	
	/*! This is the integrand from lutz paper, (22) w.r.t. v, i.e. for calculating the outer integral */
	class LutzVIntegrand {
	public:
		/*! the integrand needs as parameters
		- laplace the branch cut corrected laplace transform
		- mu1 drift for swap rate 1 as in lutz, (20)
		- mu2 drift for swap rate 2 as in lutz, (20)
		- rho correlation as in lutz, p. 12
		- k adjusted strike as in lutz, 4.2
		- sR1 forward swap rate 1
		- sR2 forward swap rate 2
		- avSkewS1 averaged skew for swap rate 1
		- avSkewS2 averaged skew for swap rate 2
		- wd flavour (1 = call, -1 = put)
		- skVolS1 av. skew times av. volatility for swap rate 1
		- skVolS2 av. skew times av. volatility for swap rate 2
		- hermiteRoots roots for hermite integration (inner integral from lutz, (22))
		- hermiteWeichts weights for hermite integration (inner integral from lutz, (22))
		*/
		LutzVIntegrand(BranchCutCorrLaplace* laplace, double mu1, double mu2, double rho, 
			double k, double sR1, double avSkewS1, double sR2, double avSkewS2, double wd,
			double skVolS1, double skVolS2, Array* hermiteRoots, Array* hermiteWeights) :
		laplace_(laplace), mu1_(mu1), mu2_(mu2), rho_(rho), k_(k), sR1_(sR1), avSkewS1_(avSkewS1), sR2_(sR2), avSkewS2_(avSkewS2),
			wd_(wd), skVolS1_(skVolS1), skVolS2_(skVolS2), hermiteRoots_(hermiteRoots), hermiteWeights_(hermiteWeights) { } 
		/*! returns the integrand at v (integrated variance) */
		double operator()(double v) {
			// calcualte inner integral
			CumulativeNormalDistribution cnd(0.0,1.0);
			double sum=0.0;
			double mux=(mu1_-0.5*skVolS1_*skVolS1_)*v;
			double muy=(mu2_-0.5*skVolS2_*skVolS2_)*v;
			double vx=skVolS1_*sqrt(v);
			double vy=skVolS2_*sqrt(v);
			double sqrt2=sqrt(2.0);
			double z,u,h,g;
			for(Size i=0;i<HERMITEPOINTS;i++) {
				z=(*hermiteRoots_)[i];
				u=sqrt2*z;
				//here something is wrong.... ?!?!? - so I use the lower alternative formulation
				//h=(k_+sR2_/avSkewS2_*exp(vy*u+muy))/(sR1_/avSkewS1_);
				//g = wd_*sR1_/avSkewS1_*exp(mux+rho_*vx*u+0.5*vx*vx*(1.0-rho_*rho_))*
				//	   cnd(wd_*(mux+rho_*vx*u-log(h)+vx*vx*(1.0-rho_*rho_))/(vx*sqrt(1.0-rho_*rho_)))-
				//	   wd_*(k_+sR2_/avSkewS2_*exp(vy*u+muy))*cnd(wd_*(mux+rho_*vx*u-log(h))/(vx*sqrt(1.0-rho_*rho_)));
				//sum += exp(-z*z)*g*(*hermiteWeights_)[i];
				double h_=k_+sR1_/avSkewS1_*exp((mu1_-0.5*skVolS1_*skVolS1_)*v+skVolS1_*sqrt(v)*u);
				double phi1_=cnd(wd_*(log(sR2_/avSkewS2_/h_)+(mu2_+(0.5-rho_*rho_)*skVolS2_*skVolS2_)*v+rho_*skVolS2_*sqrt(v)*u)/
					(skVolS2_*sqrt(v*(1.0-rho_*rho_))));
				double phi2_=cnd(wd_*(log(sR2_/avSkewS2_/h_)+(mu2_-0.5*skVolS2_*skVolS2_)*v+rho_*skVolS2_*sqrt(v)*u)/
					(skVolS2_*sqrt(v*(1.0-rho_*rho_))));
				double f=wd_*sR2_/avSkewS2_*exp(mu2_*v-0.5*rho_*rho_*skVolS2_*skVolS2_*v+rho_*skVolS2_*sqrt(v)*u)*phi1_-wd_*h_*phi2_;
				sum += exp(-z*z)*f*(*hermiteWeights_)[i];
			}
			//calculate density f(v)
			//calculate contour parameters
			double a=1.0/v;
			double b=laplace_->zeroH(v);
			//calculate integral (14) from lutz
			GaussLobattoIntegral gl(1000,DENSITYACCURACY);
			LutzDensityIntegrand ldi(v,a,b,laplace_);
			double dens=gl(ldi,DENSITYLOWERINTEGRALBOUND,1.0);
			//printf("%f;%f;%f\n",v,sum/sqrt(M_PI),dens/M_PI);
			return sum*dens;
		}
	private:
		double mu1_, mu2_, rho_, k_, sR1_, sR2_, avSkewS1_, avSkewS2_, wd_, skVolS1_, skVolS2_;
		BranchCutCorrLaplace* laplace_;
		//boost::shared_ptr<GaussHermiteIntegration> hermite_; // for computing the inner cmsso integral (Lutz)
		Array* hermiteRoots_;
		Array* hermiteWeights_;
	};


}

#endif


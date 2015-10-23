/*! \file effectiveParametersHelper.hpp
\brief Helper classes for the effectiveParametersSVLfm main class
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
#include <gammaFunction.hpp>

#define LAMBDAACCURACY 1.0E-5 // accuracy for the effective volatility brent search (Piterbarg, 6.23)
#define ODEEPS 1.0E-4 // epsilon for ODE
#define ODEHSTART 0.01 // start step size for ODE
#define SKEWINTEGRALPOINTS 32 // number of points for gauss legendre integration to compute effective skew integral 
#define NUMDIFFH 1.0E-4 // h for numerical differentation Swap Rates by libors

#ifndef quantlib_effectiveParametersHelper_hpp
#define quantlib_effectiveParametersHelper_hpp

using namespace boost;
using namespace std;

namespace QuantLib {

	/*! Return the covariance of two swap rate volatility functions from a to b.
	The swap rate volatility functions are built as in Piterbarg, (5.2) with
	- sigma the libor volatility parametrization
	- q1, q2 the weights from (5.2)
	- n, m1, m2 the indices from (5.2)
	*/

	class SigmaCovariance {
	public:
		SigmaCovariance(boost::shared_ptr<AbcdVolRatioCorrSigma> sigma, boost::shared_ptr<std::vector<Real>> q1, boost::shared_ptr<std::vector<Real>> q2,
			Size n, Size m1, Size m2):sigma_(sigma), q1_(q1), q2_(q2), n_(n), m1_(m1), m2_(m2) {}
		Real covariance(double a, double b) const {
			Real result=0.0;
			for(int k=0;k<(*sigma_).numberOfFactors_;k++) {
				for(int i=n_;i<=m1_-1;i++) {
					for(int j=n_;j<=m2_-1;j++) {
						result+=(*q1_)[i-n_]*(*q2_)[j-n_]*(*sigma_).integrate(a,b,i,j,k);
					}
				}
			}
			return result;
		}

	private:
		boost::shared_ptr<AbcdVolRatioCorrSigma> sigma_;
		boost::shared_ptr<std::vector<Real>> q1_,q2_;
		Size n_,m1_,m2_;
	};

	/*! This class represents the swap rate volatility as in Piterbarg, (5.2)*/
	class FunctionSigma {
	public:
		/*! the class is constructed by
		- sigma	the libor volatility parametrization
		- q the weights vector from Piterbarg, (5.2)
		- n,m the indices from Piterbarg, (5.2) */
		FunctionSigma(boost::shared_ptr<AbcdVolRatioCorrSigma> sigma, boost::shared_ptr<std::vector<Real>> q, Size n, Size m) : sigma_(sigma),q_(q),n_(n),m_(m) {	}
		/*! return the swap rate volatility at t for factor k (Piterbarg, (5.2)) */
		Real operator()(Real t, Size k) const {
			Real result=0.0;
			for(int i=n_;i<=m_-1;i++) {
				result+=(*q_)[i-n_] * (*sigma_)(t,i,k);
			}
			return result;
		}
		/*! return \f$ \sum_k \sigma_k^2 \f$ at time t,
		where \f$ \sigma_k \f$ is the swap rates volatility function at t for factor k */
		Real squareValue(Real t) const {
			Real result=0.0;
			for(int k=0;k<(*sigma_).numberOfFactors_;k++) {
				for(int i=n_;i<=m_-1;i++) {
					for(int j=n_;j<=i;j++) {
						result+=(*q_)[i-n_]*(*q_)[j-n_]*(*sigma_)(t,i,k)*(*sigma_)(t,j,k)*( j<i ? 2.0 : 1.0 );
					}
				}
			}
			return result;
		}
		/*! return \f$ \int_a^b \sum_k \sigma_k^2(s) e^{-\theta s} ds \f$
		where \f$ \sigma_k(s) \f$ is the swap rates volatility function at s for factor k */
		Real squareIntegrate(Real a, Real b,Real theta=0.0) const {
			Real result=0.0;
			for(int k=0;k<(*sigma_).numberOfFactors_;k++) {
				for(int i=n_;i<=m_-1;i++) {
					for(int j=n_;j<=i;j++) {
						result+=(*q_)[i-n_]*(*q_)[j-n_]*(*sigma_).integrate(a,b,i,j,k,theta)*( j<i ? 2.0 : 1.0 );
					}
				}
			}
			return result;
		}
		int numberOfFactors() const {
			return sigma_->numberOfFactors();
		}

	private:
		boost::shared_ptr<AbcdVolRatioCorrSigma> sigma_;
		boost::shared_ptr<std::vector<Real>> q_;
		Size n_,m_;
	};

	/*! This class represents the function \f$ p_i(n,m) (t) \f$ as in Piterbarg, (5.5) */ 
	class FunctionP {
	public:
		/*! the constructor needs
		- sigma the libor volatility parametrization
		- sigmaNm the swap rates volatility approximation (which can be built with help of the class FunctionSigma */
		FunctionP(boost::shared_ptr<AbcdVolRatioCorrSigma> sigma, boost::shared_ptr<FunctionSigma> sigmaNm) : sigma_(sigma), sigmaNm_(sigmaNm) { }
		/*! return the function \f$ p_i(n,m) (t) \f$ at t for libor i */
		Real operator()(Real t, Size i) const {
			Real nom=0.0,denom=0.0;
			for(int k=0;k<(*sigma_).numberOfFactors_;k++) {
				double sigNm = (*sigmaNm_)(t,k);
				nom += (*sigma_)(t,i,k) * sigNm;
				denom += sigNm*sigNm;
			}
			return nom/denom;
		}
	private:
		boost::shared_ptr<AbcdVolRatioCorrSigma> sigma_;
		boost::shared_ptr<FunctionSigma> sigmaNm_;
	};

	/*! This class represents the function \f$ \beta(t,n,m) \f$ as in Piterbarg, (5.5) */
	class FunctionBeta {
	public:
		/*! the constructor takes as input
		- beta the beta parametrization of the libors
		- p the p function as in Piterbarg, (5.3) which can be set up by class FunctionP
		- n, m the indices belonging to the swap rate as in Piterbarg, (5.3) */
		FunctionBeta(boost::shared_ptr<PiecewiseConstantBeta> beta, boost::shared_ptr<FunctionP> p,Size n, Size m) : beta_(beta), p_(p), n_(n), m_(m) { }
		/*! return \f$ beta(t) \f$, i.e. the function as in Piterbarg, (5.3) at t */
		Real operator()(Real t) const {
			Real result=0.0;
			for(int i=n_;i<=m_-1;i++) {
				result+=(*p_)(t,i)*(*beta_)(t,i);
				//printf(" compute: i=%d, t=%f, p=%f, beta=%f\n",i,t,(*p_)(t,i),(*beta_)(t,i));
			}
			return result/((double)(m_-n_)); // error in paper: factor 1/(n-m+1) is not there
		}
	private:
		boost::shared_ptr<PiecewiseConstantBeta> beta_;
		boost::shared_ptr<FunctionP> p_;
		Size n_,m_;
	};

	/*! This class represents the product function \f$ v^2(t) s^2(t) \f$ as in Piterbarg, (6.14) */
	class FunctionVsSs {
	public:
		/*! The class is constructed with
		- sigmaNm the volatility function of the swap rate as in Piterbarg, (5.2)
		- v0 stoch vol parameter: start value for the variance process
		- eta stoch vol parameter: volatility of volatility
		- theta stoch vol parameter: mean reversion */
		FunctionVsSs(boost::shared_ptr<FunctionSigma> sigmaNm, Real v0, Real eta, Real theta) : sigmaNm_(sigmaNm), v0_(v0), eta_(eta), theta_(theta) { }
		/*! return the function \f$ v^2(t) s^2(t) \f$ as in Piterbarg, (6.14) at t */
		Real operator()(Real t) const {
			Real sNm = sigmaNm_->squareValue(t);
			Real vs= v0_*v0_*sigmaNm_->squareIntegrate(0.0,t)+v0_*eta_*eta_*exp(-theta_*t)/(2.0*theta_)*(
				sigmaNm_->squareIntegrate(0.0,t,theta_)-sigmaNm_->squareIntegrate(0.0,t,-theta_));
			//printf("VsSs(%f)=%f\n",t,sNm*vs);
			return sNm*vs;
		}
	private:
		boost::shared_ptr<FunctionSigma> sigmaNm_;
		Real v0_,eta_,theta_;
	};




}

#endif


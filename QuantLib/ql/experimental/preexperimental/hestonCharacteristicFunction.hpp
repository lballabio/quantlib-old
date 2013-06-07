/*! \file hestonCharacteristicFunction.hpp
    \brief characteristic function for two correlated heston processes
	Peter Caspers */

#include <ql/quantlib.hpp>
#include <complex>
#include <complexRungeKutta.hpp>
#include <effectiveParametersHelper.hpp>

#ifndef quantlib_hestonCharacteristicFunction_hpp
#define quantlib_hestonCharacteristicFunction_hpp

#define ODEEPS2 1.0E-4 // epsilon for ODE to compute characteristic function value

using namespace boost;
using namespace std;

namespace QuantLib {
	
	/*! This helper class defines the ODE, cf. Antonov Apdx. B */
	/*template<class S, class R>*/ class CharFctHestonODEHelper : public FunctionRnC {
		public:
			/*! Constructor takes the following arguments
			- sigmaS and sigmaR are vol functions (double t, Size k)
			- avSkewS and avSkewR are averaged (effective), i.e. constant skew parameters
			- xiS, xiR are the arguments of the characteristic function to compute
			- theta, eta: stoch vol mean reversion and volvol
			- T is the maturity */
			CharFctHestonODEHelper(FunctionSigma& sigmaS, FunctionSigma& sigmaR, double avSkewS, double avSkewR, complex<double> xiS, complex<double> xiR,
								   double theta, double eta, double T) :
			  sigmaS_(sigmaS), sigmaR_(sigmaR), avSkewS_(avSkewS), avSkewR_(avSkewR), xiS_(xiS), xiR_(xiR),
			  theta_(theta), eta_(eta), T_(T) { }

			  /*! return ODE defining value at t, y */			
			  std::vector<complex<double>> operator()(Real t, std::vector<complex<double>> y) const {
				Size K = sigmaS_.numberOfFactors(); // R must have the same number of factors as S
				std::vector<complex<double>> res(2,0.0);
				// compute vola terms
				double sSq=0.0, rSq=0.0, rSMx=0.0;
				for(Size k=0;k<K;k++) {
					double sTmp=sigmaS_(T_-t,k);
					double rTmp=sigmaR_(T_-t,k);
					sSq+=sTmp*sTmp;
					rSq+=rTmp*rTmp;
					rSMx+=sTmp*rTmp;
				}
				//printf("vol R = %f\n",sqrt(rSq));
				// define ODE function
				res[0] = theta_*y[1];
				res[1] = 0.5*y[1]*y[1]*eta_*eta_-y[1]*theta_+0.5*(sSq*(avSkewS_*avSkewS_*(xiS_*xiS_-xiS_))+
					                                         rSq*(avSkewR_*avSkewR_*(xiR_*xiR_-xiR_))+
															 2.0*xiS_*xiR_*avSkewS_*avSkewR_*rSMx);
				return res;
			}
		private:
			Real theta_,eta_,avSkewS_,avSkewR_,T_;
			complex<double> xiS_, xiR_;
			FunctionSigma& sigmaS_;
			FunctionSigma& sigmaR_;
	};

	/*! computes characteristic functions of two correlated heston processes (cf. Antonov, Apdx. B)
	
	dS = ( b_S S(t) + (1-b_S) S(0) ) sqrt(z(t)) sum_k sigma_S_k(t) dW_k
	dR = ( b_R R(t) + (1-b_R) R(0) ) sqrt(z(t)) sum_k sigma_R_k(t) dW_k
	dz = theta ( 1 - z(t)) dt + eta sqrt(z(t)) dV
	dV dW_k = 0

	define

	X_q = b_q S(t) + (1-b_q) S(0)
	y_q = ln ( X_q(t) / X_q(0) )

	for q=S,R. Then (dropping subscript q)

	dy = b sqrt(z(t)) sum_k sigma_k(t) dW_k - 0.5 b^2 z(t) sum_k sigma_k(t)^2 dt

	Here we compute

	Phi(t,xi_S,xi_R) = E( exp( xi_S*y_S(t) + xi_R*y_R(t) ) )

	sigma_S_k resp. sigma_R_k must be given by classes S, R with methods
		operator()(double t, Size k)
		numberOfFactors()
	*/



	/*template<class S, class R>*/ class CharFctHeston {

		public:
			/*! Constructor needs parameters
			- sigmaS and sigmaR are vol functions (double t, Size k)
			- avSkewS and avSkewR are averaged (effective), i.e. constant skew parameters
			- theta, eta: stoch vol mean reversion and volvol */
			CharFctHeston(FunctionSigma& sigmaS, FunctionSigma& sigmaR, double avSkewS, double avSkewR, double theta, double eta, double T) :
			  sigmaS_(sigmaS), sigmaR_(sigmaR), avSkewS_(avSkewS), avSkewR_(avSkewR), theta_(theta), eta_(eta), T_(T)
			  {}

			/*! compute characteristic function \f$ \Phi(T, xiS, xiR) \f$, see class description */
		    complex<double> operator()(complex<double> xiS, complex<double> xiR);

		private:
			FunctionSigma& sigmaS_;
			FunctionSigma& sigmaR_;
			double avSkewS_, avSkewR_, theta_, eta_, T_;

	};

	
}

#endif


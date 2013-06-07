/*! \file sigmaParametrization.hpp
    \brief parametrization for libor volatility in the libor market model
	Peter Caspers */

#include <ql/quantlib.hpp>

#ifndef quantlib_sigmaParametrization_hpp
#define quantlib_sigmaParametrization_hpp

#define TINYBETA 1.0E-10 // if abs(beta) < this number then exp(beta*t) is replaced by 1.0 in computation of primitive function

namespace QuantLib {
	
	/*! sigma parametrization as proposed in schoenmakers 2.2.3, 2.12 */
	
	class AbcdVolRatioCorrSigma {
		public:
			/*! the constructor takes as input
			- rateTimes grid times
			- mode different ways of parameterization as described below are available and encoded by this number
			- parameters generic parameters belonging to the mode w.r.t. which the parametrization is done
			- factor number of factors to use, 0 means use no factor reduction
			The following modes are available and the following generic parameters must be given to each of them (\f$ \phi_i=1 \f$ when not mentioned in the following)
			- mode=0	(a,b,c,d, \f$ \Delta_i \f$ (i=2,...,N)) abcd functional and \f$ \Delta_i \f$ according to Schoenmakers, Theorem 2.1
			- mode=1	(a,b,c,d, \f$ \beta \f$) specialisation of mode 0 with \f$ \Delta_i=0, \Delta_n=\beta \f$, Schoenmakers, Example 2.2 with \f$ \alpha=0 \f$
			- mode=2	(a,b,c,d, \f$ \alpha_1, \alpha_2, \beta \f$) is Schoenmakers, Example 2.3, linear decreasing Deltas, last Delta = beta
			- mode=3	(a,b,c,d)	only abcd functional (if only this shall be calibrated). correlation must be set by another mode, than you can switch to this mode by changeMode method.
			- mode=4	(a,b,c,d, \f$ \beta \, d_{\infty}, d_0, \lambda \f$) is Schoenmakers example 2.2 and \f$ \phi_i = d_0 + (d_{\infty}-d_0) e^{-\lambda i} \f$
			- mode=5	(a,b,c,d, \f$ d_{\infty}, d_0, \lambda \f$) same as mode=4, but correlation must be set by another mode
			- mode=6	(a,b,c,d, \f$ d_0, \lambda\f$) same as mode=5 with \f$ d_{\infty}=1 \f$
			- mode=7	(a,b,c,d, a',b',c',d', lambda) two state model w.r.t. lambda 
			*/
			AbcdVolRatioCorrSigma(std::vector<Time>& rateTimes,
				int mode, 
				std::vector<Real>& parameters, // generic parameters
				Size factors=0 // number of factors to use (if 0, full factors are used)
				) : numberOfFactors_(factors > 0 ? factors : rateTimes.size()-1), rateTimes_(rateTimes), 
					N_(rateTimes.size()-1) {

				// intitialize beta and phi and second state abcd
				beta_=std::vector<Real>(N_,0.0);
				phi_=std::vector<Real>(N_,1.0);
				a2_=0.0; b2_=0.0; c2_=0.0; d2_=0.0;

				changeMode(mode,parameters);

				// prepare variables for precomputing integral				
				integral0_ = std::vector<Real>(N_*N_*numberOfFactors_*N_,0.0);
				integraltheta_ = std::vector<Real>(N_*N_*numberOfFactors_*N_,0.0);
				integralminustheta_ = std::vector<Real>(N_*N_*numberOfFactors_*N_,0.0);
				integralthetavalue_=-1000.0; 
				integralprecomputed_=false;

				// do factor reduction
				factorReducedEta_ = Matrix(N_,factors);
				factorReduction();
			}
			/*! sigma for libor i and factor k at time t is returned*/
			Real operator()(Real t, Size i, Size k) const; 
			/*! average sigma for rateTimes[m] to rateTimes[m+1] is returned. m may be -1. correlation is taken at index m+1 (because sigma function is not RCLL here) */
			Real valueAtIndex(int m, Size i, Size k) const; // 
			/*! integrate sigma_k(t,i)*sigma_k(t,j)*exp(-theta*t) dt from t0 to t1 */
			Real integrate(Real t0, Real t1, Size i, Size j, Size k,Real theta=0.0) const; 
			/*! precomputation for integrate()-method w.r.t. given theta (must be equal to all thetas != 0.0 in invokation of integrate(), otherwise precompute must be done again!) */
			void precomputeIntegral(Real theta);
			/*! integrate from rateTimes[index-1] (or zero) to rateTimes[index] */
			Real integrateOnePeriod(int index, Size i, Size j, Size k, Real theta) const; 
			/*! return correlation between libor i and j at time t using no factor reduction */
			Real fullFactorCorrelation(Real t, Size i, Size j) const ; 
			/*! return correlation between libor i and j at time t using factor reduced correlation structure */
			Real correlation(Real t, Size i, Size j) const; 
			/*! return factor reduced correlation matrix */
			const Matrix& factorReducedCorrelationMatrix() const;
			/*! recalibrate parametrization with generic parameters suitable for mode */
			void recalibrate(std::vector<double>& parameters); 
			/*! returns 1.0 if no penalty is assigned to parameters, otherwise > 1.0 */
			Real penaltyFactor(); 
			/*! returns number of generic parameters w.r.t. mode */
			Size numberOfParameters() { return P_; }
			/*! return mode of parametrization */
			int mode() { return mode_; }
			/*! return generic parameters */
			std::vector<double>& parameters() { return parameters_; }
			/*! change mode of parametrization */
			void changeMode(int mode, std::vector<double>& parameters);
			/*! return number of factors used for factor reduction of correlation matrix */
			Size numberOfFactors_; // the number of factors to use
			/*! return number of factors used for factor reduction of correlation matrix */
			Size numberOfFactors() { return numberOfFactors_; };

		private:

			void setParameters(); // sets the parameters a,b,c,d and beta according to parameters_ and mode
			void factorReduction(); // do factor reduction (is done automatically after initialization and recalibration)

			Real beta(Real t, Size k) const; // returns beta(T_k-t) (correlation generating beta)

			Real fullFactorEta(Real t, Size i, Size k) const; // return eta_i,k(t) (full factor)
			Real eta(Real t, Size i, Size k) const; // return eta_i,k(t) (factor reduced)

			Size nextResetIndex(Real t) const; // returns the index l = min(m:T(m)>=t)

			Real abcd(Real t, bool secondState=false) const ; // returns the abcd functional form at t (for first or second state)
			Real abcdIntegrate(double t0, double t1,Size i, Size j, double theta) const; // integrates exp(theta*t)*abcd(T-t)*abcd(S-t) from t0 to t1 for mixed state function abcd
			Real primitive(Real t, Real T, Real S, Real theta, bool TsecondState=false, bool SsecondState=false) const; // primitive function of exp(theta*t)*abcd(T-t)*abcd(S-t) for first or second state for T-abcd or S-abcd respectively
			
			std::vector<Time> rateTimes_;
			Size N_; // last index of rateTimes_ vector
			
			Size P_; // number of generic parameters determining beta and a,b,c,d
			int mode_; // mode of parameters
			std::vector<double> parameters_;
			std::vector<double> beta_;
			Real a_,b_,c_,d_;
			std::vector<double> phi_;
			Real a2_,b2_,c2_,d2_,twoStateLambda_; // for two state model
			bool twoState_;

			double integralthetavalue_;
			bool integralprecomputed_;
			std::vector<Real> integral0_, integraltheta_, integralminustheta_;

			Matrix factorReducedEta_;
	};


	
}

#endif


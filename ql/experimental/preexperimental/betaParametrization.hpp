/*! \file betaParametrization.hpp
\brief Parametrization of skew for libor market model

This class represents different parametrizations of libor specific skews for the libor market model.
The different possible parametrizations are chosen by the parameter mode.
All parametrizations are piecewise constant w.r.t. the libor grid.
Peter Caspers
*/

#include <ql/quantlib.hpp>

#ifndef quantlib_betaParametrization_hpp
#define quantlib_betaParametrization_hpp

namespace QuantLib {

	/*! Class representing some libor specific, piecewise constant parametrizations for the skew */
	class PiecewiseConstantBeta {
	public:
		/*! The constructor takes as inputs
		- rateTimes		the libor grid points
		- mode			the parametrization mode, see below which are available
		- parameters	generic parameters describing the parameterization, see below for their meaning for each mode
		The following values for mode are possible and the following parameters must be given:
		- mode=0: N (N=number of libors) parameters must be given. The first parameter is the skew for the next libor not yet expired,
		the second one for the libor after that and so on.
		- mode=1: 2 Parameters \f$ (b_0, lambda) \f$ must be given. The skew for the ith not expired libor (i starting with 0) is
		\f$ 1.0 - b_0 e^{-\lambda i}\f$
		*/
		PiecewiseConstantBeta(std::vector<Time>& rateTimes,
			int mode, 
			std::vector<Real>& parameters
			) : rateTimes_(rateTimes), N_(rateTimes.size()-1), parameters_(parameters), mode_(mode) {

				// set number of parameters
				switch(mode_) {
					case 0: P_=N_;
						break;
					case 1: P_=2;
						break;
					default: QL_FAIL("mode " << mode_ << " not supported.");
				}

				// initialize beta
				beta_=std::vector<double>(N_,0.0);
				setParameters();
		}

		/*! return the skew at time t for libor i */
		Real operator()(Real t, Size i) const;
		/*! return the skew at index m for libor i. m may be -1. beta is evaluated at m+1 (beta is not RCLL here). */
		Real valueAtIndex(int m, Size i) const;
		/*! recalibrate the parametrization w.r.t. the given parameters */
		void recalibrate(std::vector<double>& parameters);
		/*! return a penalty factor for the current parametrization (1.0 = no penalty, otherwise > 1.0) */
		Real penaltyFactor();
		/*! return the number of parameters needed for the current mode */
		Size numberOfParameters() { return P_; }
		/*! return the current mode of parametrization */
		int mode() { return mode_; }
		/*! return the current parameters */
		std::vector<double>& parameters() { return parameters_; }

	private:
		void setParameters(); // sets beta according to parameters_ and mode
		Real beta(Real t, Size k) const ; // returns beta(T_k-t)
		Size nextResetIndex(Real t) const; // returns the index l = min(m:T(m)>=t)

		std::vector<Time> rateTimes_; // libor time grid
		std::vector<double> beta_; // internal piecewise constant beta
		std::vector<double> parameters_; // generic parameters describing the parametrization depending on mode
		Size N_; // last index of rateTimes_ vector

		int mode_; // mode of parametrization
		Size P_; // number of generic parameters
	};



}

#endif


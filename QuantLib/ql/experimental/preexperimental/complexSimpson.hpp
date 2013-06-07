/*! \file complexSimpson.hpp
\brief Simpon integration for complex valued functions
This is a modification of the original quantlib class to handle complex valued integrands
Peter Caspers
*/

#include <ql/quantlib.hpp>
#include <complex>

#ifndef quantlib_complexSimpson_hpp
#define quantlib_complexSimpson_hpp

using namespace boost;
using namespace std;

namespace QuantLib {

	/*! function \f$ R \rightarrow C \f$
	represents the integrand */
	class FunctionC {
	public:
		FunctionC() {}
		virtual complex<double> operator()(double x) { return 0.0; }
	};

	/*! Complex Simpson Integral class */
	class ComplexSimpsonIntegral {

	public:
		/*! The class constructor takes the following inputs
		- accuracy	required accuracy
		- maxIterations	maximum number of interations
		*/
		ComplexSimpsonIntegral(Real accuracy, Size maxIterations):accuracy_(accuracy), maxIterations_(maxIterations) {}
		/*! integrate the given function f from given bounds a to b */
		complex<double> operator()(FunctionC& f, Real a, Real b) const;

	private:

		double accuracy_;
		Size maxIterations_;

	};

}

#endif


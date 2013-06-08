/*! \file complexErf.hpp
\brief Some stand alone utility functinos
Peter Caspers
*/

#ifndef quantlib_complexErf_hpp
#define quantlib_complexErf_hpp

#include <ql/quantlib.hpp>
#include <complex>
#include <math.h>

using namespace std;

namespace QuantLib {

	complex<double> erfc(complex<double> z);
		

}

#endif

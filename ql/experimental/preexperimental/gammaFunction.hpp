/*! \file gammaFunction.hpp
    \brief Complex Gamma function
	Gamma function for complex arguments as described in "Numerical Recipes in C", Chapter 6.1
	Peter Caspers */


#ifndef quantlib_gammaFunction_hpp
#define quantlib_gammaFunction_hpp

#include <ql/quantlib.hpp>
#include <vector>
#include <math.h>
#include <stdio.h>
#include <complex>

using namespace std;

namespace QuantLib {
	/*! return the log of gamma function */
	complex<double> gammaLn(complex<double> z);
	double gammaLn(double x);
}

#endif

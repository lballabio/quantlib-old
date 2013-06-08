#include <gammaFunction.hpp>

using namespace std;

namespace QuantLib {

	complex<double> gammaLn(complex<double> z) {
		
		if(z.real()<1.0) return log(M_PI*(1.0-z)/sin(M_PI*(1.0-z))) - gammaLn(2.0-z); // reflection formula for Re z < 1
		
		complex<double> res = log(z-1.0+5.0+0.5)*(z-0.5) - (z-1.0+5.0+0.5);
		res+= log ( sqrt(2.0*M_PI) * ( 1.000000000190015 +
									   76.18009172947146 / z + 
			                          (-86.50532032941677 / (z+1.0) ) +
									  24.01409824083091 / (z+2.0) +
									  (-1.231739572450155 / (z+3.0)) +
									  0.1208650973866179e-2 / (z+4.0) +
									  (-0.5395239384953e-5 / (z+5.0)) ) );
		return res;
	}

	double gammaLn(double x) {
		return gammaLn(complex<double>(x,0.0)).real();
	}

}


	

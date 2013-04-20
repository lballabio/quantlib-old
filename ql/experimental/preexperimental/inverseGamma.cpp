#include <inverseGamma.hpp>

using namespace std;

namespace QuantLib {

	
	double InverseGamma::density(const double x) {
		if(x<=0.0) return 0.0;
		return pow(b_,a_)*exp(-gammaLn(a_))*pow(1.0/x,a_+1.0)*exp(-b_/x);	
	}

	double InverseGamma::cumulative(const double x) {
		if(x<=0.0) return 0.0;
		double p=1.0-incompleteGammaFunction(a_,b_/x);
		//FILE *out=fopen("incomplgamma.log","a");
		//fprintf(out,"%f;%f\n",x,p);
		//fclose(out);
		return p;
	}

	double InverseGamma::cumulativeInv(const double x) {
			IGInvHelper hlp(this,x);
			Brent b;
			return b.solve(hlp,INVGINVACC,1.0,0.1);
	}

	
			
}


	

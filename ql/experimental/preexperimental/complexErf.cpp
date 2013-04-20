#include <complexErf.hpp>

using namespace std;

namespace QuantLib {

	complex<double> erfc(complex<double> z) {
		CumulativeNormalDistribution cnd;
		double x=z.real();
		double y=z.imag();
		double erfr=2.0*cnd(sqrt(2.0)*x)-1.0;	double a=erfr;
		double erfi=0.0;
		double emxs=exp(-x*x);
		if(x!=0.0) erfr+=emxs/(2.0*M_PI*x)*(1.0-cos(2.0*x*y));
		if(x!=0.0) erfi+=emxs/(2.0*M_PI*x)*sin(2.0*x*y);
			else erfi+=y/(M_PI);
		double rr=0.0,ri=0.0,nd;
		for(int n=1;n<=10;n++) {
			nd=(double)n;
			rr+=exp(-0.25*nd*nd)/(nd*nd+4.0*x*x)*(2.0*x-2.0*x*cosh(nd*y)*cos(2.0*x*y)+nd*sinh(n*y)*sin(2.0*x*y));
			ri+=exp(-0.25*nd*nd)/(nd*nd+4.0*x*x)*(2.0*x*cosh(nd*y)*sin(2.0*x*y)+nd*sinh(nd*y)*cos(2.0*x*y));
		}
		rr*=2.0/M_PI*emxs;
		ri*=2.0/M_PI*emxs;
		/*FILE *out=fopen("erf.log","a");
		fprintf(out,"%f;%f => %f;%f;%f\n",x,y,erfr+rr,erfi+ri,a);
		fclose(out);*/
		return complex<double>(erfr+rr,erfi+ri);
	}

}


#include <spreadOptionPricer.hpp>
#include <stdio.h>

namespace QuantLib {

	SpreadOptionPricer::SpreadOptionPricer(int hermitePoints) {
			CMSSOHERMITEPOINTS=hermitePoints;
			initHermiteIntegrator();
	}

	void SpreadOptionPricer::initHermiteIntegrator() {
		ghInt_ = new GaussHermiteIntegration(CMSSOHERMITEPOINTS);
	}

	
	Real SpreadOptionPricer::npv(double s1, double s2, double k, double t, double vol1, double vol2, double rho, double a, double b, int w) {
		double in=(*ghInt_)(CmssoFunction2(s2,s1,vol2,vol1,rho,k,t,-b,a,w));
		return in;
	}
	
	CmssoFunction2::CmssoFunction2(const double s1,const double s2,
							 const double vol1,const double vol2,
							 const double rho,const double strike,const double t,
							 const double mult1,const double mult2, const int flavor):
	swapRate1_(s1), swapRate2_(s2), vol1_(vol1),vol2_(vol2),rho_(rho),strike_(strike),t_(t),
	mult1_(mult1),mult2_(mult2),flavor_(flavor) {}

	double CmssoFunction2::operator ()(const double& x) const {
		// this is Brigo, 13.16.2 with x = v/sqrt(2), v=sqrt(2)*x
		CumulativeNormalDistribution cnd(0.0,1.0);
		double mu1_=0.0;
		double mu2_=0.0;
		double v_=sqrt(2.0)*x;
		double h_=strike_+mult1_*swapRate1_*exp((mu1_-0.5*vol1_*vol1_)*t_+vol1_*sqrt(t_)*v_);
		double phi1_,phi2_;
		if(mult2_*swapRate2_/h_>0) {
			phi1_=cnd((double)flavor_*(log(mult2_*swapRate2_/h_)+(mu2_+(0.5-rho_*rho_)*vol2_*vol2_)*t_+rho_*vol2_*sqrt(t_)*v_)/
				(vol2_*sqrt(t_*(1.0-rho_*rho_))));
			phi2_=cnd((double)flavor_*(log(mult2_*swapRate2_/h_)+(mu2_-0.5*vol2_*vol2_)*t_+rho_*vol2_*sqrt(t_)*v_)/
				(vol2_*sqrt(t_*(1.0-rho_*rho_))));
		}
		else {
			phi1_= flavor_ == 1 ? 1.0 : 0.0;
			phi2_= phi1_;
		}
		double f=mult2_*(double)flavor_*swapRate2_*exp(mu2_*t_-0.5*rho_*rho_*vol2_*vol2_*t_+rho_*vol2_*sqrt(t_)*v_)*phi1_-(double)flavor_*h_*phi2_;
		double res=1.0/sqrt(M_PI)*exp(-x*x)*f;
		return res;
	}

	
}
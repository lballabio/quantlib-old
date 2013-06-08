#include <betadistribution.hpp>

using namespace std;

namespace QuantLib {

	BetaDistribution::BetaDistribution(double a, double b, bool meanStddevGiven) {
		if(meanStddevGiven) {
			QL_REQUIRE(a>0 && a<1, "Mean (" << a << ") must be in (0,1)");
			double c=(1.0-a)/a;
			a_ = (c-(2.0*c+1.0+c*c)*b*b)/((2.0*c+1.0+c*c)*(c+1.0)*b*b);
			b_ = a_*c;
		}
		else {
			a_=a;
			b_=b;
		}
	}
	
	double BetaDistribution::density(const double x) {
		if(x<=0.0 || x>=1.0) return 0.0;
		return pow(x,a_-1.0)*pow(1-x,b_-1.0) / betaFunction(a_,b_);	
	}

	double BetaDistribution::cumulative(const double x) {
		if(x<=0.0) return 0.0;
		if(x>=1.0) return 1.0;
		double p=incompleteBetaFunction(a_,b_,x);// / betaFunction(a_,b_);
		return p;
	}

	double BetaDistribution::cumulativeInv(const double p, const bool fast, const long bins) {
			if(fast) {
				if(!fastCalculated_ || bins!=fastBins_) {
					fastBins_=bins;
					preCalculateCumulativeInv();
				}
				return fastCumulativeInv(p);
			}
			BetaInvHelper hlp(this,p);
			Brent b;
			return b.solve(hlp,INVBETAACC,0.5,0.1);
	}

	double BetaDistribution::fastCumulativeInv(double p) {
		/* old: */
		/*unsigned int a=0,b=fastx_.size()-1,c;
		//if(p<=fastx_[0]) return fasty_[0]+(fasty_[0]-fasty_[1])/(fastx_[0]-fastx_[1])*(p-fastx_[0]);
		if(p<=fastx_[0]) return 0.0+(0.0-fasty_[0])/(0.0-fastx_[0])*(p-0.0);
		if(p>=fastx_[b]) return fasty_[b]+(fasty_[b-1]-fasty_[b])/(fastx_[b-1]-fastx_[b])*(p-fastx_[b]);
		do {
			if(b-a < 10) {
				while(p>fastx_[a]) a++;
				return fasty_[a-1]+(fasty_[a]-fasty_[a-1])/(fastx_[a]-fastx_[a-1])*(p-fastx_[a-1]);
			}
			else {
				c = (a+b)/2;
				if(p>=fastx_[c]) a=c; else b=c;
			}
		} while(true);*/
		
		// new:
		unsigned int a = (unsigned int)(p*fastBins_);
		if(a<fastBins_-1)
			return fasty_[a]+(fasty_[a+1]-fasty_[a])/(fastx_[a+1]-fastx_[a])*(p-fastx_[a]);
		else
			//return fasty_[a]+(fasty_[a]-fasty_[a-1])/(fastx_[a]-fastx_[a-1])*(p-fastx_[a]);
			return p<1.0-1.0E-8 ? cumulativeInv(p,false) : cumulativeInv(1.0-1.0E-8); // dont extrapolate near 1
	}

	void BetaDistribution::preCalculateCumulativeInv() {
		fastx_.clear();
		fasty_.clear();
		double p=0.0,pt;
		double step=1.0/((double)fastBins_);
		fastx_.push_back(0.0); fasty_.push_back(0.0); // for the new version of fast cum inv only!
		for(int i=0;i<fastBins_-1;i++) {
			p+=step;
			pt=p;									// uniform
			//pt=0.5*(sin(M_PI/2.0*(2.0*p-1))+1);	// generate more points at the ends
			fastx_.push_back(pt);
			fasty_.push_back(cumulativeInv(pt,false));
		}
		fastCalculated_=true;
	}

	
			
}


	

#include <gamma2distribution.hpp>

using namespace std;

namespace QuantLib {

	Gamma2Distribution::Gamma2Distribution(double a, double b, bool meanStddevGiven) {
		if(meanStddevGiven) {
			a_ = a*a/(b*b);
			b_ = b*b/a;
		}
		else {
			a_ = a;
			b_ = b;
		}
		fastCalculated_=false;
		fastBins_=0;
	}

	
	double Gamma2Distribution::density(const double x) {
		if(x<=0.0) return 0.0;
		return pow(x,a_-1.0)*exp(-x/b_)/pow(b_,a_)*exp(-gammaLn(a_));	
	}

	double Gamma2Distribution::cumulative(const double x) {
		if(x<=0.0) return 0.0;
		double p=incompleteGammaFunction(a_,x/b_);//*exp(-gammaLn(a_));
		//FILE *out=fopen("incomplgamma.log","a");
		//fprintf(out,"%f;%f\n",x,p);
		//fclose(out);
		return p;
	}

	double Gamma2Distribution::cumulativeInv(const double p, const bool fast, const long bins) {
			if(fast) {
				if(!fastCalculated_ || bins!=fastBins_) {
					fastBins_=bins;
					preCalculateCumulativeInv();
				}
				return fastCumulativeInv(p);
			}
			G2InvHelper hlp(this,p);
			Brent b;
			return b.solve(hlp,INVG2ACC,1.0,0.1);
	}

	double Gamma2Distribution::fastCumulativeInv(double p) {
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

	void Gamma2Distribution::preCalculateCumulativeInv() {
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


	

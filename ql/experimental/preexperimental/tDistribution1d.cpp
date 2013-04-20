#include <tDistribution1d.hpp>

using namespace std;

namespace QuantLib {

	
	double TDistribution1d::density(const double nu, const double x) {
		if(nu==0.0) 
			return exp(-x*x/2.0)/sqrt(2.0*M_PI);
		else
			return exp(gammaLn((nu+1.0)/2.0)-gammaLn(nu/2.0))*pow(1.0+x*x/nu,-(nu+1.0)/2.0)/sqrt(M_PI*nu);
	}

	double TDistribution1d::cumulative(const double nu, const double x) {
		if(nu==0.0) {
			CumulativeNormalDistribution cnd;
			return cnd(x);
		}
		else {
			double y = nu/(x*x+nu);
			return 0.5 + 0.5 * (x>0?1.0:-1.0) * ( incompleteBetaFunction (0.5 * nu, 0.5, 1.0)
									   -incompleteBetaFunction (0.5 * nu, 0.5, y));
		}
	}

	double TDistribution1d::cumulativeInv(const double nu, const double x) {
		if(nu==0.0) {
			InverseCumulativeNormal icn;
			return icn(x);
		}
		else {
			T1dInvHelper hlp(this,nu,x);
			Brent b;
			return b.solve(hlp,accuracy_,0.0,0.1);
		}
	}

	vector<double> TDistribution1d::sample(const double nu, const unsigned long n, const unsigned long seed) {
		
		MersenneTwisterUniformRng mt(seed);
		
		double u;
		vector<double> result;
		
		for(int i=0;i<n;i++) {
			u=mt.next().value;
			result.push_back(cumulativeInv(nu,u));
		}
		return result;
	}

	
			
}


	

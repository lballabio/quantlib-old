#ifndef _HAZARD__
#define _HAZARD__

#include <map>
#include <vector>
#include <math.h>

#define R 0.6  // recovery rate
#define absolute(x) (x > 0) ? x : -x
#define MAX_ITER    25
#define THIRTY_SIX  36
#define TWELVE      12
#define SIXTY       60
#define EIGHTY_FOUR 84

class HazardRate
{
	public:
		HazardRate() {}
		HazardRate(std::vector<double> sp) : spread(sp) {}
		virtual ~HazardRate() {}
		void init();			         // initialize and store tenors and 
         // discount rates	
		void calcDiscountBonds();                   // calculates discount bond prices
		double calcHazard(int num);	         // calculates hazard rates	
		double calcDeriv(double h, int num);   // calculates first derivatives in 
		double calcFunc(double h, int num);
		double Q(int num, double h, double t);
	private:
		std::vector<double> P;	       // stores discount coupon bond prices
		std::map<int,double> T;        // stores zero coupon maturities
		std::vector<double> delta;     // stores tenors between payment dates
		std::vector<double> r;	        // stores discount rates
		std::vector<double> haz;       // stores hazard rates
		std::vector<double> spread;  // stores spread quotes
};

#endif _HAZARD__

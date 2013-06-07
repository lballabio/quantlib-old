/*! \file Spread Option Pricer
    \brief pricer for 2 dim spread option, acc to Brigo, Apdx E
	Peter Caspers 
*/

#include <ql/quantlib.hpp>

#ifndef quantlib_spreadOptionPricer_hpp
#define quantlib_spreadOptionPricer_hpp


using namespace boost;
using namespace std;

namespace QuantLib {

	/*! generic spread option pricer acc to Brigo, Apdx E */
	
	class SpreadOptionPricer {
		public:	
			
			SpreadOptionPricer(int hermitePoints=32);

			Real npv(double s1, double s2, double k, double t, double vol1, double vol2, double rho, double a, double b, int w);
		
		private:
			void initHermiteIntegrator();
			GaussHermiteIntegration* ghInt_;
			long CMSSOHERMITEPOINTS;
			
	};


	/*! Helper class for Cmsso calculation (Brigo, 13.16.2)
	    WARNING: swap rate numbering is swapped here ;-) (payoff here is max(w*mult2*swaprate2-w*mult1*swaprate1-w*strike,0)) */

	class CmssoFunction2 {
		public:
			CmssoFunction2(const double s1,const double s2,
						  const double vol1,const double vol2,const double rho,
						  const double strike,const double t,const double mult1,const double mult2, const int flavor);
			double operator()(const double& x) const;
		
		private:
			double swapRate1_,swapRate2_;
			double vol1_,vol2_;
			double rho_;
			double strike_;
			double t_;
			double mult1_,mult2_;
			int flavor_;
	};

}

#endif

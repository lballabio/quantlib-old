/*! \file betaDistribution.hpp
	beta distribution (a,b)
	Peter Caspers */


#ifndef quantlib_betaDistribution_hpp
#define quantlib_betaDistribution_hpp

#include <ql/quantlib.hpp>
#include <gammaFunction.hpp>
#include <vector>
#include <math.h>
#include <stdio.h>

#define INVBETAACC 1.0E-6 // accuracy for inverse cumulative
#define SMALL 1.0E-8 // for p<SMALL, p>1-SMALL inverse cumulative is cut here

using namespace std;

namespace QuantLib {
	
	class BetaDistribution {
		
		public:

			BetaDistribution::BetaDistribution(double a, double b, bool meanStddevGiven=false);

			double density(const double x);
			double cumulative(const double x);
			double cumulativeInv(const double p, const bool fast=false, const long bins=100);

		private:

			double fastCumulativeInv(double p); // compute cumulative inverse using precalculation
			void preCalculateCumulativeInv();   // precalculate the cumulative inverse
			
			double a_, b_;

			bool fastCalculated_;
			long fastBins_;
			Interpolation fastInterpol_;
			vector<double> fastx_,fasty_;

			
	};

	class BetaInvHelper {
		
		public:
			
			BetaInvHelper(BetaDistribution *t, const double p) : t_(t), p_(p) {
				p_ = p;
				if(p_<SMALL) p_=SMALL;
				if(p_>1.0-SMALL) p_=1.0-SMALL;
			}

			double operator()(double x) const {
				return t_->cumulative(x)-p_;				
			}

		private:
			
			BetaDistribution *t_;
			double p_;
	};

}

#endif

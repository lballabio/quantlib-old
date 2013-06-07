/*! \file gamma2Distribution.hpp
	gamma distribution (a,b)
	Peter Caspers */


#ifndef quantlib_gamma2_hpp
#define quantlib_gamma2_hpp

#include <ql/quantlib.hpp>
#include <gammaFunction.hpp>
#include <vector>
#include <math.h>
#include <stdio.h>

#define INVG2ACC 1.0E-6 // accuracy for inverse cumulative
#define SMALL 1.0E-8 // for p<SMALL, p>1-SMALL inverse cumulative is cut here

using namespace std;

namespace QuantLib {
	
	class Gamma2Distribution {
		
		public:
			
			// distribution can be specified directly by parameters a and b
			// or via mean and stddev (meanStddevGiven=true)
			Gamma2Distribution(double a, double b, bool meanStddevGiven=false);

			double density(const double x);
			double cumulative(const double x);
			double cumulativeInv(const double p, const bool fast=false, const long bins=100);
			
			// compute cumulative inverse using precalculation, bins are taken from last invokation of cumulativeInv
			// cumulativeInv must have been invoked once at least to initialize precomputation!
			double fastCumulativeInv(double p); 

		private:
			
			void preCalculateCumulativeInv();   // precalculate the cumulative inverse
			
			double a_, b_;

			bool fastCalculated_;
			long fastBins_;
			//Interpolation fastInterpol_;
			vector<double> fastx_,fasty_;

			
	};

	class G2InvHelper {
		
		public:
			
			G2InvHelper(Gamma2Distribution *t, const double p) : t_(t), p_(p) {
				p_ = p;
				if(p_<SMALL) p_=SMALL;
				if(p_>1.0-SMALL) p_=1.0-SMALL;
			}

			double operator()(double x) const {
				return t_->cumulative(x)-p_;				
			}

		private:
			
			Gamma2Distribution *t_;
			double p_;
	};

}

#endif

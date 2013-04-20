/*! \file inverseGamma.hpp
	inverse gamma distribution
	Peter Caspers */


#ifndef quantlib_inverseGamma_hpp
#define quantlib_inverseGamma_hpp

#include <ql/quantlib.hpp>
#include <gammaFunction.hpp>
#include <vector>
#include <math.h>
#include <stdio.h>

#define INVGINVACC 1.0E-6 // accuracy for inverse cumulative
#define SMALL 1.0E-8 // for p<SMALL, p>1-SMALL inverse cumulative is cut here

using namespace std;

namespace QuantLib {
	
	class InverseGamma {
		
		public:

			InverseGamma(double a, double b) : a_(a), b_(b) {}

			double density(const double x);
			double cumulative(const double x);
			double cumulativeInv(const double p);

		private:

			double a_, b_;

			
	};

	class IGInvHelper {
		
		public:
			
			IGInvHelper(InverseGamma *t, const double p) : t_(t), p_(p) {
				p_ = p;
				if(p_<SMALL) p_=SMALL;
				if(p_>1.0-SMALL) p_=1.0-SMALL;
			}

			double operator()(double x) const {
				return t_->cumulative(x)-p_;				
			}

		private:
			
			InverseGamma *t_;
			double p_;
	};

}

#endif

/*! \file tDistribution1d.hpp
	t distribution 1 dim, for nu=0.0 normal distribution
	Peter Caspers */


#ifndef quantlib_tDistribution1d_hpp
#define quantlib_tDistribution1d_hpp

#include <ql/quantlib.hpp>
#include <gammaFunction.hpp>
#include <vector>
#include <math.h>
#include <stdio.h>

#define SMALL 1.0E-8 // for p<SMALL, p>1-SMALL inverse cumulative is cut here

using namespace std;

namespace QuantLib {
	
	class TDistribution1d {
		
		public:

			TDistribution1d(double accuracy=1.0E-13) : accuracy_(accuracy) {} // accuracy for inverse cumulative

			double density(const double nu, const double x);
			double cumulative(const double nu, const double x);
			double cumulativeInv(const double nu, const double p);
			vector<double> sample(const double nu, const unsigned long n, const unsigned long seed);

		private:
			double accuracy_;

			
	};

	class T1dInvHelper {
		
		public:
			
			T1dInvHelper(TDistribution1d *t, const double nu, const double p) : t_(t), nu_(nu), p_(p) {
				p_ = p;
				if(p_<SMALL) p_=SMALL;
				if(p_>1.0-SMALL) p_=1.0-SMALL;
			}

			double operator()(double x) const {
				return t_->cumulative(nu_,x)-p_;				
			}

		private:
			
			TDistribution1d *t_;
			double p_,nu_;
	};

}

#endif

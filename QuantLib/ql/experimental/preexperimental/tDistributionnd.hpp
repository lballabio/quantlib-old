/*! \file tDistribution1d.hpp
	t distribution n dim
	Peter Caspers */


#ifndef quantlib_tDistributionnd_hpp
#define quantlib_tDistributionnd_hpp

#include <ql/quantlib.hpp>
#include <gammaFunction.hpp>
#include <inverseGamma.hpp>
#include <tDistribution1d.hpp>
#include <vector>
#include <math.h>
#include <stdio.h>


using namespace std;

namespace QuantLib {
	
	class TDistributionnd {
		
		public:

			TDistributionnd(const Matrix& corr);
			TDistributionnd(const vector<vector<double>>& corr);

			double density(const double nu, const vector<double> x);

			// generates t-copula sample from U[0,1] input. If normalize = true U[0,1] output is generated, t else
			vector<double> copulaSample(const double nu, const vector<double>& u, const double v=0.0, const bool normalize=true); 
			// generates n t-copula samples using mersenne twister 
			vector<vector<double>> copulaSample(const double nu, const unsigned long n, const unsigned long seed=0); 
			// returns cholesky decomposition of correlation matrix
			Matrix& cholesky(); 

		private:
			
			void setup();
			Matrix corr_,SInv_,Chol_;
			double det_;
			Size p_;

			
	};

}

#endif

/*! \file riskengine.hpp
    \brief loads risk factor data and computes this and that
	Peter Caspers */


#ifndef quantlib_riskengine_hpp
#define quantlib_riskengine_hpp

#include <ql/quantlib.hpp>
#include <wgz_utilities.hpp>
#include <gammaFunction.hpp>
#include <kernelDensity.hpp>
#include <tDistribution1d.hpp>
#include <tDistributionnd.hpp>
#include <vector>
#include <math.h>
#include <stdio.h>
#include <iostream>
#include <fstream>

using namespace std;

#define SMALLESTEIGENVALUE 1.0E-6	// Eigenvalues smaller than this are replaced by this value

namespace QuantLib {
	
	class RiskEngine {
		
		public:

			RiskEngine(string path);
			RiskEngine(vector<string> names, vector<long> dates, vector<vector<double>> values);

			vector<double> series(string name); // return data of series given by name
			vector<long> dates(string name); // return dates of series given by name
			vector<string> names(); // return all series names

			boost::shared_ptr<KernelDensity> kernelDensity(string name, int method); // return kernel density of data, method: 0 = raw, 1 = abs diff, 2 = rel diff, 3 = log diff
			vector<vector<double>> consolidateData(vector<string> names, int mode); // consolidate given data series (only take common dates) and transform with given mode
			long numberOfData(vector<string> names); // return number of lines in consolidated data matrix

			// compute correlation matrix from kendalls tau (naive=true => use linear correlation)
			// if start > 0 a weighting scheme is used with a decay factor of lambda
			Matrix correlation(vector<string> names,  int mode, bool ensurePositivity=false, bool naive=false, int start=0, double lambda=0.0); 
			Matrix correlation(const vector<vector<double>>& data,  bool ensurePositivity=false, bool naive=false, int start=0, double lambda=0.0); // compute correlation matrix from kendalls tau
			vector<double> eigenvalues(vector<string> names,  int mode, bool naive=false,int start=0, double lambda=0.0); // return vector of eigenvalues of (uncorrected) correlation matrix
			vector<double> tLogLikelihood(vector<string> names, int mode, vector<double> nu); // compute t-loglikelihood for data, correlation taken from kendalls tau. For nu=0.0, gauss log-likelihood is computed
			
			boost::shared_ptr<KernelDensity> lossDistribution(vector<string> names, const vector<double> deltas, const vector<double> gammas, const int method, const vector<double> fx, const double nu, const long paths=1000, const long seed=0,
						int start=0, double lambda=0.9923, bool naive=false, bool normal=false, int resolution=100); // compute loss distribution with mc and t-copula

			boost::shared_ptr<KernelDensity> lossDistribution(vector<string> names, const vector<double> deltas, const vector<double> gammas, const int method, const vector<double> fx,
				int length=0); // compute historical simulation var, based on last length observations (0 all observations)

		private:
			
			long lookup(string name); // searches for series index, throws exception if not found
			vector<double> transformData(const vector<double>& data, int mode=0); // transform given data, method: 0 = raw, 1 = abs diff, 2 = rel diff, 3 = log diff
			double kendallsTau(const vector<double>& d1, const vector<double>& d2,int start=0, double lambda=0.0); // compute kendalls tau
			double naiveCorr(const vector<double>& d1, const vector<double>& d2,int start=0, double lambda=0.0); // compute naive correlation coeff

			vector<string> name_;
			vector<vector<double>> value_;
			vector<vector<long>> date_;
			

	};

}

#endif

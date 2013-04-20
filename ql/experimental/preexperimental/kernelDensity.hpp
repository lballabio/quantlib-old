/*! \file kernelDensity.hpp
    \brief Kernel Density Estimation
	
	The automatic bandwidth selection can be done for the distribution function following
	"Bandwidth Selection for Nonparametric Distribution" and for the density following
	"Lecture Notes on Nonparametrics", both by Bruce E. Hansen.

	Peter Caspers */


#ifndef quantlib_kernelDensity_hpp
#define quantlib_kernelDensity_hpp

#include <ql/quantlib.hpp>
#include <gammaFunction.hpp>
#include <vector>
#include <math.h>
#include <stdio.h>

using namespace std;

namespace QuantLib {
	
	class KernelDensity {
		
		public:

			// construct density object by given data, hDist and hDens are bandwidth for distribution and density
			// if zero the optimal bandwidth are determined
			// for distribution bandwidth this follows "Bandwidth Selection for Nonparametric Distribution Estimation" (Bruce E Hansen) with given iterations (4 recommended)
			// for density bandwidth a cross validation scheme is used searching bandwidth in hmin=plugin bandwidth / factor, ... , hmax=plugin bandwidth * factor with steps points
			KernelDensity(const vector<double>& data, const double hDist = 0.0, const double hDens = 0.0, const int iterations=0, const int steps=50, const double factor=3.0);
		
			// set default bandwidth for density, if 0.0 set this value to optimal bandwidth for density in interval p/factor,p*factor, where p is the plug bandwidth
			double setDensityBandwidth(const double h=0.0, const int steps=50, const double factor=3.0);
			// set default bandwidth for distribution, if 0.0 set this value to optimal bandwidth for distribution using given number of iterations (0, ... 4)
			double setDistributionBandwidth(const double h=0.0, const int iterations=4);

			// return default bandwidth for density
			double densityBandwidth();
			// return default bandwidth for distribution
			double distributionBandwidth();

			// return density w.r.t. bandwidth h, if 0.0 default bandwidth for density is used
			// if leaveOut>=0, the ith data point is left out in the estimation (for cross validation)
			double density(const double x, const double h=0.0, const int leaveOut=-1);

			// return cumulative w.r.t. bandwidth h, if h=0.0 default bandwidth for cumulative is used
			double cumulative(const double x, const double h=0.0);
			// return inverse cumulative w.r.t. bandwidth h (see above), fast=true precomputes a grid with number of points given by bins and interpolates on this grid
			// ! fast computation should be improved, see the corresponding algorithms in betaDistribution and gammaDistribution !
			double cumulativeInverse(const double p, const double h=0.0, bool fast=false, const long bins=100, const double accuracy=1.0E-13);

			// return new kernel density estimated from quantiles computed on a partition consisting of n batches of the data
			// the distribution bandwidth is computed as (n/n0)^(1/3) * Original bandwidth
			// where n=original number of data points, n0= number of data points in one batch
			boost::shared_ptr<KernelDensity> quantileDensity(const double p, const long numberOfBatches);

			// use sample variance estimation (with decay and start value calculation, if you want) and normal distribution for cumulative inverse
			// this has nothing to do with kernel densities and is only here for purposes of the risk engine class
			double normalCumulativeInv(const double p, const int start=0, const double lambda=0.9923);
			
			// return the input data on which the estimator is based
			vector<double> data();
			// return minimum over data
			double minData(); 
			// return maximum over data
			double maxData(); 

			// return first four moments (non standardized, but centralized) (numbers 1 - 4)
    	    // 2nd and 4th raw moment (i.e. usual point estimators, numbers 5-6)
			vector<double> moments(); 

			// return first two weighted moments
			vector<double> weightedMoments(int start=0, double lambda=0.9932); 

			// return point mass density
			vector<double> pointMassDensity(double unit);

			// return point mass density minimum index
			long pointMassMinIndex(double unit);

			// return quantile based on point mass density
			double pointMassCumulativeInverse(double unit, double p);
			

		private:

			double fastCumulativeInv(double p); // compute cumulative inverse using precalculation
			void preCalculateCumulativeInv();   // precalculate the cumulative inverse

			double kernel(const double x);  // kernel pdf
			double kernelF(const double x); // kernel cdf
			double kernelC(const double x); // kernel self convolution
			double kernelPsi();				// kernel psi

			double hermiteP(const int m, const double x); // m-th hermite polynomial at x

			double optimalBandwidth(const int iterations=0); // compute optimal bandwidth for distribution (see above)
			double optimalBandwidth2(int steps=10, double factor=3.0); // compute optimal bandwidth for density (searching a grid of steps points in hmin=plug in bw / factor, hmax=plug in bw * factor
			double plugInBandwidth(); // compute plug in bandwidth (as a starting point for cross validation)
			vector<double> crossValidation(double h1, double h2, int steps); // cross validation for optimal density bandwidth estimation
			double estimateRm(const int m, const double r); // helper for bandwidth calculation
			
			double normalDensity(double x); // return normal density function

			void setup(const double hDist = 0.0, const double hDens = 0.0, const int iterations=0, const int steps=50, const double factor=3.0); // set up internal data

			vector<double> data_;	// data on which estimator is based
			long n_;				// number of data points
			double h_,h2_;			// default bandwidth for distribution and density
			
			CumulativeNormalDistribution* cnd_;
			InverseCumulativeNormal* icn_;

			vector<double> moments_;
			bool momentsCalculated_;
			double mH_;

			vector<double> weightedMoments_;
			bool weightedMomentsCalculated_;
			double wmStart_,wmLambda_;

			double minData_,maxData_;
			bool minCalculated_,maxCalculated_;

			bool fastCalculated_;
			long fastBins_;
			double fastH_, fastMin_, fastMax_;
			Interpolation fastInterpol_;
			vector<double> fastx_,fasty_;

			vector<double> pointMassDensity_;
			double pointMassDensityUnit_;
			bool pointMassDensityCalculated_;
			long pointMassDensityMinIndex_;

			class InvHelper {
		
				public:
					
					InvHelper(KernelDensity *density, double p, double h=0.0) : 
					  density_(density), p_(p), h_(h){ }

					double operator()(double x) const {
						return density_->cumulative(x,h_)-p_;				
					}

				private:
					
					KernelDensity *density_;
					double p_,h_;
					bool fast_;
			};

	};
	

}

#endif

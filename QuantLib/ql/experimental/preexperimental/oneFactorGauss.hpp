/*! \file oneFactorGauss.hpp
    \brief one factor gauss credit risk model
	Peter Caspers */


#ifndef quantlib_onefactorgauss_hpp
#define quantlib_onefactorgauss_hpp

#include <ql/quantlib.hpp>
#include <wgz_utilities.hpp>
#include <kernelDensity.hpp>
#include <vector>
#include <math.h>
#include <stdio.h>

using namespace std;

namespace QuantLib {
	
	class OneFactorGauss {
		
		public:

			OneFactorGauss();
			
			// sample numberOfObligors and estimate pd via #Defaults/#Obligors, return density for this
			boost::shared_ptr<KernelDensity> empiricalPdDensity(long numberOfObligors, double pd, double correlation, int seed=0, long samples=100000);
			
			// sample numberOfObligors and estimate gordy var, return density for this
			boost::shared_ptr<KernelDensity> empiricalGordyVarDensity(long numberOfObligors, double pd, double quantile, double correlation, int seed=0, long samples=100000);
			
			// sample convexity adjusted gordy var
			boost::shared_ptr<KernelDensity> convexityAdjGordyVarDensity(long numberOfObligors, double pd, double quantile, double correlation, bool useCorr=true,int seed=0, long samples=100000);
			
			// return empirical bootstrap variance of pd
			double empiricalBootstrapVariance(long numberOfObligors, double pd, double correlation, int seed, long samples);

			// simulate portfolio loss for pd homogeneous portfolio
			boost::shared_ptr<KernelDensity> lossDistribution(long numberOfObligors, double pd, double correlation, int seed=0, long samples=100000);
			

		private:

			NormalDistribution nd_;
			CumulativeNormalDistribution cnd_;
			InverseCumulativeNormal icn_;
			GaussHermiteIntegration* gh_;

			long sampleNumberOfDefaults(long numberOfObligors, double pd, double correlation, MersenneTwisterUniformRng& mt);

	};

}

#endif

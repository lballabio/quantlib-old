/*! \file ultraCon.hpp
    \brief credit risk model

	Peter Caspers */


#ifndef quantlib_ultraCon_hpp
#define quantlib_ultraCon_hpp

#include <ql/quantlib.hpp>
#include <vector>
#include <map>
#include <math.h>
#include <stdio.h>
#include <kernelDensity.hpp>
#include <tDistributionnd.hpp>
#include <gamma2Distribution.hpp>
#include <betaDistribution.hpp>

using namespace std;
using namespace boost;

#define GAMMABINS 10000			  // number of bins for fast inversion of gamma distribution (if pds are gamma distributed)
#define SMALLESTEIGENVALUE 1.0E-6 // small positive value which replaces negative eigenvalues in correlation matrix

namespace QuantLib {
	
	class UltraCon {
		
		public:

			// construct model on basis of given
			// - sector correlation matrix (must be positive definite)
			// - list of entities with given id (just for information), pd, sector association and exposure
			// - exposure unit for analytical computations
			UltraCon(const Matrix& sectorCorrelations,
				const vector<string>& id,
				const vector<double>& pd, 
				const vector<double>& stddev, const vector<long>& sector, 
				const vector<double>& exposure,
				const double& unit = 1000000.0);
		
			// return loss distribution generated from paths simulations paths
			// seed is the seed for mersenne twister rng for the simulation
			// lognormal = false then use gamma distributed pds, true then use lognormal pds
			// nu=0 then use gaussian copula, nu>0 then use t-copula (nu>0 only possible for gamma distributed pds!)
			boost::shared_ptr<KernelDensity> lossDistribution(const long paths, const long seed=0, const bool lognormal=false, const double nu=0.0);

			// return correlation matrix (which might have changed since it is
			// enforced to be positive definite
			Matrix correlation() { return sectorCorr_; }

			// return expected loss in exposure bands (divided by unit (see constructor))
			vector<double> exposureBandsEl() { return epsilonC_; }

			// return list of ids of obligors
			vector<string> obligorId() { return id_; }

			// return sector exposures
			vector<double> sectorExposure() { return sectorExposure_; }

			// return analytic loss distribution (i.e. probabilities belonging to unit multipliers)
			vector<double> analyticLossDistribution() { return analyticLossDistribution_; }

			// return analytic loss quantile
			double analyticLossQuantile(double p);

			// return analytic risk contribution (Bürgisser: Integrating Correlations)
			vector<double> marginalRisk() { return riskContribution_; }

			// return synthetic standard deviation (formula 12)
			double syntheticStddev() { return syntheticStddev_; }

			// return exposure bands
			vector<double> exposureBands() { return nuC_; }

			// activate stochastic exposures
			// the exposures set in the constructor are assumed to be the unsecured volumes
			// the securization vector here are the corresponding security volumes
			// the recovery rate is assumed to be beta distributed (hard coded)
			// stochastic exposures only possible with lognormal pds!
			//bool setStochasticExposures(const vector<double>& securization);
			

		private:

			void doAnalyticComputations(); // does the computations for analytic loss distribution and risk contributions
			void computeAnalyticLossDistribution(); // computes the analytic loss distribution

			Matrix sectorCorr_;
			TDistributionnd* sectorDist_;
			vector<string> id_;
			vector<double> pd_;
			vector<double> stddev_;
			vector<long> sector_;
			vector<double> exposure_;
			vector<double> mu_,sigma_; // mu and sigma parameter for lognormal distribution to match entities pd and stddev
			vector<boost::shared_ptr<Gamma2Distribution>> invGamma_; // inverse gamma distributions with mean, stddev = entity pd and stddev
			
			double syntheticStddev_;	// sigma in formula 12 in Bürgisser Integrating Correlatoins
			double matchStdDev2_; // rhs in formula 13

			vector<double> sectorPdSum_; // pd sum per sector
			vector<double> sectorExpectedLoss_; // EL per sector
			vector<double> sectorStddev_; // (pd weighted) stddev of pd scaling factor per sector
			vector<double> sectorExposure_; // exposure per sector
			
			vector<double> sectorSpecTerms_; // sector specific terms in formula 15 Bürgisser Integrating Correlatoins
			vector<double> riskContribution_; // RC_A, formula 15 Bürgisser Integrating Correlatoins
			vector<double> analyticLossDistribution_; // credit risk 1 sector approximate loss distribution (see bürgisser paper)
			
			double exposureSum_;	// Credit Risk+ specific calculations, sum of exposures
			double expectedLoss_;	// total expected loss
			double pdSum_;			// sum of pds
			double sigmaC_;			// sigmaC is the relative default variance from formula 13 in Bürgisser
			double alphaC_, betaC_; // pdSum / sigmaC, sigmaC^2 / mu
			double pC_;				// betaC / (1 + betaC)

			double unit_;			// exposure unit L
			double exposureBands_;  // number of exposure bands
			long upperIndex_; // biggest number of units that can occur as loss
			map<long,double,less<long>> epsNuC_; // map of nuC_ to expected loss
			long maxNu_; // biggest exponent nuC_ occuring in previous map
			vector<double> epsilonC_, nuC_; // expected loss and common exposure in band j (only needed as return value for information!)

			int n_; // number of sectors
			int m_; // number of entities

			//bool useStochasticExposure_;
			//vector<double> securization_;



	};
	

}

#endif

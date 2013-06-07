/*! \file lgdsim.hpp
    \brief simulation engine for stochastic lgd losses

	Peter Caspers */


#ifndef quantlib_lgdSim_hpp
#define quantlib_lgdSim_hpp

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

#define FBINS 10000 // number of bins used for fast computation of inverse gamm / beta distribution
#define IMPLSECACC 0.01

namespace QuantLib {
	
	class LgdSim {
		
		public:

			// construct model on basis of given
			// - distribution for the recovery rate
			//   type: 0 normal 1 lognormal 2 gamma 3 beta
			//   distribution is cut at given lower and upper points for normal, lognormal, gamma
			//   mean and stddev are mean and standard dev of the uncutted version!
			//   use recRateMean and recRateStddev to get mean and standard dev of the cutted version
			//   for the beta distribution the original support (0,1) is transformed 
			//   to (lower cut, upper cut)
			// - list of total risk volumes
			// - list of already impaired volumes
			LgdSim(const vector<double>& riskVolume, 
					 const vector<double>& impairedVolume,
					 const int type,
					 const double mean,
					 const double stddev,
					 const double lowerCut=0.0,
					 const double upperCut=2.0);
		
			// return loss distribution generated from paths simulations paths
			// seed is the seed for mersenne twister rng for the simulation
			boost::shared_ptr<KernelDensity> lossDistribution(const long paths, const long seed=0);

			// return implied secured volumne
			vector<double> impliedSecuredVolume() { return secVol_; }

			// return density of recovery rate at x (including the cuts)
			double recRateDensity(double x);

			// return cumulative of recovery rate at x (not (!) including the cuts)
			double recRateCumulative(double x);

			// return inverse cumulative of recovery rate (including the cuts, for simulation purposes)
			// of course cutted version is used here
			double recRateInverseCumulative(double u);

			// compute expteced recovery as E( max ( riskVol, securization * recRate ) )
			double computeExpectedRecovery(double riskVol, double securization);

			// return mean of recovery rate
			double recRateMean();

			// return stddev of recovery rate
			double recRateStddev();


		private:
			
			vector<double> riskVolume_;
			double riskVolSum_;
			vector<double> impairedVolume_;
			vector<bool> active_;

			NormalDistribution* nd_;
			CumulativeNormalDistribution* cnd_;
			InverseCumulativeNormal* icn_;
			Gamma2Distribution* gamma_;
			BetaDistribution* beta_;
			
			double mean_,stddev_;
			double mLog_,sLog_; // mean and stddev for lognormal distribution to match mean_, stddev_
			double densityLift_; // factor to multiply to get an integral of 1
			double lowerCut_,upperCut_; // lower cut and upper cut for rec rate distribution
			double lowerCum_,upperCum_; // cumulative at lower and upper cut
			int type_;

			vector<double> secVol_;

			GaussLegendreIntegration* glInt_;
			Array glIntR_,glIntW_;

	};

	class ImpliedSecHelper {
		
		public:
			
			ImpliedSecHelper(LgdSim *t, const double r, const double er) : t_(t), r_(r), er_(er) {
			}

			double operator()(double x) const {
				return t_->computeExpectedRecovery(r_,x)-er_;				
			}

		private:
			
			LgdSim *t_;
			double er_,r_;
	};

}

#endif

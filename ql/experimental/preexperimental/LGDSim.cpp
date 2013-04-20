#include <LgdSim.hpp>

using namespace std;

namespace QuantLib {

	LgdSim::LgdSim(const vector<double>& riskVolume, 
					 const vector<double>& impairedVolume,
					 const int type,
 				     const double mean,
					 const double stddev,
					 const double lowerCut,
					 const double upperCut) :

	riskVolume_(riskVolume), impairedVolume_(impairedVolume),
		mean_(mean), stddev_(stddev), type_(type), lowerCut_(lowerCut), upperCut_(upperCut) { 
				
				QL_REQUIRE(lowerCut>=0.0 && upperCut>lowerCut,"0 <= lowerCut (" << lowerCut << ") < upperCut (" << upperCut << ")"); 
				QL_REQUIRE(type >=1 && type <= 4, "LgdSim Type " << type << " not known.");
				QL_REQUIRE(riskVolume_.size() == impairedVolume_.size(),"Risk Volume size (" << riskVolume_.size() << ") must be equal to impaired Volume size (" << impairedVolume_.size());

				// initialize distribution objects for later use
				if(type==1) {
					nd_=new NormalDistribution();
					cnd_=new CumulativeNormalDistribution();
					icn_=new InverseCumulativeNormal();
				}
				if(type==2) {
					nd_=new NormalDistribution();
					cnd_=new CumulativeNormalDistribution();
					icn_=new InverseCumulativeNormal();
					double tlog=log(1.0+stddev_*stddev_/(mean_*mean_));
					mLog_=log(mean_)-0.5*tlog;
					sLog_=tlog;
				}
				if(type==3) {
					gamma_=new Gamma2Distribution(mean,stddev,true);
				}
				if(type==4) {
					beta_=new BetaDistribution((mean-lowerCut)/(upperCut-lowerCut),stddev/(upperCut-lowerCut),true);
				}

				// compute density lift factor due to cuts
				upperCum_=recRateCumulative(upperCut);
				lowerCum_=recRateCumulative(lowerCut);
				densityLift_ = 1.0 / (upperCum_-lowerCum_);

				glInt_ = new GaussLegendreIntegration(64);
				glIntR_=glInt_->x();
				glIntW_=glInt_->weights();

				// compute implied secured volume and sum of risk volumes
				riskVolSum_=0.0;
				Brent b;
				for(int i=0;i<riskVolume_.size();i++) {
					if(riskVolume_[i]<=impairedVolume_[i]) {
						secVol_.push_back(0.0);
						active_.push_back(false);
					}
					else {
						active_.push_back(true);
						riskVolSum_+=riskVolume_[i];
						ImpliedSecHelper hlp(this,riskVolume_[i],riskVolume_[i]-impairedVolume_[i]);
						try {
							secVol_.push_back(b.solve(hlp,IMPLSECACC,1.0,0.1));					
						} catch(...) {
							QL_FAIL("Error while computing implied secured volume # " << i);
						}
					}
				}	
	
	}

	boost::shared_ptr<KernelDensity> LgdSim::lossDistribution(const long paths, const long seed) {

		QL_REQUIRE(paths<=5000000,"paths (" << paths << ") should not exceed 5 Mio.");
		
		vector<double> data;

		MersenneTwisterUniformRng mt(seed); // rng for simulation
		
		double rec,u,loss;
		for(int i=0;i<paths;i++) {
			loss=0.0;
			for(int j=0;j<riskVolume_.size();j++) {
				if(active_[j]) {
					u=mt.next().value;
					rec=recRateInverseCumulative(u);
					loss+=riskVolume_[j]-std::min(rec*secVol_[j],riskVolume_[j]);
				}
			}
			data.push_back(loss);
		}
	
		// use iterations = 0 for distribution bandwidth and default value for density bandwidth
		return boost::shared_ptr<KernelDensity>(new KernelDensity(data,0.0,riskVolSum_/10000.0,0)); 

	}

	double LgdSim::recRateDensity(double x) {
		if(type_==1) {
			double y=(*nd_)((x-mean_)/stddev_)/stddev_;
			return x<lowerCut_ || x>upperCut_ ? 0.0 : y*densityLift_;
		}
		if(type_==2) {
			if(x<=0.0) return 0.0;
			double y=(*nd_)((log(x)-mLog_)/sLog_)/(x*sLog_);
			return x<lowerCut_ || x>upperCut_ ? 0.0 : y*densityLift_;
		}
		if(type_==3) {
			if(x<=0.0) return 0.0;
			double y = (*gamma_).density(x);
			return x<lowerCut_ || x>upperCut_ ? 0.0 : y*densityLift_;			
		}
		if(type_==4) {
			if(x<=lowerCut_ || x>=upperCut_) return 0.0;
			double y = (*beta_).density((x-lowerCut_)/(upperCut_-lowerCut_)) / (upperCut_-lowerCut_);
			return x<lowerCut_ || x>upperCut_ ? 0.0 : y*densityLift_;
		}
		QL_FAIL("LgdSim Type " << type_ << " not known.");
	}

	double LgdSim::recRateCumulative(double x) {
		if(type_==1) {
			return (*cnd_)((x-mean_)/stddev_);
		}
		if(type_==2) {
			return (*cnd_)((log(x)-mLog_)/sLog_);
		}
		if(type_==3) {
			return (*gamma_).cumulative(x);
		}
		if(type_==4) {
			if(x<=lowerCut_) return 0.0;
			if(x>=upperCut_) return 1.0;
			return (*beta_).cumulative((x-lowerCut_)/(upperCut_-lowerCut_));
		}
		QL_FAIL("LgdSim Type " << type_ << " not known.");
	}

	double LgdSim::recRateInverseCumulative(double u) {
		double us = lowerCum_ + u * (upperCum_ - lowerCum_);
		if(type_==1) {
			return (*icn_)(us)*stddev_+mean_;
		}
		if(type_==2) {
			return exp((*icn_)(us)*sLog_+mLog_);
		}
		if(type_==3) {
			return (*gamma_).cumulativeInv(us,true,FBINS); // fast calculated
		}
		if(type_==4) {
			return (*beta_).cumulativeInv(us,true,FBINS)*(upperCut_-lowerCut_)+lowerCut_; // fast calculated
		}
		QL_FAIL("LgdSim Type " << type_ << " not known.");
	}

	double LgdSim::computeExpectedRecovery(double riskVol, double securization) {
		double res=0.0;
		double x,y;
		for(int i=0;i<glIntR_.size();i++) {
			y=glIntR_[i];
			x=lowerCut_+(upperCut_-lowerCut_)*(y+1.0)/2.0;
			res+=std::min(riskVol,securization*x)*recRateDensity(x)*glIntW_[i];
		}
		return res*(upperCut_-lowerCut_)/2.0;
	}

	double LgdSim::recRateMean() {
		double res=0.0;
		double x,y;
		for(int i=0;i<glIntR_.size();i++) {
			y=glIntR_[i];
			x=lowerCut_+(upperCut_-lowerCut_)*(y+1.0)/2.0;
			res+=x*recRateDensity(x)*glIntW_[i];
		}
		return res*(upperCut_-lowerCut_)/2.0;
	}

	double LgdSim::recRateStddev() {
		double mean = recRateMean();
		double res=0.0;
		double x,y;
		for(int i=0;i<glIntR_.size();i++) {
			y=glIntR_[i];
			x=lowerCut_+(upperCut_-lowerCut_)*(y+1.0)/2.0;
			res+=(x-mean)*(x-mean)*recRateDensity(x)*glIntW_[i];
		}
		return sqrt(res*(upperCut_-lowerCut_)/2.0);
	}

			
}


	

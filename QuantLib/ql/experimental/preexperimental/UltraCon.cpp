#include <ultraCon.hpp>


namespace QuantLib {

	UltraCon::UltraCon(const Matrix& sectorCorrelations, const vector<string>& id,
		const vector<double>& pd, const vector<double>& stddev, const vector<long>& sector,
		const vector<double>& exposure, const double& unit) :

		sectorCorr_(sectorCorrelations), id_(id), pd_(pd), stddev_(stddev), 
			sector_(sector), exposure_(exposure), unit_(unit) {

		//useStochasticExposure_=false;

		n_ = sectorCorr_.rows();

		// ensure positive definiteness of matrix
		
		SymmetricSchurDecomposition dec(sectorCorr_);
		Array ev = dec.eigenvalues();
		Matrix tr = dec.eigenvectors();
		Array dg(n_);

		for(int i=0;i<n_;i++) {
			if(ev[i] < SMALLESTEIGENVALUE)
				ev[i]=SMALLESTEIGENVALUE;
		}
		
		for(int i=0;i<n_;i++) {
			for(int j=0;j<n_;j++) {
				sectorCorr_[i][j]=0.0;
				for(int k=0;k<n_;k++) {
					sectorCorr_[i][j] += tr[i][k]*ev[k]*tr[j][k];
				}
			}
		}

		for(int i=0;i<n_;i++) {
			dg[i]=sectorCorr_[i][i];
		}

		for(int i=0;i<n_;i++) {
			for(int j=0;j<n_;j++) {
				sectorCorr_[i][j]/=sqrt(dg[i]*dg[j]);
			}
		}
		// end of pos def

		sectorDist_ = new TDistributionnd(sectorCorr_); // t Distribution with sector correlation matrix

		m_ = id_.size();
		QL_REQUIRE(pd_.size()==m_,"pd list (" << pd_.size() << ") must have the same length as id list (" << m_ << ")");
		QL_REQUIRE(sector_.size()==m_,"sector list (" << sector_.size() << ") must have the same length as id list (" << m_ << ")");
		QL_REQUIRE(exposure_.size()==m_,"exposure list (" << exposure_.size() << ") must have the same length as id list (" << m_ << ")");

		// precompute mu and sigma (lognormal parameters for matching pd expectation and stddev) and 
		// inverse gamma distributions with mean, stddev = entity pd, stddev
		// exposure sum, total expected loss, check for regular sectors
	
		double var=0.0;
		exposureSum_=0.0;
		expectedLoss_=0.0;
		pdSum_=0.0;
		map<double,boost::shared_ptr<Gamma2Distribution>,less<double>> tmpG2; // temp map of pd to precomputed gamma distribution
		map<double,boost::shared_ptr<Gamma2Distribution>,less<double>>::iterator g2Iter; // iterator for previous map

		for(int k=0;k<m_;k++) {
			// check if sector is regular and pd positive
			QL_REQUIRE(sector_[k]>=0 && sector_[k]<n_,"sector #" << k << " (" << sector_[k] << ") must be in range 0 ... n (" << n_ << ")");
			QL_REQUIRE(pd[k]>0.0,"pd #" << k << " is zero or negative");
			// precompute mu and sigma
			var=log(1.0+stddev_[k]*stddev_[k]/(pd_[k]*pd_[k]));
			sigma_.push_back(sqrt(var));
			mu_.push_back(log(pd_[k]) - 0.5 * var);
			// precompute inverse gamma distributions
			g2Iter=tmpG2.find(pd_[k]);
			if(g2Iter==tmpG2.end()) {
				boost::shared_ptr<Gamma2Distribution> g2(new Gamma2Distribution(pd_[k]*pd_[k]/(stddev_[k]*stddev_[k]),(stddev_[k]*stddev_[k])/pd_[k]));
				g2->cumulativeInv(0.5,true,GAMMABINS); // force precomputation
				tmpG2.insert(pair<double,boost::shared_ptr<Gamma2Distribution>>(pd_[k],g2));
				invGamma_.push_back(g2);
			}
			else {
				invGamma_.push_back(((*g2Iter).second));
			}
			// compute exposure sum
			exposureSum_+=exposure[k];
			// compute total expected loss
			expectedLoss_+=pd[k]*exposure[k];
		}

		// do analytical computations
		doAnalyticComputations();
		computeAnalyticLossDistribution();

	}

	void UltraCon::doAnalyticComputations() {

		// init vectors
		sectorPdSum_ = vector<double>(n_,0.0);
		sectorExposure_ = vector<double>(n_,0.0);
		sectorExpectedLoss_ = vector<double>(n_,0.0);
		sectorStddev_ = vector<double>(n_,0.0);
		sectorSpecTerms_ = vector<double>(n_,0.0);
		riskContribution_ = vector<double>(m_,0.0);
		vector<double> pdAdj(m_,0.0);

		// compute exposure bands
		exposureBands_=0;
		maxNu_=0;
		upperIndex_=0;
		map<long,double,less<long>>::iterator iter;
		for(int k=0;k<m_;k++) {
			long exUnit = floor(0.5 + exposure_[k] / unit_); // round
			//long exUnit = exposure_[k] / unit_;		     // always round down and adjust pd up
			if(exposure_[k]>0 && exUnit==0) exUnit=1;        // but avoid zero exposure
			if(exUnit>maxNu_) maxNu_ = exUnit;
			pdAdj[k]=exposure_[k] > 0.0 ? exposure_[k]*pd_[k]/(exUnit*unit_) : 0.0; // adjusted pd
			double el = exUnit * pdAdj[k];
			//double el = exUnit * pd_[k];
			if(exUnit > 0) {
				iter=epsNuC_.find(exUnit);
				if(iter==epsNuC_.end()) {
					epsNuC_.insert(pair<long,double>(exUnit,el));
				}
				else {
					(*iter).second += el ;
				}
				upperIndex_+=exUnit;
			}
		}
		for(iter=epsNuC_.begin();iter!=epsNuC_.end();iter++) {
			nuC_.push_back((double)(*iter).first);
			epsilonC_.push_back((*iter).second);
			exposureBands_++;
		}
		
		// compute per sector figures
		for(int k=0;k<m_;k++) {
			pdSum_+=pdAdj[k]; // for band computation => take adjusted pd
			sectorPdSum_[sector_[k]]+=pd_[k];
			sectorExposure_[sector_[k]]+=exposure_[k];
			sectorExpectedLoss_[sector_[k]]+=exposure_[k]*pd_[k];
			sectorStddev_[sector_[k]]+=stddev_[k]; // this is pd weighted, exposure weighted: stddev[k]/pd[k]*exposure[k];
		}

		for(int i=0;i<n_;i++) {
			// pd weighted stddev needs to be divided by sector pd sum
			if(sectorPdSum_[i]>0.0 /* exposure weighted: sectorExposure_[i]>0.0*/) {
				sectorStddev_[i]/=sectorPdSum_[i]; // exposure weighted: sectorExposure_[i];
			}
			else {
				sectorStddev_[i]=0.0;
			}
		}
		for(int i=0;i<n_;i++) {
			// precompute sector specific terms (formula 15 in Bürgisser)
			sectorSpecTerms_[i] += sectorStddev_[i]*sectorStddev_[i]*sectorExpectedLoss_[i];
			for(int j=0;j<n_;j++) {
				if(j!=i) {
					sectorSpecTerms_[i] += sectorCorr_[i][j]*sectorStddev_[i]*sectorStddev_[j]*sectorExpectedLoss_[j];
				}
			}
		}
		
		// compute synthetic standard deviation (formula 12 in Bürgisser)
		syntheticStddev_=0.0;
		for(int i=0;i<n_;i++) {
			syntheticStddev_+=sectorStddev_[i]*sectorStddev_[i]*sectorExpectedLoss_[i]*sectorExpectedLoss_[i];
			for(int j=0;j<n_;j++) {
				if(j!=i) syntheticStddev_+=sectorCorr_[i][j]*sectorStddev_[i]*sectorStddev_[j]*
											sectorExpectedLoss_[i]*sectorExpectedLoss_[j];
			}
		}
		matchStdDev2_=syntheticStddev_; // formula 13 rhs
		for(int k=0;k<m_;k++) {
			syntheticStddev_+=pd_[k]*exposure_[k]*exposure_[k];
		}
		syntheticStddev_ = sqrt(syntheticStddev_);

		// compute risk contributions (formula 15 in Bürgisser, these sum up to UL = synthetic standard deviation)
		for(int k=0;k<m_;k++) {
			riskContribution_[k] = pd_[k]*exposure_[k] / syntheticStddev_ *
									( sectorSpecTerms_[sector_[k]] + exposure_[k] );
		}

		// compute sigmaC_ 
		// and deduced figures
		sigmaC_ = pdSum_*sqrt(matchStdDev2_ / (expectedLoss_*expectedLoss_));
		alphaC_ = pdSum_*pdSum_ / (sigmaC_*sigmaC_);
		betaC_ = sigmaC_ * sigmaC_ / pdSum_;
		pC_ = betaC_ / (1.0+betaC_);

	}

	void UltraCon::computeAnalyticLossDistribution() {

		analyticLossDistribution_.clear();

		// compute A(0)
		analyticLossDistribution_.push_back(pow(1.0-pC_,alphaC_));

		// compute A(n+1) recursively
		double res;
		map<long,double,less<long>>::iterator iter;
		for(long n=0;n<upperIndex_-1;n++) { // test
			res=0.0;
			for(long j=0;j<=std::min<long>(maxNu_-1,n);j++) {
				iter=epsNuC_.find(j+1);
				if(iter!=epsNuC_.end()) {
					res+=(*iter).second*analyticLossDistribution_[n-j]*alphaC_;
					if(j<=n-1) res+=(*iter).second/((double)(j+1))*((double)(n-j))*analyticLossDistribution_[n-j];
				}
			}
			analyticLossDistribution_.push_back(res*pC_/(pdSum_*((double)(n+1))));
		}

	}

	double UltraCon::analyticLossQuantile(double p) {
		
		// look up the two neighbour points
		int i=0;
		double sum=0.0;
		do {
			sum+=analyticLossDistribution_[i];
			i=i+1;
		} while(i<upperIndex_ && sum < p);
		
		double p1=sum-analyticLossDistribution_[i-1];
		double p2=sum >= p ? sum : 1.0;
		double l1=(i-1)*unit_;
		double l2=i*unit_;

		return l1+(p-p1)/(p2-p1)*(l2-l1);

	}

	boost::shared_ptr<KernelDensity> UltraCon::lossDistribution(const long paths, const long seed, const bool lognormal, const double nu) {
		
		QL_REQUIRE(paths<=5000000,"paths (" << paths << ") should not exceed 5 Mio.");

		vector<double> v(n_,0.0);	// random vector 
		double w=0.0;				// random number, only needed for nu > 0
		vector<double> u(n_,0.0);	// correlated normally distributied vector
		vector<double> def;		    // defaulted exposures sum realizations per path
		double pathDef;				// sum of defaulted exposures in one path

		// --- stochastic exposure ---
		//BetaDistribution beta(1.77,4.75);
		// ----------------------------

		MersenneTwisterUniformRng mt(seed); // rng for simulation
		
		for(int i=0;i<paths;i++) {
			// only needed for nu > 0
			if(nu>0) {
				w=mt.next().value;
			}
			for(int k=0;k<n_;k++) {
				v[k]=mt.next().value;
			}
			// start simulation
			//double rec=0.0; // for stochastic exposures
			pathDef=0.0;
			// use lognormal distributed pds (like criscon)
			if(lognormal) {
				u = sectorDist_->copulaSample(0.0,v,w,false); // false = do not normalize to U[0,1]
				for(int k=0;k<m_;k++) {
					// --- stochastic exposure ---
					/*if(useStochasticExposure_) {
						//rec=beta.cumulativeInv(mt.next().value,true,10000)*2.0*securization_[k];
						//if(rec>exposure_[k]) rec = exposure_[k];
						mt.next().value;
						rec=0.0;
					}*/
					// ---------------------------
					if(mt.next().value <= exp(u[sector_[k]]*sigma_[k]+mu_[k])) {
						pathDef+=exposure_[k]; // -rec; for stochastic exposure
					}
				}
			} 
			// use gamma distributed pds (like credit risk+)
			else {
				u = sectorDist_ -> copulaSample(nu,v,w,true); // true = normalize to U[0,1]
				for(int k=0;k<m_;k++) {
					if(mt.next().value <= invGamma_[k]->fastCumulativeInv(u[sector_[k]])) { // fast calculation (see constructor for precomputation)
						pathDef+=exposure_[k];
					}
				}
			}
			def.push_back(pathDef);
		}
		
		// use iterations = 0 for distribution bandwidth and default value for density bandwidth
		return boost::shared_ptr<KernelDensity>(new KernelDensity(def,0.0,exposureSum_/10000.0,0));

	}

	/*bool UltraCon::setStochasticExposures(const vector<double>& securization) {
		
		QL_REQUIRE(securization.size() == m_,"List of securities (" << securization.size() <<") must have equal length as obligors (" << m_ <<")");
		
		useStochasticExposure_=true;
		securization_=securization;

		return true;
		
	}*/

			
}


	

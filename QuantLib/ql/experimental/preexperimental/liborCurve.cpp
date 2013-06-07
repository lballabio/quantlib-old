#include <liborCurve.hpp>

namespace QuantLib {


	void LiborCurve::resetPath() {
		for(int i=0;i<N_;i++) {
			rates_[i]=initialRates_[i];
		}
		currentIndex_=-1;
		t_=0.0;
		v_=v0_;
		//sobolBg_->nextPath();
		mtBg_->nextPath();
		//rRate_=0.230165; // DEBUG
	}

	double LiborCurve::evolve(Size steps) {
		
		double wgt=1.0;

		while(steps-- > 0) {

			// evolve rates

			// only rates that are alive at target time t are evolved (assuming that the rateTimes grid points are always subset of evolved times)
			Size m = currentIndex_+1;

			double Delta=(rateTimes_[m] - t_)/((double)REFINEMENTSTEPS);

			Size refSteps=REFINEMENTSTEPS;
			do {
				
				// Mersenne Twister own
				/*for(int k=0; k < K_+1; k++) {
					n_[k]=inverseCumulative_(mt_.next().value); // normal variate
				}*/
				// Mersenne Twister QL
				wgt*=mtBg_->nextStep(n_);
				// Sobol
				//wgt*=sobolBg_->nextStep(n_);

				double vv = v_>0.0 ? v_ : 0.0;

				for(int i=m; i<N_; i++) { // i = libor rate #
					double b=beta_->valueAtIndex(currentIndex_,i);
					double sum=0.0;
					for(int k=0; k<K_; k++) {
						double drift=0.0;
						double s=sigma_->valueAtIndex(currentIndex_,i,k);
						// spot measure
						for(int j=m;j<=i;j++) { 
							double tau=rateTimes_[j+1]-rateTimes_[j];
							drift+=sigma_->valueAtIndex(currentIndex_,j,k)*tau*(b*rates_[j]+(1.0-b)*initialRates_[j]) / 
								(1.0 + tau*rates_[j]);
						}
						// terminal measure (belonging to last libor)
						/*for(int j=i+1;j<N_;j++) { 
							double tau=rateTimes_[j+1]-rateTimes_[j];
							drift+=-sigma_->valueAtIndex(currentIndex_,j,k)*tau*(b*rates_[j]+(1.0-b)*initialRates_[j]) / 
								(1.0 + tau*rates_[j]);
						}*/
						//swap measure (w.r.t. alpha and beta, S_alpha,beta the swap rate)
						/*Size alpha=10; Size beta=alpha+20; // 5y10y swap rate
						Size sPf=1; // payFreq
						for(int j=alpha+1;j<=beta;j++) {
							double tau=rateTimes_[j+1]-rateTimes_[j];
							double sgn = j <= i+1 ? 1.0 : -1.0;
							double fk = (rateTimes_[j+1]-rateTimes_[j])*discountFactor(currentIndex_ < 0 ? 0 : currentIndex_,rateTimes_[j])/swapAnnuity(currentIndex_ < 0 ? 0 : currentIndex_,alpha,beta-alpha,sPf);
							for(int l=std::min(i+1,j);l<=std::max(i,j-1);l++) {
								drift+=sigma_->valueAtIndex(currentIndex_,j,k)*tau*(b*rates_[j]+(1.0-b)*initialRates_[j]) / 
									(1.0 + tau*rates_[j]);
							}
							drift*=fk*sgn;
						}*/
						sum+=b*s*sqrt(vv)*n_[k]*sqrt(Delta)+(b*vv*s*drift-0.5*b*b*s*s*vv)*Delta;
					}
					factors_[i]=exp(sum);			
				}
				
				// update current state of rates
				for(int i=m; i<N_; i++) {
					double b=beta_->valueAtIndex(currentIndex_,i);
					rates_[i]=(rates_[i]+(1.0-b)/b*initialRates_[i])*factors_[i]-(1.0-b)/b*initialRates_[i];
					//if(rates_[i]<MINRATE) rates_[i]=MINRATE;
				}

				//DEBUG: evolve rRate
				/*double volR=0.1676;
				rRate_*=exp(-0.5*volR*volR*Delta+volR*sqrt(Delta)*n_[0]);
				*/
				// evolve variance process
				v_ = omega_+(v_-omega_)*exp(-theta_*Delta)+n_[K_]*eta_*sqrt(vv)*sqrt(0.5/theta_*(1.0-exp(-2.0*theta_*Delta))) ;

			} while(--refSteps>0); // do REFINEMENTSTEPS steps for evolution between rate times
		
			// update current time
			currentIndex_++;
			t_=rateTimes_[currentIndex_]; 
		}
		return wgt;
	}

	int LiborCurve::currentIndex() {
		return currentIndex_;
	}

	void LiborCurve::shift(double offset) {
		for(int i=0;i<N_;i++) {
			initialRates_[i]+=offset;
			rates_[i]+=offset;
		}
	}

	void LiborCurve::shift(Size fixing, double offset, bool currentStateOnly) {
		rates_[fixing]+=offset;
		if(!currentStateOnly) initialRates_[fixing]+=offset;
	}


	Real LiborCurve::spotNumeraire() {
		double p=1.0;
		for(int j=0;j<currentIndex_;j++) {
			p*=1.0+(rateTimes_[j+1]-rateTimes_[j])*rates_[j];
		}
		return p;
	}

	Real LiborCurve::forwardRate(Size fixing) {
		return rates_[fixing];
	}

	Real LiborCurve::swapRate(Size fixing, Size length, Size payFreq) {
		Size pf = payFreq;
		double sum=0.0;
		double p=1.0;
		for(int j=fixing;j<fixing+length;j++) {
			p*=1.0/(1.0+(rateTimes_[j+1]-rateTimes_[j])*rates_[j]);
			if(--pf == 0) { sum+=p*(rateTimes_[j+1]-rateTimes_[j+1-payFreq]); pf=payFreq; }
 		}
		return (1.0-p)/sum;
	}

	Real LiborCurve::rRate(Size reference, Size fixing, Size delay, Size length, Size payFreq) {
		QL_REQUIRE(reference<=fixing,"reference index " << reference << " must be leq fixing " << fixing);
		double sumD=rateTimes_[fixing+length]-rateTimes_[fixing];
		return swapAnnuityT0(fixing,length,payFreq)/(discountFactorT0(0,fixing+delay)*sumD)*
			(discountFactor(reference,fixing+delay)*sumD/swapAnnuity(reference,fixing,length,payFreq)-1.0);
	}

	Real LiborCurve::swap2TerminalMeasure(Size reference, Size fixing, Size delay, Size length, Size payFreq) {
		return discountFactor(reference,fixing+delay)/discountFactorT0(0,fixing+delay) *
				  swapAnnuityT0(fixing,length,payFreq)/swapAnnuity(reference,fixing,length,payFreq);
	}


	Real LiborCurve::swapAnnuity(Size reference, Size fixing, Size length, Size payFreq) {
		QL_REQUIRE(reference<=fixing,"reference index " << reference << " must be leq fixing " << fixing);
		Size pf = payFreq;
		double sum=0.0;
		double p=1.0;
		for(int j=fixing;j<fixing+length;j++) {
			p*=1.0/(1.0+(rateTimes_[j+1]-rateTimes_[j])*rates_[j]);
			if(--pf == 0) { sum+=p*(rateTimes_[j+1]-rateTimes_[j+1-payFreq]); pf=payFreq; }
		}
		//printf("swap Annuity=%1.11f\n",discountFactor(reference,fixing)*sum);
		return discountFactor(reference,fixing)*sum;
	}

	Real LiborCurve::swapAnnuityT0(Size fixing, Size length, Size payFreq) {
		Size pf = payFreq;
		double sum=0.0;
		double p=1.0;
		for(int j=fixing;j<fixing+length;j++) {
			p*=1.0/(1.0+(rateTimes_[j+1]-rateTimes_[j])*initialRates_[j]);
			if(--pf == 0) { sum+=p*(rateTimes_[j+1]-rateTimes_[j+1-payFreq]); pf=payFreq; }
		}
		return discountFactorT0(0,fixing)*sum;
	}

	Real LiborCurve::discountFactor(Size start, Size end) {
		double p=1.0;
		for(int j=start;j<end;j++) {
			p*=1.0/(1.0+(rateTimes_[j+1]-rateTimes_[j])*rates_[j]);
		}
		//printf("discount Factor=%1.11f\n",p);
		return p;
	}

	Real LiborCurve::discountFactorT0(Size start, Size end) {
		double p=1.0;
		for(int j=start;j<end;j++) {
			p*=1.0/(1.0+(rateTimes_[j+1]-rateTimes_[j])*initialRates_[j]);
		}
		//printf("discount Factor T0=%1.11f\n",p);
		return p;
	}

	// private helper functions

	/*Size LiborCurve::nextResetIndex(Real t) const {
		QL_REQUIRE(t>=0.0 || t<=rateTimes_[N_], "t (" << t << ") must be >= 0 and <= last rate time (" << rateTimes_[N_] << ")");
		if(t<=rateTimes_[0]) return 0; 
		Size i = (int)(t * N_ / (rateTimes_[N_] - rateTimes_[0]));
		if(i>N_) i=N_;
		while(rateTimes_[i]<t) i++;
		while(rateTimes_[i]>=t) i--;
		return i+1;
	}*/
			

}
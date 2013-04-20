#include <betaParametrization.hpp>

namespace QuantLib {

	Size PiecewiseConstantBeta::nextResetIndex(Real t) const {
		QL_REQUIRE(t>=0.0 && t<=rateTimes_[N_], "t (" << t << ") must be >= 0 and <= last rate time (" << rateTimes_[N_] << ")");
		if(t<=rateTimes_[0]) return 0; 
		Size i = (int)(t * N_ / (rateTimes_[N_] - rateTimes_[0]));
		if(i>N_) i=N_;
		while(rateTimes_[i]<t) i++;
		while(rateTimes_[i]>=t) i--;
		return i+1;
	}

	Real PiecewiseConstantBeta::operator()(Real t, Size i) const {
		QL_REQUIRE(i<N_,"i (" << i << ") must be < N (" << N_ << ")");
		return beta(t,i);
	}

	Real PiecewiseConstantBeta::valueAtIndex(int m, Size i) const {
		QL_REQUIRE(i<N_,"i (" << i << ") must be < N (" << N_ << ")");
		int mp=m+1;
		if(mp>i) return 0.0;
		return beta_[i-mp];
	}


	void PiecewiseConstantBeta::recalibrate(std::vector<double>& parameters) {
		parameters_=parameters;
		setParameters();
	}

	void PiecewiseConstantBeta::setParameters() {
		switch(mode_) {
			case 0:	 for(int i=0;i<N_;i++) {
				beta_[i]=parameters_[i];
					 }
					 break;
			case 1: for(int i=0;i<N_;i++) {
				beta_[i]=1.0-parameters_[0]*exp(-parameters_[1]*(double)i);
					}
					break;
			default: QL_FAIL("mode " << mode_ << " not supported.");
				break;
		}
	}

	Real PiecewiseConstantBeta::penaltyFactor() {
		double p=1.0;
		// all modes
		for(int i=0;i<N_;i++) {
			if(beta_[i]<0.0 || beta_[i]>1.0) p*=100.0;
			// mode 1
			if(mode_==1) {
				if(parameters_[0]<0.0 || parameters_[0]>1.0) p*=100.0; // initital value between 0 and 1
				if(parameters_[1]<0.0) p*=100.0; // decay factor shall be positive
			}
			return p;
		}
	}

	// private helper functions

	Real PiecewiseConstantBeta::beta(Real t, Size i) const {
		Size m=nextResetIndex(t);
		if(m>i) return 0.0;
		return beta_[i-m];
	}


}
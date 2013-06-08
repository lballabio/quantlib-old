#include <sigmaParametrization.hpp>

namespace QuantLib {

	Size AbcdVolRatioCorrSigma::nextResetIndex(Real t) const {
		QL_REQUIRE(t>=0.0 && t<=rateTimes_[N_], "t (" << t << ") must be >= 0 and <= last rate time (" << rateTimes_[N_] << ")");
		if(t<=rateTimes_[0]) return 0; 
		Size i = (int)(t * N_ / (rateTimes_[N_] - rateTimes_[0]));
		if(i>N_) i=N_;
		while(rateTimes_[i]<t) i++;
		while(rateTimes_[i]>=t) i--;
		return i+1;
	}

	Real AbcdVolRatioCorrSigma::operator()(Real t, Size i, Size k) const {
		QL_REQUIRE(i<N_,"i (" << i << ") must be < N (" << N_ << ")");
		//Size m = nextResetIndex(t);
		//if(k<m || k>i) return 0.0;
		double etai= eta(t,i,k);
		double res=abcd(rateTimes_[i]-t)*etai*phi_[i];
		if(twoState_) {
			res=exp(-twoStateLambda_*t)*res+(1.0-exp(-twoStateLambda_*t))*abcd(rateTimes_[i]-t,true)*etai*phi_[i];
		}
		return res;
	}

	Real AbcdVolRatioCorrSigma::valueAtIndex(int m, Size i, Size k) const {
		QL_REQUIRE(i<N_,"i (" << i << ") must be < N (" << N_ << ")");
		QL_REQUIRE(integralprecomputed_,"Integral must be precomputed");
		int mp=m+1;
		if(mp>i) return 0.0;
		double t=0.0; 
		if(m>=0) t=rateTimes_[m];
		return sqrt(integral0_[i*N_*N_*numberOfFactors_+i*N_*numberOfFactors_+k*N_+mp]/(rateTimes_[mp]-t));
		//return factorReducedEta_[i-mp][k]*abcd(rateTimes_[i]-t); // this is for sigma value at index m
	}

	void AbcdVolRatioCorrSigma::precomputeIntegral(Real theta) {
		//printf("Precompute sigma integral (theta=%f)...",theta);
		QL_REQUIRE(theta>0.0,"theta (" << theta << ") must be >0");
		if(integralprecomputed_ && theta==integralthetavalue_) return; // already precomputed for this theta
		for(int k=0;k<numberOfFactors_;k++) {
			for(int i=0;i<N_;i++) {
				for(int j=0;j<=i;j++) {
					for(int m=0;m<N_;m++) {
						//printf("k=%d i=%d j=%d m=%d\n",k,i,j,m);
						double t0= m>0 ? rateTimes_[m-1] : 0.0;
						double t1= rateTimes_[m];
						double i0= integrateOnePeriod(m,i,j,k,0.0);
						double it= integrateOnePeriod(m,i,j,k,theta);
						double itm= integrateOnePeriod(m,i,j,k,-theta);
						integral0_[i*N_*N_*numberOfFactors_+j*N_*numberOfFactors_+k*N_+m]=i0;
						integraltheta_[i*N_*N_*numberOfFactors_+j*N_*numberOfFactors_+k*N_+m]=it;
						integralminustheta_[i*N_*N_*numberOfFactors_+j*N_*numberOfFactors_+k*N_+m]=itm;
					}
				}
			}
		}
		integralthetavalue_=theta;
		integralprecomputed_=true;
		//printf("done\n");
	}

	Real AbcdVolRatioCorrSigma::integrateOnePeriod(int index, Size i, Size j, Size k, Real theta) const {
		if(i<index || j<index) return 0.0;
		double li=0.0;
		if(index>0) li=rateTimes_[index-1];
		double ri = rateTimes_[index];
		double etai= factorReducedEta_[i-index][k]; // this is eta(ri,i,k);
		double etaj= factorReducedEta_[j-index][k]; // this is eta(ri,j,k);
		return etai*etaj*abcdIntegrate(li,ri,i,j,theta)*phi_[i]*phi_[j];
	}

	Real AbcdVolRatioCorrSigma::integrate(Real t0, Real t1, Size i, Size j, Size k, Real theta) const {
		QL_REQUIRE(i<N_ && j<N_,"i (" << i << ") and j (" << j << ") must be < N (" << N_ << ")");
		//QL_REQUIRE(j<=i,"i (" << i << ") must be >= j (" << j <<")");
		if(i<j) {
			Size i2=i;
			i=j;
			j=i2;
		}
		Size m0 = nextResetIndex(t0);
		Size m1 = nextResetIndex(t1);
		double sum=0.0;
		for(int m=m0;m<=m1;m++) {
			//if(k>=m && k<=i && k<=j) {
				double li = m>0 ? rateTimes_[m-1] : 0.0;
				double ri = rateTimes_[m];
				double left = std::max(t0,li);
				double right = std::min(t1,ri);
				if(/*!debug &&*/ left==li && right==ri && integralprecomputed_ && (theta==0.0 || integralthetavalue_==fabs(theta))) { // in case of full interval take precomputed value
					if(theta==0.0)	sum+=integral0_[i*N_*N_*numberOfFactors_+j*N_*numberOfFactors_+k*N_+m];
					else {
						if(theta>0.0) sum+=integraltheta_[i*N_*N_*numberOfFactors_+j*N_*numberOfFactors_+k*N_+m]; 
								 else sum+=integralminustheta_[i*N_*N_*numberOfFactors_+j*N_*numberOfFactors_+k*N_+m]; 
					}
				}
				else {
					double etai= eta(right,i,k);
					double etaj= eta(right,j,k);
					sum+=etai*etaj*abcdIntegrate(left,right,i,j,theta)*phi_[i]*phi_[j];
					//if(debug && theta==0.0 && (i==10 || i==11 || j==10 || j==11)) printf("%d;%d;%1.12f;%1.12f;%1.12f;%1.12f;%d;%1.12f\n",i,j,li,ri,etai,etaj,k,abcdIntegrate(left,right,i,j,theta));
				}
			//}
		}
		return sum;
	}

	Real AbcdVolRatioCorrSigma::fullFactorCorrelation(Real t, Size i, Size j) const {
		QL_REQUIRE(i<N_ && j<N_,"i (" << i << ") and j (" << j << ") must be < N (" << N_ << ")");
		Size m = nextResetIndex(t);
		//if(m==N_ || i<m || j<m) return 0.0;
		double sum=0.0;
		for(int k=m;k<=N_-1;k++) {
			double etai= fullFactorEta(t,i,k);
			double etaj= fullFactorEta(t,j,k);
			sum+=etai*etaj;
		}
		return sum;
	}

	Real AbcdVolRatioCorrSigma::correlation(Real t, Size i, Size j) const {
		QL_REQUIRE(i<N_ && j<N_,"i (" << i << ") and j (" << j << ") must be < N (" << N_ << ")");
		Size m = nextResetIndex(t);
		//if(m==N_ || i<m || j<m) return 0.0;
		double sum=0.0;
		for(int k=0;k<numberOfFactors_;k++) {
			sum+=eta(t,i,k)*eta(t,j,k);
		}
		return sum;
	}

	void AbcdVolRatioCorrSigma::recalibrate(std::vector<double>& parameters) {
		parameters_=parameters;
		setParameters();
		factorReduction();
		integralprecomputed_=false;
		precomputeIntegral(integralthetavalue_);
	}

	void AbcdVolRatioCorrSigma::changeMode(int mode, std::vector<double>& parameters) {
		parameters_=parameters;
		mode_=mode;
		twoState_=false;
		// set number of parameters
		switch(mode_) {
			case 0: P_=4+N_-1;
					break;
			case 1: P_=4+1;
					break;
			case 2: P_=4+3;
					break;
			case 3: P_=4;
					break;
			case 4: P_=4+4;
					break;
			case 5: P_=4+3;
					break;
			case 6: P_=4+2;
					break;
			case 7: P_=4+4+1;
					twoState_=true;
					break;
			default: QL_FAIL("mode " << mode_ << " not supported.");
		}

		QL_REQUIRE(parameters.size()==P_,"Parameters size (" << parameters.size() << ") must match " << P_ << " which is the required size for mode " << mode_);

		// initialize true parameters a,b,c,d and beta according to parameters_
		setParameters();
	}

	void AbcdVolRatioCorrSigma::setParameters() {
		a_=parameters_[0];
		b_=parameters_[1];
		c_=parameters_[2];
   		d_=parameters_[3];
		switch(mode_) {
			case 0:	 for(int i=0;i<N_;i++) {
						  double sum=0.0;
						  for(int l=2;l<=N_;l++) {
							  sum+=std::min<int>(l-1,i)*parameters_[4+l-2];
						  }
						  beta_[i]=exp(sum);
					  }
					  break;
			case 1:  for(int i=0;i<N_;i++) {
						  beta_[i]=exp(std::min<int>(N_-1,i)*parameters_[4]);
					  }
					  break;
			case 2: for(int i=0;i<N_;i++) {
						double sumi=0.0;
						for(int l=2;l<=N_;l++) {
							double deltaL;
							if(l<N_) 
								deltaL=parameters_[4]*(N_-l)/(N_-3)+parameters_[5]*(l-1)/(N_-3);
							else
								deltaL=parameters_[6];
							sumi+=std::min<int>(l-1,i)*deltaL;
						}
						beta_[i]=exp(sumi);
					}
					break;
			case 3: break;
			case 4: for(int i=0;i<N_;i++) {
						phi_[i]=parameters_[5]+(parameters_[6]-parameters_[5])*exp(-parameters_[7]*(double)i);
					}
					for(int i=0;i<N_;i++) {
						  beta_[i]=exp(std::min<int>(N_-1,i)*parameters_[4]);
					  }
					 break;
			case 5: for(int i=0;i<N_;i++) {
						phi_[i]=parameters_[4]+(parameters_[5]-parameters_[4])*exp(-parameters_[6]*(double)i);
					}
					break;
			case 6: for(int i=0;i<N_;i++) {
						phi_[i]=1.0+(parameters_[4]-1.0)*exp(-parameters_[5]*(double)i);
					}
					break;
			case 7: a2_=parameters_[4];
					b2_=parameters_[5];
					c2_=parameters_[6];
					d2_=parameters_[7];
					twoStateLambda_=parameters_[8];
					break;
			default: QL_FAIL("mode " << mode_ << " not supported.");
				break;
		}
	}

	Real AbcdVolRatioCorrSigma::penaltyFactor() {
		double p=1.0;
		// all modes
		//if(a_<0.0) p*=100; // a positive (not necessary)
		if(b_+d_ < 0.0 || c_ < 0.0 || d_ < 0.0) p*=100.0; // long term and time zero vola > 0, decay >0
		for(int i=0;i<N_;i++) if(beta_[i]<0.0) p*=100.0; // all betas >0
		//if(d_ > 1.0) p*=exp(d_-1.0); // d <= 100% (not necessary)
		//if(b_+d_ > 1.0) p*=exp(b_+d_-1.0); // b+d <=100% (not necessary)
		// mode 1
		if(mode_==1) {
			if(parameters_[4]<=0.0) p*=100.0; // decay factor for correlation must be > 0
			if(parameters_[4]>0.5) p*=exp(parameters_[4]); // decay factor shall not be too big (not neccessary)
		}
		// mode 2
		if(mode_==2) {
			if(parameters_[4]<=0.0 || parameters_[5]<=0.0) p*=100.0; // alpha1 and alpha2 must be >= 0
			if(parameters_[6]<=0.0) p*=100.0; // beta must be >= 0
		}
		// mode 3
		// mode 4
		if(mode_==4) {
			if(parameters_[4]<=0.0) p*=100.0; // decay factor for correlation must be > 0
			if(parameters_[4]>0.5) p*=exp(parameters_[4]); // decay factor shall not be too big
			if(parameters_[5]<=0.0) p*=100.0; // dinf >0
			if(parameters_[6]<=0.0) p*=100.0; // d0 > 0
			if(parameters_[7]<=0.0) p*=100.0; // lambda > 0
			if(parameters_[6]<=parameters_[5]) p*=100.0; // d0>dinf
		}
		// mode 5
		if(mode_==5) {
			if(parameters_[4]<=0.0) p*=100.0; // dinf >0
			if(parameters_[5]<=0.0) p*=100.0; // d0 > 0
			if(parameters_[6]<=0.0) p*=100.0; // lambda > 0
			if(parameters_[5]<=parameters_[4]) p*=100.0; // d0>dinf
		}
		// mode 6
		if(mode_==6) {
			if(parameters_[4]<=1.0) p*=100.0; // d0 > 1.0
			if(parameters_[5]<=0.0) p*=100.0; // lambda > 0
		}
		// mode 7
		if(mode_==7) {
			//if(a2_<0.0) p*=100; // a (2nd state) positive not necessary
			if(b2_+d2_ < 0.0 || c2_ < 0.0 || d2_ < 0.0) p*=100.0; // 2nd state long term and time zero vol >0, decay>0
			//if(parameters_[7]<2.0) p*=exp(2.0-parameters_[7]); // make excited period short (not necessary)
		}
		return p;
	}

	void AbcdVolRatioCorrSigma::factorReduction() {
		//printf("doing factor reduction...");
		if(penaltyFactor()>=100.0) { printf("not possible due to non admissable parameters, keeping previous one\n"); return; }
		Matrix corr(N_,N_);
		for(int i=0;i<N_;i++) {
			for(int j=0;j<N_;j++) {
				corr[i][j]=fullFactorCorrelation(0.0,i,j);
			}
		}
		factorReducedEta_=rankReducedSqrt(corr,numberOfFactors_,1.0,SalvagingAlgorithm::Spectral); // Spectral
		//factorReducedEta_=Matrix(N_,N_,0.0);for(int i=0;i<N_;i++) factorReducedEta_[i][i]=1.0; // test!!! full factor uncorrelated
		QL_ENSURE(factorReducedEta_.columns()==numberOfFactors_,"Wrong number of columns: "
                      << factorReducedEta_.columns() << " instead of " << numberOfFactors_);
		//printf("done\n");
	}

	const Matrix& AbcdVolRatioCorrSigma::factorReducedCorrelationMatrix() const {
		return factorReducedEta_;
	}

	// private helper functions
	
	Real AbcdVolRatioCorrSigma::beta(Real t, Size k) const {
		Size m=nextResetIndex(t);
		if(m>k) return 0.0;
		return beta_[k-m];
	}

	Real AbcdVolRatioCorrSigma::fullFactorEta(Real t, Size i, Size k) const {
		Size m = nextResetIndex(t);
		return i < k ? 0.0 : sqrt(beta(t,k)*beta(t,k) - (k>m ? beta(t,k-1)*beta(t,k-1):0.0)) / beta(t,i);
	}

	Real AbcdVolRatioCorrSigma::eta(Real t, Size i, Size k) const {
		Size m=nextResetIndex(t);
		if(i<m || k>numberOfFactors_-1) return 0.0;
		return factorReducedEta_[i-m][k];
	}

	Real AbcdVolRatioCorrSigma::abcdIntegrate(double t0, double t1,Size i, Size j, double theta) const {
		double S=rateTimes_[i];
		double T=rateTimes_[j];
		double cutOff = std::min(S,T);
        if (t0>=cutOff) {
            return 0.0;
        } else {
            cutOff = std::min(t1, cutOff);
			double res;
			if(twoState_) {
				res = (primitive(cutOff, T, S, theta-2.0*twoStateLambda_, false,false) - primitive(t0, T, S,theta-2.0*twoStateLambda_, false,false)) +
					(primitive(cutOff, T, S, theta-twoStateLambda_, false,true) - primitive(t0, T, S,theta-twoStateLambda_, false,true)) -
					(primitive(cutOff, T, S, theta-2.0*twoStateLambda_, false,true) - primitive(t0, T, S,theta-2.0*twoStateLambda_, false,true)) +
					(primitive(cutOff, T, S, theta-twoStateLambda_, true,false) - primitive(t0, T, S,theta-twoStateLambda_, true,false)) -
					(primitive(cutOff, T, S, theta-2.0*twoStateLambda_, true,false) - primitive(t0, T, S,theta-2.0*twoStateLambda_, true,false)) +
					(primitive(cutOff, T, S, theta, true,true) - primitive(t0, T, S,theta, true,true)) -
					2.0*(primitive(cutOff, T, S, theta-twoStateLambda_, true,true) - primitive(t0, T, S,theta-twoStateLambda_, true,true)) +
					(primitive(cutOff, T, S, theta-2.0*twoStateLambda_, true,true) - primitive(t0, T, S,theta-2.0*twoStateLambda_, true,true));
			}
			else {
				res = primitive(cutOff, T, S, theta) - primitive(t0, T, S,theta);
			}
            return res;
        }
	}

	Real AbcdVolRatioCorrSigma::abcd(Real t, bool secondState) const {
		if(!secondState)
			return (a_*t+b_)*exp(-c_*t)+d_;
		else
			return (a2_*t+b2_)*exp(-c2_*t)+d2_;
	}

	Real AbcdVolRatioCorrSigma::primitive(Real t, Real T, Real S, Real theta, bool TsecondState,bool SsecondState) const {
		
		double aStar,bStar,cStar,dStar;
		double aTilde,bTilde,cTilde,dTilde;

		if(!TsecondState && !SsecondState) { aStar=a_; bStar=b_; cStar=c_; dStar=d_; }
		if(TsecondState && SsecondState) { aStar=a2_; bStar=b2_; cStar=c2_; dStar=d2_; }
		if(!TsecondState && SsecondState) { aStar=a_; bStar=b_; cStar=c_; dStar=d_; aTilde=a2_; bTilde=b2_; cTilde=c2_; dTilde=d2_; }
		if(TsecondState && !SsecondState) { aStar=a2_; bStar=b2_; cStar=c2_; dStar=d2_; aTilde=a_; bTilde=b_; cTilde=c_; dTilde=d_; }

		if((!TsecondState && !SsecondState) || (TsecondState && SsecondState)) { // simple formula
			double alpha1=exp(-cStar*(T+S));
			double beta1=theta+2.0*cStar;
			double gamma1=aStar*aStar;
			double delta1=-(T+S)*aStar*aStar-2.0*aStar*bStar;
			double epsilon1=aStar*aStar*T*S+aStar*bStar*(T+S)+bStar*bStar;
			double i1=0.0;
			if(fabs(beta1)>TINYBETA)  {
				i1=alpha1*exp(beta1*t)/beta1 *(gamma1*(t*t-2.0*t/beta1+2.0/(beta1*beta1))+
												  delta1*(t-1.0/beta1)+
												  epsilon1);
			}
			else {
				i1=alpha1*(gamma1*1.0/3.0*t*t*t+delta1*0.5*t*t+epsilon1*t);
			}
			double alpha2=dStar*exp(-cStar*S);
			double beta2=theta+cStar;
			double gamma2=aStar*S+bStar;
			
			double i2=0.0;
			if(fabs(beta2)>TINYBETA) {
				i2=alpha2/beta2*exp(beta2*t)*(-aStar*t+aStar/beta2+gamma2);
			}
			else {
				i2=alpha2*(-0.5*aStar*t*t+gamma2*t);
			}
			double alpha3=dStar*exp(-cStar*T);
			double beta3=beta2;
			double gamma3=aStar*T+bStar;
			
			double i3=0.0;
			if(fabs(beta3)>TINYBETA) {
				i3=alpha3/beta3*exp(beta3*t)*(-aStar*t+aStar/beta3+gamma3);
			}
			else {
				i3=alpha3*(-0.5*aStar*t*t+gamma3*t);
			}
			
			double i4=0.0;
			if(fabs(theta)>TINYBETA) {
				i4=dStar*dStar/theta*exp(theta*t);
			}
			else {
				i4=dStar*dStar*t;
			}
			return i1+i2+i3+i4;
		}

		else  { // formula for mixed abcd states (aStar,... and aTilde...)
			double alpha1=exp(-cStar*T-cTilde*S);
			double beta1=theta+cStar+cTilde;
			double gamma1=aStar*aTilde;
			double delta1=-aStar*aTilde*(T+S)-aStar*bTilde-aTilde*bStar;
			double epsilon1=aStar*aTilde*T*S+aStar*bTilde*T+aTilde*bStar*S+bStar*bTilde;
			double i1=0.0;
			if(fabs(beta1)>TINYBETA)  {
				i1=alpha1*exp(beta1*t)/beta1 *(gamma1*(t*t-2.0*t/beta1+2.0/(beta1*beta1))+
												  delta1*(t-1.0/beta1)+
												  epsilon1);
			}
			else {
				i1=alpha1*(gamma1*1.0/3.0*t*t*t+delta1*0.5*t*t+epsilon1*t);
			}
			
			double alpha2=dTilde*exp(-cStar*T);
			double beta2=theta+cStar;
			double gamma2=aStar*T+bStar;
			double i2=0.0;
			if(fabs(beta2)>TINYBETA) {
				i2=alpha2/beta2*exp(beta2*t)*(-aStar*t+aStar/beta2+gamma2);
			}
			else {
				i2=alpha2*(-0.5*aStar*t*t+gamma2*t);
			}
			
			double alpha3=dStar*exp(-cTilde*S);
			double beta3=theta+cTilde;
			double gamma3=aTilde*S+bTilde;
			double i3=0.0;
			if(fabs(beta3)>TINYBETA) {
				i3=alpha3/beta3*exp(beta3*t)*(-aStar*t+aStar/beta3+gamma3);
			}
			else {
				i3=alpha3*(-0.5*aStar*t*t+gamma3*t);
			}
			
			double i4=0.0;
			if(fabs(theta)>TINYBETA) {
				i4=dStar*dTilde/theta*exp(theta*t);
			}
			else {
				i4=dStar*dTilde*t;
			}
			return i1+i2+i3+i4;
		}

	}


}
#include <oneFactorGauss.hpp>

using namespace std;

namespace QuantLib {

	OneFactorGauss::OneFactorGauss() {
		nd_ = NormalDistribution();
		cnd_ = CumulativeNormalDistribution();
		icn_ = InverseCumulativeNormal();
		gh_= new GaussHermiteIntegration(64);
		
	}

	boost::shared_ptr<KernelDensity> OneFactorGauss::empiricalPdDensity(long numberOfObligors, double pd, double correlation, int seed, long samples) {
		
		MersenneTwisterUniformRng mt(seed);
		vector<double> sam;
		double spd;

		for(int i=0;i<samples;i++) {
			spd=((double)sampleNumberOfDefaults(numberOfObligors,pd,correlation,mt)) / ((double)numberOfObligors);
			sam.push_back(spd);
		}
		//return estimator with default bandwidth, can be computed by user later
		return boost::shared_ptr<KernelDensity>(new KernelDensity(sam,0.05,0.01));

	}
			
	boost::shared_ptr<KernelDensity> OneFactorGauss::empiricalGordyVarDensity(long numberOfObligors, double pd, double quantile, double correlation, int seed, long samples) {
		MersenneTwisterUniformRng mt(seed);
		vector<double> sam;
		double spd,gvar;

		for(int i=0;i<samples;i++) {
			spd=((double)sampleNumberOfDefaults(numberOfObligors,pd,correlation,mt)) / ((double)numberOfObligors);
			if(spd>0.0&&spd<1.0) {
				gvar=spd>0.0?cnd_(pow(1.0-correlation*correlation,-0.5)*icn_(spd)+pow(correlation*correlation/(1.0-correlation*correlation),0.5)*icn_(quantile)) : 0.0;
			}
			else {
				gvar=spd==0.0?0.0:1.0;
			}
			sam.push_back(gvar);
		}
		//return estimator with default bandwidth, can be computed by user later
		return boost::shared_ptr<KernelDensity>(new KernelDensity(sam,0.3,0.1));
	}

	double OneFactorGauss::empiricalBootstrapVariance(long numberOfObligors, double pd, double correlation, int seed, long samples) {
		
		Array roots = gh_->x();
		Array weights = gh_->weights();
		
		double i1=0.0,i2=0.0;
		double icnPd=icn_(pd);
		double condPd;
		double u,df;

		//FILE* out=fopen("ASFR.log","a");
		for(int i=0;i<roots.size();i++) {
			u=roots[i]*sqrt(2.0);
			df=exp(-roots[i]*roots[i]);
			condPd=cnd_((icnPd-correlation*u)/sqrt(1.0-correlation*correlation));
			i1+=condPd*weights[i]*df;
			i2+=condPd*condPd*weights[i]*df;
		}
		i1/=sqrt(M_PI);
		i2/=sqrt(M_PI);
		//fclose(out);
		
		return (i1-i2)/((double)numberOfObligors)+i2-i1*i1;


		/*MersenneTwisterUniformRng mt(seed);

		IncrementalStatistics stat1;
		double spd,globalFactor,condPd,condSpr,defTime,rn;
		double icnPd=icn_(pd);
		int nod;
		for(int i=0;i<samples;i++) {
			//spd=((double)sampleNumberOfDefaults(numberOfObligors,pd,correlation,mt)) / ((double)numberOfObligors);
			// fast implementation
			globalFactor=icn_(mt.next().value);
			condPd=cnd_((icnPd-correlation*globalFactor)/sqrt(1.0-correlation*correlation));
			//condSpr=condPd<1.0 ? -log(1-condPd) : 1000.0;
			nod=0;
			for(int i=0;i<numberOfObligors;i++) {
				rn=mt.next().value;
				//defTime=condSpr>0.0 && rn<1.0 ? -log(1.0-mt.next().value)/condSpr : 100000.0;
				//if(defTime<1.0) nod++;
				if(rn<condPd) nod++;
			}
			spd=nod/((double)numberOfObligors);
			stat1.add(spd);
		}
		//return estimator with default bandwidth, can be computed by user later
		//fprintf(out,"mean=%f,var=%f\n",stat1.mean(),stat1.variance());
		//fclose(out);
		return stat1.variance();*/
	}

	boost::shared_ptr<KernelDensity> OneFactorGauss::convexityAdjGordyVarDensity(long numberOfObligors, double pd, double quantile, double correlation, bool useCorr, int seed, long samples) {
		MersenneTwisterUniformRng mt(seed);
		vector<double> sam;
		double spd,gvar,spd2,conv;
		boost::shared_ptr<KernelDensity> eg;

		double A=1.0/sqrt(1.0-correlation*correlation);
		double B=sqrt(correlation*correlation/(1.0-correlation*correlation))*icn_(quantile);
		double icnSpd,phi1,phi1p,phi2,phi2p,alpha,alphaP,alphaPP,der2;
		double m2a,m2b,m2c,m2Corr,bv2der;
		double h=1E-5; // for 2nd derivative computation
		
		for(int i=0;i<samples;i++) {
			spd=((double)sampleNumberOfDefaults(numberOfObligors,pd,correlation,mt)) / ((double)numberOfObligors);
			if(spd>0.0&&spd<1.0) {
				gvar=spd>0.0?cnd_(pow(1.0-correlation*correlation,-0.5)*icn_(spd)+pow(correlation*correlation/(1.0-correlation*correlation),0.5)*icn_(quantile)) : 0.0;
			}
			else {
				gvar=spd==0.0?0.0:1.0;
			}
			// compute convexity adjustment
			// estimate variance of pd estimator and 2nd derivative
			if(spd>h && spd<1.0) {
				m2a=empiricalBootstrapVariance(numberOfObligors,spd-h,correlation,0,0);
				m2b=empiricalBootstrapVariance(numberOfObligors,spd,correlation,0,0);
				m2c=empiricalBootstrapVariance(numberOfObligors,spd+h,correlation,0,0);
				if(useCorr) bv2der=(m2c-2.0*m2b+m2a)/(h*h);
				else bv2der=0.0;
				m2Corr=m2b-0.5*bv2der*m2b; // convexity correction for m2
				//fprintf(out,"%f;%f;%f\n",m2b,bv2der,m2Corr);
				// compute conv adj
				icnSpd=icn_(spd);
				phi1=exp(-0.5*icnSpd*icnSpd)/sqrt(2.0*M_PI);
				//if(phi1>0.0) {
					phi1p=-phi1*icnSpd;
					alpha=A*icnSpd+B;
					alphaP=A/phi1;
					alphaPP=-A*phi1p/(phi1*phi1*phi1);
					phi2=exp(-0.5*alpha*alpha)/sqrt(2.0*M_PI);	
					phi2p=-phi2*alpha;
					der2=phi2p*alphaP*alphaP+phi2*alphaPP;
					conv=0.5*m2Corr*der2;
				//}
				//else {
				//	conv=0.0;
				//}
			}
			else {
				conv=0.0;
			}
			sam.push_back(gvar-conv);
		}
		//return estimator with default bandwidth, can be computed by user later
		return boost::shared_ptr<KernelDensity>(new KernelDensity(sam,0.3,0.1));
	}


	long OneFactorGauss::sampleNumberOfDefaults(long numberOfObligors, double pd, double correlation, MersenneTwisterUniformRng& mt) {

		double globalFactor, condPd, condSpr, defTime, rn;
		long numberOfDefaults=0;
		
		globalFactor=icn_(mt.next().value);
		condPd=cnd_((icn_(pd)-correlation*globalFactor)/sqrt(1.0-correlation*correlation));
		condSpr=condPd<1.0 ? -log(1-condPd) : 1000.0;
		
		for(int i=0;i<numberOfObligors;i++) {
			rn=mt.next().value;
			defTime=condSpr>0.0 && rn<1.0 ? -log(1.0-mt.next().value)/condSpr : 100000.0;
			if(defTime<1.0) numberOfDefaults++;
		}

		return numberOfDefaults;

	}



			
}


	

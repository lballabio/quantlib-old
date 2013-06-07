#include <sabrModel.hpp>
#include <stdio.h>



namespace QuantLib {
	
	SabrModel::SabrModel(Real alpha, Real beta, Real nu, Real rho,double minStrike,double blackAccuracy,double minDistAtm) :
				blackAccuracy_(), minDistAtm_(minDistAtm), minStrike_(minStrike) {
					recalibrate(alpha,beta,nu,rho); // FIXME (the check really takes long when retrieving many values from the cube ...)
				}

	Real SabrModel::alpha() { return alpha_; }
	Real SabrModel::beta() { return beta_; }
	Real SabrModel::nu() { return nu_; }
	Real SabrModel::rho() { return rho_; }

	std::vector<Real> SabrModel::modelParameters() {
		std::vector<Real> res;
		res.push_back(alpha_);
		res.push_back(beta_);
		res.push_back(nu_);
		res.push_back(rho_);
		return res;
	}

	void SabrModel::recalibrate(Real alpha, Real beta, Real nu, Real rho) {
		QL_REQUIRE(alpha>0.0,"alpha (" << alpha << ") must be positve");
		QL_REQUIRE(beta>=0.0 && beta<=1.0,"beta (" << beta << ") must be in [0,1]");
		QL_REQUIRE(nu>=0.0,"nu (" << nu << ") must be geq 0");
		QL_REQUIRE(rho>-1.0 && rho <1.0,"rho (" << rho << ") must be in [-1,1]");
		recalibrateWithoutCheck(alpha,beta,nu,rho);
	}

	void SabrModel::recalibrateWithoutCheck(Real alpha, Real beta, Real nu, Real rho) {
		alpha_=alpha;
		beta_=beta;
		nu_=nu;
		rho_=rho;
	}

	Real SabrModel::calibrate(Real forward, Real maturity, std::vector<Real> strikes, std::vector<Real> blackVolas,
				bool alphaFixed, bool betaFixed, bool nuFixed, bool rhoFixed, bool vegaWeighted,
				double acceptRmse, int haltonIterations,
				const boost::shared_ptr<EndCriteria>& endCriteria,
                const boost::shared_ptr<OptimizationMethod>& optMethod) {
			
			QL_REQUIRE(strikes.size()==blackVolas.size(), "Number of given strikes (" << strikes.size() << ") is not equal to number of given implied vols (" << blackVolas.size() << ")");
			QL_REQUIRE(forward>0.0,"SabrModel: forward (" << forward << ") must be positive");
			QL_REQUIRE(maturity>0.0,"SabrModel: maturity (" << maturity << ") must be positive");
			
			for(int i=0;i<strikes.size();i++) {
				if(strikes[i]<0.0) QL_FAIL("Strike " << i << " (" << strikes[i] << ") must be positive");
				if(blackVolas[i]<0.0) QL_FAIL("Implied Black Vol " << i << " (" << blackVolas[i] << ") must be positive");
			}

			boost::shared_ptr<EndCriteria> endCriteria_=endCriteria;
			boost::shared_ptr<OptimizationMethod> optMethod_=optMethod;

			if (!optMethod_) {
                //optMethod_ = boost::shared_ptr<OptimizationMethod>(new
                //    LevenbergMarquardt(1e-8, 1e-8, 1e-8));
                optMethod_ = boost::shared_ptr<OptimizationMethod>(new
                    Simplex(0.01));
			}
            if (!endCriteria_) {
                    endCriteria_ = boost::shared_ptr<EndCriteria>(new
                        EndCriteria(60000, 100, 1e-8, 1e-8, 1e-8));
             }

			SabrModelCostFunction costfct(this,forward,maturity,strikes,blackVolas,alphaFixed,betaFixed,nuFixed,rhoFixed,vegaWeighted);
			NoConstraint constraint;

			int id=0;
			if(!alphaFixed) { id++; }
			if(!betaFixed) { id++; }
			if(!nuFixed) { id++; }
			if(!rhoFixed) { id++; }
			Array guess(id);
			Array add(id), mult(id); // transform halton values in [0,1] to start values for parameters
			id=0;
			if(!alphaFixed) { guess[id]=alpha_; add[id]=0.0; mult[id]=1.0; id++; }
			if(!betaFixed) { guess[id]=beta_; add[id]=0.0; mult[id]=1.0; id++; }
			if(!nuFixed) { guess[id]=nu_; add[id]=0.0; mult[id]=5.0; id++; }
			if(!rhoFixed) { guess[id]=rho_; add[id]=-1.0; mult[id]=2.0; id++; }

			Array x;
			double y;

			double bestAlpha=0.5, bestBeta=0.5, bestNu=0.5, bestRho=0.5; // if no solution
			double minError=1.0E10;
			bool foundSolution=false;
			
			EndCriteria::Type ret;

			// try calibration with start value
			Problem prblm(costfct,constraint,guess);
			ret=optMethod_->minimize(prblm,*endCriteria_);
			if(ret!=EndCriteria::None &&
			   ret!=EndCriteria::MaxIterations &&
			   ret!=EndCriteria::Unknown) {
				   x=prblm.currentValue();
				   y=costfct.value(x);		// make sure, model is set to optimal value, y is error
   				   bestAlpha=alpha_;
				   bestBeta=beta_;
				   bestNu=nu_;
				   bestRho=rho_;
				   minError=y;
				   foundSolution=true;
			}

			// if accept level is not reached with given start guess, try halton sequence as guesses
			if(minError>acceptRmse) {
				HaltonRsg hal(id,0); // dimension = id, seed = 0
				vector<double> params;
				int tr=0;
				while(minError>acceptRmse && tr<haltonIterations) {
					params=hal.nextSequence().value;
					for(int i=0;i<id;i++) {
						guess[i]=params[i]*mult[i]+add[i];
					}
					Problem prblm(costfct,constraint,guess);
					ret=optMethod_->minimize(prblm,*endCriteria_);
					if(ret!=EndCriteria::None &&
						ret!=EndCriteria::MaxIterations &&
						ret!=EndCriteria::Unknown) {
							x=prblm.currentValue();
							y=costfct.value(x); // make sure, model is set to optimal value, y is error
							if(y<minError) {
								bestAlpha=alpha_;
								bestBeta=beta_;
								bestNu=nu_;
								bestRho=rho_;
								minError=y;
								foundSolution=true;
							}
					}
					tr++;
				} 
			}

			recalibrate(bestAlpha,bestBeta,bestNu,bestRho);

			if(!foundSolution) return -1.0;
			else return minError;

	}
	
	Real SabrModel::optionPrice(Real forward,Real strike,Real maturity,Option::Type type) {

		if(strike<minStrike_) strike=minStrike_;
		
		QL_REQUIRE(forward>=0.0001 && forward<=0.5,"Forward (" << forward << ") is not reasonable");
		//QL_REQUIRE(strike>=0.0001 && strike<=10.0,"Strike (" << strike << ") is not reasonable");
		QL_REQUIRE(maturity>0.0 && maturity<=100.0,"Maturity (" << maturity << ") is not reasonable");
		
		// compute price out of impl vola (classic approach)
		double stdDev=impliedVola(forward,strike,maturity)*sqrt(maturity);
		if(stdDev<=0.0) stdDev=0.0001;
		double res=blackFormula(type,strike,forward,stdDev);
		return res;

		// compute price by paper local time for sabr
		/*	double z=(pow(forward,1.0-beta_)-pow(strike,1.0-beta_))/(alpha_*(1.0-beta_));
			double z0=(pow(forward,1.0-beta_)-pow(strike,1.0-beta_))/(alpha_*(1.0-beta_));
			double b1=beta_/(alpha_*(1.0-beta_)*z0+pow(strike,1.0-beta_));
			double x=1.0/nu_*log((-rho_+nu_*z+sqrt(1.0-2.0*nu_*rho_*z+nu_*nu_*z*z))/(1.0-rho_));
			//double theta=0.25*rho_*nu_*alpha_*b1*z*z+log((alpha_*pow(forward*strike,beta_/2.0)*z)/(forward-strike))
			//	+log(x/z*pow(1.0-2.0*nu_*rho_*z+nu_*nu_*z*z,0.25));
			double k=0.125*(
				(alpha_*alpha_*(beta_-2.0)*beta_*pow(strike,2.0*beta_))/pow(strike-alpha_*(beta_-1.0)*pow(strike,beta_)*z0,2.0)+
				6.0*alpha_*beta_*pow(strike,beta_)*nu_*rho_/(strike-alpha_*(beta_-1.0)*pow(strike,beta_)*z0)+
				nu_*nu_*(2.0-3.0*rho_*rho_+2.0*nu_*rho_*z0-nu_*nu_*z0*z0)/(1.0-2.0*nu_*rho_*z0+nu_*nu_*z0*z0));
			complex<double> sqrtk;
			if(k>=0) sqrtk=complex<double>(sqrt(k),0.0);
			else sqrtk=complex<double>(0.0,sqrt(-k));
			complex<double> etp = x/sqrt(2.0*maturity)+complex<double>(0.0,1.0)*sqrtk*sqrt(maturity);
			complex<double> etm = -x/sqrt(2.0*maturity)+complex<double>(0.0,1.0)*sqrtk*sqrt(maturity);
			complex<double> et0 = complex<double>(0.0,1.0)*sqrt(2.0)*x*sqrtk;
			complex<double> etd = complex<double>(0.0,2.0)*sqrtk;
			complex<double> in=sqrt(M_PI)/etd*(
				exp(et0)*(erfc(etp)-1.0)+exp(-et0)*(erfc(etm)+1.0)
				);
			double res=max(forward-strike,0.0)+exp(0.25*rho_*nu_*alpha_*b1*z*z)*
				alpha_*pow(forward*strike,beta_/2.0)*pow(1.0-2.0*nu_*rho_*z+nu_*nu_*z*z,0.25) /
				(2.0*sqrt(2.0*M_PI))*in.real();
			//double res=max(forward-strike,0.0)+(forward-strike)/(2.0*x*sqrt(2.0*M_PI))*exp(theta)*in.real();
			return res;
		*/
	}

	Real SabrModel::impliedVola(Real forward,Real strike,Real maturity) {

		if(strike<minStrike_) strike=minStrike_;

		// classic approach (Hagans paper Managing Smile Risk)
		/*double lg=log(forward/strike);
		double p=forward*strike;
		double z=nu_/alpha_*pow(p,(1.0-beta_)/2.0)*lg;
		double x=log((sqrt(1.0-2.0*rho_*z+z*z)+z-rho_)/(1.0-rho_));
		double e1=(1.0-beta_)*lg;
		double e2=e1*e1;
		double e4=e2*e2;
		double nonAtm=fabs(lg)<MINDISTATM ? 1.0 : (z/x)/(1.0+e2/24.0+e4/1920.0);
		double implVola=alpha_/(pow(p,(1.0-beta_)/2.0))*(1.0+
			maturity*((1.0-beta_)*(1.0-beta_)*alpha_*alpha_/(24.0*pow(p,(1.0-beta_)))+
						0.25*(rho_*beta_*nu_*alpha_)/pow(p,(1.0-beta_)/2.0)+
						(2.0-3.0*rho_*rho_)*nu_*nu_/24.0))*nonAtm;
		return implVola;*/

		// fine tune your smile
		const Real oneMinusBeta = 1.0-beta_;
		const Real x=log(forward/strike);
		Real z, ksi, I_0, I_1, nominator, denominator, s1, s2, s3;
		
		// The formulas are taken from table one of the paper fine tune your smile
		if(fabs(log(forward/strike))<minDistAtm_){ // close(log(forward/strike),0.0)
			I_0=alpha_*pow(strike,-oneMinusBeta);
		}
		else {
			if(close(nu_,0.0)){
				I_0=x*alpha_*oneMinusBeta/(pow(forward,oneMinusBeta)-pow(strike,oneMinusBeta));
			}
			else {
				if(close(beta_,1.0)){
					z=nu_*x/alpha_;
					I_0=nu_*x/log((z-rho_+sqrt(1+z*z-2*z*rho_))/(1-rho_));
				}
				else {
					// case log(forward/strike)>0 & nu>0 & beta <1
					z=nu_/alpha_/oneMinusBeta*(pow(forward,oneMinusBeta)-pow(strike,oneMinusBeta));
					ksi=(nu_/alpha_*(forward-strike))/pow((strike*forward),beta_/2);
					nominator=nu_*x;
					denominator=log((z-rho_+sqrt(1+z*z-2*z*rho_))/(1-rho_));
					I_0=nominator/denominator;
				}
			}
		}
		s1=pow(strike*forward,-oneMinusBeta)/24*alpha_*alpha_*(-oneMinusBeta)*(-oneMinusBeta);
		s2=pow(strike*forward,-oneMinusBeta/2)/4*alpha_*beta_*nu_*rho_;
		s3=nu_*nu_/24*(2-3*rho_*rho_);
		I_1=s1+s2+s3;

		return I_0*(1+I_1*maturity);

		// convert price to vola
		/*Real res=optionPrice(forward,strike,maturity);
		//Real res=mcPrice(forward,strike,maturity,24,5000);
		Real implVola=0.0;
		try {
			implVola=blackFormulaImpliedStdDev(Option::Call,strike,forward,res,1.0,0.0,alpha_,BLACKACCURACY,100)/sqrt(maturity);
		} catch(QuantLib::Error er) {
			implVola = 100000.0;
		}
		return implVola;*/
		
	}

	/*Real SabrModel::mcPrice(Real forward, Real strike, Real maturity,Size timestepsPerYear, Size numberOfPaths) {
		IncrementalStatistics stat;
		Size steps=(int)(timestepsPerYear*maturity);
		//boost::shared_ptr<MTBrownianGenerator> mtBg_(new MTBrownianGenerator(2,steps));
		boost::shared_ptr<SobolBrownianGenerator> mtBg_(new SobolBrownianGenerator(2,steps,SobolBrownianGenerator::Factors));
		std::vector<double> n(2);
		double c1,c2,a,s,weight;
		double h=maturity/(double)steps;
		//FILE* out=fopen("mc.log","a");
		for(int i=0;i<numberOfPaths;i++) {
			a=alpha_;
			s=forward;
			mtBg_->nextPath();
			weight=1.0;
			for(int d=0;d<steps;d++) {
				//weight*=sobolBg_->nextStep(n);
				weight*=mtBg_->nextStep(n); 
				c1=n[0];
				c2=c1*rho_+n[1]*sqrt(1.0-rho_*rho_);
				if(s<0.0001) s=0.0001;
				if(a<0.0001) a=0.0001;
				s+=a*pow(s,beta_)*c1*sqrt(h);
				a+=nu_*a*c2*sqrt(h);
				//a*=exp(nu_*c2*sqrt(h)-0.5*nu_*nu_*h);
			}
			stat.add(weight*max(s-strike,0.0));
			//fprintf(out,"%f;%f;%f\n",s,strike,max(s-strike,0.0));
		}
		//fclose(out);
		return stat.mean();
	}*/


	// Helper classes

	SabrModelCostFunction::SabrModelCostFunction(SabrModel* model,Real forward,Real maturity,
		std::vector<Real> strikes,std::vector<Real> blackVolas,
		bool alphaFixed,bool betaFixed,bool nuFixed,bool rhoFixed,bool vegaWeighted):
		model_(model), forward_(forward), maturity_(maturity), strikes_(strikes), blackVolas_(blackVolas),
		alphaFixed_(alphaFixed), betaFixed_(betaFixed), nuFixed_(nuFixed), rhoFixed_(rhoFixed), 
		vegaWeighted_(vegaWeighted) {

			QL_REQUIRE(strikes_.size()==blackVolas_.size(),"Number of strikes (" << strikes_.size() << ") must be equal to number of black volas (" << blackVolas_.size() << ")"); 
			n_=strikes_.size();
			weights_=vector<double>(n_,1.0/sqrt((double)n_));
			if(vegaWeighted) {
				double sumW=0.0;
				for(int i=0;i<n_;i++) {
					weights_[i] = blackFormulaVolDerivative(strikes_[i],forward_,blackVolas_[i]*sqrt(maturity_),maturity);
					sumW+=weights_[i];
				}
				for(int i=0;i<blackVolas_.size();i++) {
					weights_[i] = sqrt(weights_[i]/sumW);
				}
			}

		}

	Real SabrModelCostFunction::value(const QuantLib::Array& x) const {
		Array res = values(x);
		double sum=0.0;
		for(int i=0;i<n_;i++) {
			sum+=res[i]*res[i];
		}
		return sqrt(sum);
	}

	Disposable<Array> SabrModelCostFunction::values(const QuantLib::Array& x) const {
		// set parameters
		Array result(n_);
		Size id=0;
		double alpha=model_->alpha();
		double beta=model_->beta();
		double nu=model_->nu();
		double rho=model_->rho();
		if(!alphaFixed_) { alpha=x[id]; id++; }
		if(!betaFixed_) { beta=x[id]; id++; }
		if(!nuFixed_) { nu=x[id]; id++; }
		if(!rhoFixed_) { rho=x[id]; id++; }
		// admissability
		double penalty=1.0;
		if(rho < -0.999) { rho = -0.999; penalty = 1000.0; }
		if(rho > 0.999) { rho = 0.999; penalty = 1000.0; }
		if(beta < 0.0001) { beta = 0.0001; penalty = 1000.0; }
		if(beta > 1.0) { beta = 1.0; penalty = 1000.0; }
		if(alpha < 0.0001) { alpha = 0.0001; penalty = 1000.0; }
		//if(alpha > 1.000) { alpha = 1.0000; penalty = 1000.0; }
		if(nu < 0.0001) { nu = 0.0001; penalty = 1000.0; }
		//if(nu > 5.0000) { nu = 5.0000; penalty = 1000.0; }
		// recalibrate model and compute error
		model_->recalibrateWithoutCheck(alpha,beta,nu,rho);
		for(int i=0;i<n_;i++) {
			double implVola=model_->impliedVola(forward_,strikes_[i],maturity_);
			double blackVola=blackVolas_[i];
			result[i] = weights_[i]*(implVola-blackVola) * penalty;
		}
		return result;
	}


}
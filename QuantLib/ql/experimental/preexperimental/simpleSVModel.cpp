#include <simpleSVModel.hpp>
#include <stdio.h>



namespace QuantLib {
	
	// for numerical computation of fourier integral: gsl integration
	/*bool isGslReady=false;
	gsl_integration_qawo_table *qawo_table_cosine; 
	gsl_integration_workspace *qawo_ws, *qawf_ws;
	gsl_function simpleSVModelIntegrandFct;
	SimpleSVModelIntegrand *simpleSVModelInt;
	Size qawoSizeInit,qawfSizeInit;

	double simpleSVMInt(double t, void* params) { // outer integrand im part
		return simpleSVModelInt->valueWithoutCosineTerm(t);
	}

	void gslQawoHandler(const char * reason,const char * file,int line, int gsl_errno) {
		QL_FAIL("GSL Qawo Handler: " << reason << " file " << file << " line " << line << " errno " << errno);
	}

	int initGslQawo(Size qawoSize, Size qawfSize) { // init gsl qawo enviroment
		//FILE* out=fopen("cost.log","a");
		//fprintf(out,"INIT GSL\n");
		//fclose(out);
		if(!isGslReady || (qawoSize!=qawoSizeInit || qawfSize!=qawfSizeInit)) {
			if(isGslReady) freeGslQawo();
			qawo_table_cosine = gsl_integration_qawo_table_alloc(1.0, 1.0, GSL_INTEG_COSINE, qawoSize);
			qawo_ws=gsl_integration_workspace_alloc(qawoSize);
			qawf_ws=gsl_integration_workspace_alloc(qawfSize);
			simpleSVModelIntegrandFct.function = &simpleSVMInt;
			isGslReady=true;
			gsl_set_error_handler(&gslQawoHandler); 
		}
		return 0;
	}

	int freeGslQawo() {
		//FILE* out=fopen("cost.log","a");
		//fprintf(out,"FREE GSL\n");
		//fclose(out);
		if(isGslReady) {
			gsl_integration_workspace_free(qawf_ws);
			gsl_integration_workspace_free(qawo_ws);
			gsl_integration_qawo_table_free(qawo_table_cosine);
			isGslReady=false;
		}
		return 0;
	}*/
	// ---- end of gsl integration

	SimpleSVModel::SimpleSVModel(Real lambda, Real b, Real theta, Real eta, Real omega, Real varianceStartValue) :
				lambda_(lambda), b_(b), theta_(theta), eta_(eta), omega_(omega), varianceStartValue_(varianceStartValue) {
				}

	Real SimpleSVModel::lambda() { return lambda_; }
	Real SimpleSVModel::b() { return b_; }
	Real SimpleSVModel::theta() { return theta_; }
	Real SimpleSVModel::eta() { return eta_; }
	Real SimpleSVModel::omega() { return omega_; }

	std::vector<Real> SimpleSVModel::modelParameters() {
		std::vector<Real> res;
		res.push_back(lambda_);
		res.push_back(b_);
		res.push_back(theta_);
		res.push_back(eta_);
		res.push_back(varianceStartValue_);
		res.push_back(omega_);
		return res;
	}

	std::vector<Real> SimpleSVModel::modelImpliedVols() { return modelVols_; }
	std::vector<Real> SimpleSVModel::marketImpliedVols() { return marketVols_; }

	void SimpleSVModel::recalibrate(Real lambda, Real b, Real theta, Real eta, Real omega) {
		lambda_=lambda;
		b_=b;
		theta_=theta;
		eta_=eta;
		omega_=omega;
	}

	Real SimpleSVModel::calibrate(Real forward, Real maturity, std::vector<Real> strikes, std::vector<Real> blackVolas,
		bool lambdaFixed, bool bFixed, bool thetaFixed, bool etaFixed, bool useImpliedVols, bool dontCalibrate) {
			
			QL_REQUIRE(strikes.size()==blackVolas.size(), "Number of given strikes (" << strikes.size() << ") is not equal to number of given implied vols (" << blackVolas.size() << ")");
			QL_REQUIRE(forward>=0.0001 && forward<=0.5,"Forward (" << forward << ") is not reasonable");
			QL_REQUIRE(maturity>0.0 && maturity<=100.0,"Maturity (" << maturity << ") is not reasonable");
			bool areParametersOk=true;			
			for(int i=0;i<strikes.size();i++) {
				if(strikes[i]<0.0001 || strikes[i]>0.5) areParametersOk=false;
				if(blackVolas[i]<0.01 || blackVolas[i]>5.0) areParametersOk=false;
			}
			QL_REQUIRE(areParametersOk,"Given strikes or implied vols are not reasonable.");

			//FILE* log;
			//log=fopen("calibration.log","a");
			//fprintf(log,"calibration started: forward=%1.12f, maturity=%f\n",forward,maturity);
			//fclose(log);
			
			std::vector<Real> result(1+2*strikes.size());

			SimpleSVModelCostFunction costfct(this,forward,maturity,strikes,blackVolas,lambdaFixed,bFixed,thetaFixed,etaFixed,useImpliedVols);
			SimpleSVModelConstraint constraint;

			int id=0;
			if(!lambdaFixed) { id++; }
			if(!bFixed) { id++; }
			if(!thetaFixed) { id++; }
			if(!etaFixed) { id++; }
			Array guess(id);
			id=0;
			if(!lambdaFixed) { guess[id]=lambda_; id++; }
			if(!bFixed) { guess[id]=b_; id++; }
			if(!thetaFixed) { guess[id]=theta_; id++; }
			if(!etaFixed) { guess[id]=eta_; id++; }

			Problem prblm(costfct,constraint,guess);
			EndCriteria ec(1000, 500, OPTACCURACY, OPTACCURACY, OPTACCURACY);
			EndCriteria::Type ret;
			if(!dontCalibrate) {
				//Simplex method(0.05);
				LevenbergMarquardt method;
				//SimulatedAnnealing method(0.1,0,50,500,2.5,0.2);
				ret=method.minimize(prblm,ec);
			}

			Array x=prblm.currentValue();
			double y=costfct.value(x);//prblm.functionValue(); to make sure, the model is set to optimal value

			// copy market and model vols to internal vectors, so that they can be returned by memthods impliedMarketVolas, impliedModelVolas
			marketVols_=std::vector<double>(strikes.size());
			modelVols_=std::vector<double>(strikes.size());
			for(int i=0;i<strikes.size();i++) {
				marketVols_[i] = blackVolas[i];
				modelVols_[i] = impliedVola(forward,strikes[i],maturity);
			}

			if(ret==EndCriteria::None) QL_FAIL("Optimizer returns status none");
			if(ret==EndCriteria::MaxIterations) QL_FAIL("Optimizer returns status max iterations");
			if(ret==EndCriteria::Unknown) QL_FAIL("Optimizer returns status unknown");

			//log=fopen("calibration.log","a");
			//fprintf(log,"done");
			//fclose(log);
			
			return y;
	}
	
	Real SimpleSVModel::callOptionPrice(Real forward,Real strike,Real maturity/*, Real upperBound*/) {
		
		QL_REQUIRE(forward>=0.0001 && forward<=0.5,"Forward (" << forward << ") is not reasonable");
		QL_REQUIRE(strike>=0.0001 && strike<=0.6,"Forward (" << forward << ") is not reasonable");
		QL_REQUIRE(maturity>0.0 && maturity<=100.0,"Maturity (" << maturity << ") is not reasonable");
		
		/* compute parameters for transformed model (in Lewis notation)
		   dX = sqrt(V*) X dB
		   dV* = (omega* - theta* V*) dt + eta* sqrt(V*) dW
		   dB dW = 0 */

		Real omegaStar = lambda_*lambda_*b_*b_*theta_*omega_;
		Real thetaStar = theta_;
		Real etaStar = lambda_*b_*eta_;
		Real varianceStarStartValue = lambda_*lambda_*b_*b_*varianceStartValue_;
		Real forwardStar = lambda_*(b_*forward+(1.0-b_)*forward);
		Real strikeStar = (1.0-b_)*lambda_*forward+strike*b_*lambda_;
	
		// compute derived values (in Lewis notation) 

		Real t = 0.5*etaStar*etaStar*maturity;
		Real omegaTilde = 2.0 / (etaStar*etaStar) * omegaStar;
		Real thetaHat = 2.0 / (etaStar*etaStar) * thetaStar;
		
		// calculate integral 
		
		//initGslQawo(qawoSize,qawfSize); // to be sure that gsl is ready
		SimpleSVModelIntegrand integrand(omegaStar,thetaStar,etaStar,varianceStarStartValue,forwardStar,strikeStar,maturity,t,omegaTilde,thetaHat);
		double integralAcc = FOURIERACCURACY * 2.0 * M_PI * lambda_ * b_ / strikeStar;
		integralAcc = std::max(MINFOURIERACC,integralAcc);
		Real X=log(forwardStar/strikeStar);
		Real period=fabs(X)<0.1 ? 0.1 : fabs(X);
		//simpleSVModelInt = &integrand;
		Real in=0.0,err=0.0;
		
		try{
			//gsl_integration_qawo_table_set(qawo_table_cosine,X,FOURIERBOUND,GSL_INTEG_COSINE);
			//if(!isGslReady) QL_FAIL("GSL Integration is not initialized, call initGslQawo first");
			//int rc=gsl_integration_qawf(&simpleSVModelIntegrandFct,0.0,integralAcc,qawfSize,qawf_ws,qawo_ws,qawo_table_cosine,&in,&err);
			//SimpsonIntegral gl(integralAcc,1000);
			GaussLobattoIntegral gl(MAXGLITERATIONS,integralAcc);
			Real length=2.0*M_PI*period*(double)((int)(FOURIERBOUND1/(2.0*M_PI*period)));
			Real partIn,x=0.0;
			Size it=0;
			do {
				partIn=gl(integrand,x,x+length);
				in+=partIn;
				x+=length;
				it++;
			} while(partIn>integralAcc/10.0 && it<MAXFOURIERPARTS);
			//in=gl(integrand,0.0,FOURIERBOUND1);
			//in=gl(integrand,0.0,1.0);
			//int rc=gsl_integration_qawo(&simpleSVModelIntegrandFct,0.0,integralAcc,0.0,MAXINTEGRATIONSIZE2,qawo_ws,qawo_table_cosine,&in,&err);
			//if(rc) QL_FAIL("qawf integration failed with rc " << rc);
		} catch(QuantLib::Error er) {
			/*try {
				GaussLobattoIntegral gl(MAXGLITERATIONS,1.0);
				in=gl(integrand,0.0,FOURIERBOUND);
			} catch(QuantLib::Error er) {
				QL_FAIL("QL threw an exception: (desired accuracy: " << integralAcc << ", error=" << err << ") " << er.what());
			} catch(std::exception ex) {
			QL_FAIL("An non-QL exception is thrown:" << ex.what());
			}*/
			QL_FAIL("QL threw an exception: (desired accuracy: " << integralAcc << ", error=" << err << ") " << er.what());
		} catch(std::exception ex) {
			QL_FAIL("An non-QL exception is thrown:" << ex.what());
		}

		in*=2.0;//*M_PI/2.0; // trafo!!!
		
		// calculate result 
		Real resStar = forwardStar-strikeStar/(2.0*M_PI)*in;
		Real res = resStar/(lambda_*b_);

		return res;
	}

	Real SimpleSVModel::impliedVola(Real forward,Real strike,Real maturity/*,Real upperBound*/) {

		Real res=callOptionPrice(forward,strike,maturity);
		Real implVola=0.0;
		try {
			implVola=blackFormulaImpliedStdDev(Option::Call,strike,forward,res,1.0,0.0,lambda_,BLACKACCURACY,100)/sqrt(maturity);
		} catch(QuantLib::Error er) {
			implVola = 100000.0;
		}

		return implVola;
	}

	Real SimpleSVModel::mcPrice(Real forward, Real strike, Real maturity,Size timestepsPerYear, Size numberOfPaths) {
		IncrementalStatistics stat;
		Size steps=(int)(timestepsPerYear*maturity);
		boost::shared_ptr<SobolBrownianGenerator> mtBg_(new SobolBrownianGenerator(2,steps,SobolBrownianGenerator::Factors));
		//boost::shared_ptr<MTBrownianGenerator> mtBg_(new MTBrownianGenerator(2,steps));
		std::vector<double> n(2);
		double h=maturity/(double)steps;
		for(int i=0;i<numberOfPaths;i++) {
			double v = varianceStartValue_;
			double s = forward;
			mtBg_->nextPath();
			//sobolBg_->nextPath();
			double weight=1.0;
			for(int d=0;d<steps;d++) {
				//weight*=sobolBg_->nextStep(n);
				weight*=mtBg_->nextStep(n); 
				// log euler scheme with exact solution for stochvol process
				double vv= v>0.0 ? v : 0.0;
				//s*=exp( (b_*s+(1.0-b_)*forward)/s * sqrt(vv) * lambda_ * (sqrt(h)*n[0] -0.5*(b_*s+(1.0-b_)*forward)/s*sqrt(vv)*lambda_*h) );
				s=(s+(1-b_)*forward/b_)*exp(b_*lambda_*sqrt(vv)*sqrt(h)*n[0]-0.5*b_*b_*lambda_*lambda_*vv*h)-(1.0-b_)/b_*forward;
				//s=s+(b_*s+(1.0-b_)*forward)*sqrt(vv)*lambda_*sqrt(h)*n[0]; // naive evolution of asset process
				v=omega_+(v-omega_)*exp(-theta_*h)+n[1]*eta_*sqrt(vv)*sqrt(0.5/theta_*(1.0-exp(-2.0*theta_*h)));
				//v=v+h*theta_*(omega_-v)+eta_*sqrt(vv)*n[1]*sqrt(h); // naive evolution of variance process
				if(s==0.0) s=0.0001;
			}
			double payoff=std::max<double>(s-strike,0);
			stat.add(payoff,weight);
		}
		double price=stat.mean();
		//double err=stat.errorEstimate(); //printf("((errest2=%f))",err*2.0);
		return price;
	}


	// Helper classes

	SimpleSVModelIntegrand::SimpleSVModelIntegrand(Real omega,Real theta,Real eta,Real varianceStartValue,
		Real forward,Real strike,Real maturity,Real t,Real omegaTilde,Real thetaHat):
			omega_(omega),theta_(theta),eta_(eta),varianceStartValue_(varianceStartValue),forward_(forward),strike_(strike),
				maturity_(maturity),t_(t),omegaTilde_(omegaTilde),thetaHat_(thetaHat) {
	}

	Real SimpleSVModelIntegrand::operator()(Real u) const  {
		double s=u;
		//double s=-log(1.0-u); // transformation!
		Real ck=(s*s+0.25)/2.0;
		Real c=ck*2.0/(eta_*eta_);
		Real d=sqrt(thetaHat_*thetaHat_+4.0*c);
		Real g=0.5*(thetaHat_+d);
		Real h=(thetaHat_+d)/(thetaHat_-d);
		//Real f1=omegaTilde_*(t_*g-log((1-h*exp(d*t_))/(1.0-h)));
		//Real f2=g*((1.0-exp(d*t_))/(1.0-h*exp(d*t_)));
		Real f1,f2,m,dt0;
		dt0=d*t_;
		//if(d*t_>100.0) dt0=100.0;
		//else dt0=d*t_;
		m=1.0-h*exp(dt0);
		if(m<=0.0 || h==1.0) {
			f1=-500.0; f2=-500.0;
		}
		else {
			f1=omegaTilde_*(t_*g-log(m/(1.0-h)));
			f2=g*(1.0-exp(dt0))/m;
		}
		Real H=exp(f1+f2*varianceStartValue_);
		Real X=log(forward_/strike_);
		Real R=cos(s*X)*exp(0.5*X)/(s*s+0.25);
		//Real tc=1.0;
		//Real tc=(1.0+s*s)*(1.0+s*s)/(2.0*s); // transformation!
		//Real tc=exp(-s);
		//FILE* out=fopen("integrand.csv","a");
		//fprintf(out,"%f;%f;%f;%f;%f;%f;%f;%f;%f;%f;%f;%f;%f\n",u,ck,c,d,g,h,m,f1,f2,H,X,R,R*H);
		//fclose(out);
		return R*H;

		/*double s=s0/factor_;
		Real ck=(s*s+0.25)/2.0;
		Real c=ck*2.0/(eta_*eta_);
		Real d=sqrt(thetaHat_*thetaHat_+4.0*c);
		Real g=0.5*(thetaHat_+d);
		Real h=(thetaHat_+d)/(thetaHat_-d);
		Real f1=omegaTilde_*(t_*g-log((1-h*exp(d*t_))/(1.0-h)));
		Real f2=g*((1.0-exp(d*t_))/(1.0-h*exp(d*t_)));
		Real H=exp(f1+f2*varianceStartValue_);
		Real X=log(forward_/strike_);
		Real R=cos(s*X)*exp(0.5*X)/(s*s+0.25);
		return R*H;*/
	}

	Real SimpleSVModelIntegrand::valueWithoutCosineTerm(Real s) const  {
		Real ck=(s*s+0.25)/2.0;
		Real c=ck*2.0/(eta_*eta_);
		Real d=sqrt(thetaHat_*thetaHat_+4.0*c);
		Real g=0.5*(thetaHat_+d);
		Real h=(thetaHat_+d)/(thetaHat_-d);
		Real f1=omegaTilde_*(t_*g-log((1-h*exp(d*t_))/(1.0-h)));
		Real f2=g*((1.0-exp(d*t_))/(1.0-h*exp(d*t_)));
		Real H=exp(f1+f2*varianceStartValue_);
		Real X=log(forward_/strike_);
		Real R=exp(0.5*X)/(s*s+0.25);
		return R*H;
	}

	SimpleSVModelCostFunction::SimpleSVModelCostFunction(SimpleSVModel* model,Real forward,Real maturity,
		std::vector<Real> strikes,std::vector<Real> blackVolas,bool lambdaFixed,bool bFixed,bool thetaFixed,bool etaFixed,bool useImpliedVols):
		model_(model), forward_(forward), maturity_(maturity), strikes_(strikes), blackVolas_(blackVolas),
		lambdaFixed_(lambdaFixed), bFixed_(bFixed), thetaFixed_(thetaFixed), etaFixed_(etaFixed), useImpliedVols_(useImpliedVols) {
			QL_REQUIRE(strikes_.size()==blackVolas_.size(),"Number of strikes (" << strikes_.size() << ") must be equal to number of black volas (" << blackVolas_.size() << ")"); 
			n_=strikes_.size();
			if(!useImpliedVols) { // convert black vols to prices
				blackPrices_=std::vector<Real>(blackVolas_.size());
				for(int i=0;i<blackVolas_.size();i++) {
					blackPrices_[i] = blackFormula(Option::Call,strikes_[i],forward_,blackVolas_[i]*sqrt(maturity_),1.0,0.0);
				}
			}
	}

	Real SimpleSVModelCostFunction::value(const QuantLib::Array& x) const {
		Array res = values(x);
		double sum=0.0;
		for(int i=0;i<n_;i++) {
			sum+=res[i]*res[i];
		}
		return sqrt(sum/n_);
	}

	Disposable<Array> SimpleSVModelCostFunction::values(const QuantLib::Array& x) const {
		// set parameters
		Array result(n_);
		Size id=0;
		double lambda=model_->lambda();
		double b=model_->b();
		double theta=model_->theta();
		double eta=model_->eta();
		double omega=model_->omega();
		if(!lambdaFixed_) { lambda=x[id]; id++; }
		if(!bFixed_) { b=x[id]; id++; }
		if(!thetaFixed_) { theta=x[id]; id++; }
		if(!etaFixed_) {eta=x[id]; id++; }
		// admissability
		double penalty=1.0;
		if(lambda < 0.0001) { lambda = 0.0001; penalty = 1000.0; }
		if(lambda > 1.000) { lambda = 1.0000; penalty = 1000.0; }
		if(b < 0.0001) { b = 0.0001; penalty = 1000.0; }
		if(b > 1.0) { b = 1.0; penalty = 1000.0; }
		if(theta < 0.0001) { theta = 0.0001; penalty = 1000.0; }
		if(theta < 0.0001) { theta = 0.0001; penalty = 1000.0; }
		if(eta < 0.0001) { eta = 0.0001; penalty = 1000.0; }
		if(eta > 5.0000) { eta = 5.0000; penalty = 1000.0; }
		// recalibrate model and compute error
		model_->recalibrate(lambda,b,theta,eta,omega);
		//FILE* out=fopen("cost.log","a");
		//fprintf(out,"lambda=%f,b=%f,theta=%f,eta=%f,omega=%f,varianceStartValue=%f\n",lambda,b,theta,eta);
		//fclose(out);
		for(int i=0;i<n_;i++) {
			if(useImpliedVols_) {
				double implVola=model_->impliedVola(forward_,strikes_[i],maturity_);
				double blackVola=blackVolas_[i];//blackFormula(Option::Call,strikes_[i],forward_,blackVolas_[i]*sqrt(maturity_));
				result[i] = (implVola-blackVola)/blackVola * penalty;
			}
			else {
				double price=model_->callOptionPrice(forward_,strikes_[i],maturity_);
				double blackPrice=blackPrices_[i];
				result[i] = (price-blackPrice)/blackPrice * penalty;
			}
			//fprintf(out,"   i=%d, forward=%f, strike=%f, mat=%f, Black %f, Model %f\n",i,forward_,strikes_[i],maturity_,blackVola,implVola);
		}
		//out=fopen("cost.log","a");
		//fprintf(out,"Finished.\n");
		//fclose(out);
		return result;
	}


}
#include <calibrationHelperCms.hpp>
#include <stdio.h>

namespace QuantLib {

	CalibrationHelperCms::CalibrationHelperCms(boost::shared_ptr<CmsSwap> cmsSwap,
				boost::shared_ptr<CmsPricer> pricer,
				vector<long> swapLetNumbers,
				vector<double> margins,
				boost::shared_ptr<SabrRbsSmile> volCube,
				int undPillarIndex,
				long noOptPillars
				) : 	
					cmsSwap_(cmsSwap), pricer_(pricer),
					volCube_(volCube), undPillarIndex_(undPillarIndex), useStdCube_(false)
	{
		QL_REQUIRE(swapLetNumbers.size()==margins.size(),"Swaplet numbers vector (" << swapLetNumbers.size() << ") must have same length as margins vector (" << margins.size() << ")");
		
		optPillarTimes_ = volCube->optionPillarTimes();
		if(noOptPillars>0) noOptPillars_=noOptPillars;
		else noOptPillars_=optPillarTimes_.size();

		QL_REQUIRE(noOptPillars_ <= optPillarTimes_.size(),"Number of option pillars (" << noOptPillars_ << ") must be leq opt pillar times (" << optPillarTimes_.size() << ")");

		int pos=0;
		
		for(int i=0;i<swapLetNumbers.size();i++) {
			pos+=swapLetNumbers[i];
			swpGroupEnds_.push_back(pos);
		}
		
		for(int i=0;i<margins.size();i++) {
			marketMargins_.push_back(margins[i]);
			modelMargins_.push_back(0.0);
		}
		
		initDefaultValues();
	}

	CalibrationHelperCms::CalibrationHelperCms(boost::shared_ptr<CmsSwap> cmsSwap,
				boost::shared_ptr<CmsPricer> pricer,
				vector<long> swapLetNumbers,
				vector<double> weights,
				vector<double> margins,
				boost::shared_ptr<SwaptionVolCube4> volCube2,
				Period underlying,
				long noOptPillars,
				long skipOptPillars
				) : 
				cmsSwap_(cmsSwap), pricer_(pricer),
					volCube2_(volCube2), underlying_(underlying), useStdCube_(true) {
		
		QL_REQUIRE(swapLetNumbers.size()==margins.size(),"Swaplet numbers vector (" << swapLetNumbers.size() << ") must have same length as margins vector (" << margins.size() << ")");
		QL_REQUIRE(swapLetNumbers.size()==weights.size(),"Swaplet numbers vector (" << swapLetNumbers.size() << ") must have same length as weights vector (" << weights.size() << ")");

		optPillarTimes_ = volCube2->optionTimes();
		if(noOptPillars>0) noOptPillars_=noOptPillars;
		else noOptPillars_=optPillarTimes_.size();
		skipOptPillars_=skipOptPillars;

		// QL_REQUIRE(noOptPillars_ <= optPillarTimes_.size(),"Number of option pillars (" << noOptPillars_ << ") must be leq opt pillar times (" << optPillarTimes_.size() << ")");

		int pos=0;
		
		for(int i=0;i<swapLetNumbers.size();i++) {
			pos+=swapLetNumbers[i];
			swpGroupEnds_.push_back(pos);
		}
		
		for(int i=0;i<margins.size();i++) {
			marketMargins_.push_back(margins[i]);
			modelMargins_.push_back(0.0);
		}

		// rescale weights
		double sumW=0.0;
		weights_=vector<double>(weights.size(),0.0);
		for(int i=0;i<weights.size();i++) {
				sumW+=weights[i];
		}
		for(int i=0;i<weights.size();i++) {
				weights_[i] = sqrt(weights[i]/sumW);
		}
		
		initDefaultValues();
	}


	void CalibrationHelperCms::initDefaultValues() {
		OPTACCURACY_=1.0E-4; // parameter for end criterion optimization
		MAXIT_=250;
		MAXST_=25;
		maxNu_=250.0;         // above this nu the target function is penalized
		maxMu_=250.0;
		optimizer_=0;
		mode_=0;
		minBeta_=0.1;
		maxBeta_=0.9;
	}

	bool CalibrationHelperCms::setParameters(double optAcc,long maxIt,long maxSt,double maxMu,double maxNu,double minBeta, double maxBeta, int mode, int optimizer) {
		QL_REQUIRE(!useStdCube_ || mode==0 || mode==2 || mode==4 || mode==6,"If std cube is used, mode must be 0,2 or 4,6");
		QL_REQUIRE(mode>=0 && mode<=7,"Mode must be 0 ... 7");
		QL_REQUIRE(optimizer==0 || optimizer==1 || optimizer==2,"Optimizer must be 0, 1 or 2")
		OPTACCURACY_=optAcc;
		MAXIT_=maxIt;
		MAXST_=maxSt;
		maxMu_=maxMu;
		maxNu_=maxNu;
		optimizer_=optimizer;
		mode_=mode;
		minBeta_=minBeta;
		maxBeta_=maxBeta;
		return true;
	}

	double CalibrationHelperCms::calibrate() {

		double sumRmse=0.0;

		//for(int i=0;i<optPillarIndex_.size();i++) {
		
			CmsCostFunction* costfct;
			
			if(!useStdCube_) costfct = new CmsCostFunction(this,&(*volCube_),noOptPillars_,optPillarTimes_,undPillarIndex_,maxMu_,maxNu_,minBeta_,maxBeta_,mode_);
			else costfct = new CmsCostFunction(this,&(*volCube2_),noOptPillars_,optPillarTimes_,skipOptPillars_,underlying_,maxMu_,maxNu_,minBeta_,maxBeta_,mode_,weights_);
			NoConstraint constraint;
			
			int nop;
			if(mode_==0 || mode_==4) nop=2;
			if(mode_==1 || mode_==5) nop=3;
			if(mode_==2 || mode_==6) nop=noOptPillars_;
			if(mode_==3 || mode_==7) nop=4;
			Array guess(nop);

			if(!useStdCube_) {
				if(mode_==0 || mode_==1) {
					guess[0]=tan(M_PI*(volCube_->pillarBeta(0,undPillarIndex_)) - M_PI/2.0);
					guess[1]=tan(M_PI*(volCube_->pillarBeta(noOptPillars_-1,undPillarIndex_)) - M_PI/2.0);
				}
				if(mode_==4 || mode_==5) {
					guess[0]=sqrt(volCube_->pillarNu(0,undPillarIndex_));
					guess[1]=sqrt(volCube_->pillarNu(noOptPillars_-1,undPillarIndex_));
				}
				if(mode_==1 || mode_==5) {
					guess[2]=1.0; // decay factor
				}
				if(mode_==2) {
					for(int i=0;i<noOptPillars_;i++) 
						guess[i]=tan(M_PI*(volCube_->pillarBeta(i,undPillarIndex_)) - M_PI/2.0);
				}
				if(mode_==6) {
					for(int i=0;i<noOptPillars_;i++) 
						guess[i]=sqrt(volCube_->pillarNu(i,undPillarIndex_));
				}
				if(mode_==3) {
					guess[3]=volCube_->pillarBeta(noOptPillars_-1,undPillarIndex_);
					guess[0]=volCube_->pillarBeta(0,undPillarIndex_)-guess[3];
					guess[1]=0.0;
					guess[2]=1.0;
				}
				if(mode_==7) {
					guess[3]=volCube_->pillarNu(noOptPillars_-1,undPillarIndex_);
					guess[0]=volCube_->pillarNu(0,undPillarIndex_)-guess[3];
					guess[1]=0.0;
					guess[2]=1.0;
				}
			}
			else {
				if(mode_==0) {
					{guess[0]=0.5; guess[1]=0.5;} // default guess for beta calibration
				}
				if(mode_==4) {
					{guess[0]=1.0; guess[1]=1.0;} // default guess for nu calibration
				}
				if(mode_==2) {
					for(int i=skipOptPillars_;i<noOptPillars_;i++) 
						guess[i]=0.5;  // default guess for beta calibration
				}
				if(mode_==6) {
					for(int i=skipOptPillars_;i<noOptPillars_;i++) 
						guess[i]=1.0;  // default guess for nu calibration
				}
			}
		
			Problem prblm(*costfct,constraint,guess);
			EndCriteria ec(MAXIT_, MAXST_, OPTACCURACY_, OPTACCURACY_, OPTACCURACY_);
			EndCriteria::Type ret;
			if(optimizer_==0) {
				Simplex method(0.5);
				ret=method.minimize(prblm,ec);	
			}
			if(optimizer_==1) {
				LevenbergMarquardt method;
				ret=method.minimize(prblm,ec);	
			}
			if(optimizer_==2) {
				SimulatedAnnealing method(0.1,0,50,500,2.5,0.2);
				ret=method.minimize(prblm,ec);	
			}
			//SteepestDescent method;
			//LevenbergMarquardt method;
			//SimulatedAnnealing method(0.1,0,50,500,2.5,0.2);
			//FILE *out=fopen("costCms.log","a");
			//fprintf(out,"Opt Settings: MAXIT=%d, MAXST=%d, OPTACCURACY=%1.5e\n",MAXIT,MAXST,OPTACCURACY);
			//fclose(out);
			Array x=prblm.currentValue();
			double y=costfct->value(x);//prblm.functionValue(); to make sure, the model is set to optimal value

			if(ret==EndCriteria::None) QL_FAIL("Optimizer returns status none");
			if(ret==EndCriteria::MaxIterations) QL_FAIL("Optimizer returns status max iterations");
			if(ret==EndCriteria::Unknown) QL_FAIL("Optimizer returns status unknown");

			//sumRmse+=y;

		//}

		if(useStdCube_) volCube2_->updateAfterRecalibration(); // recalibrate 

		return y; //sumRmse;

	}

	double CalibrationHelperCms::margin(int group) {
			int lowerInd=0, upperInd;
			double sngMargin=0.0;
			vector<double> result;
			//for(int group=0;group<swpGroupEnds_.size();group++) {
				if(group>0) lowerInd = swpGroupEnds_[group-1];
				upperInd=swpGroupEnds_[group];
				//FILE *out=fopen("costCms.log","a");
				//fprintf(out,"Group #%d, %d - %d\n",group,lowerInd,upperInd);
				//fclose(out);
				sngMargin=cmsSwap_->margin(pricer_,lowerInd, group>0 ? modelMargins_[group-1] : 0.0,upperInd);
				modelMargins_[group]=sngMargin;
				//result.push_back(sngMargin);
			//}
			return sngMargin;
	}

	vector<double> CalibrationHelperCms::marketMargins() {
		return marketMargins_;
	}

	int CalibrationHelperCms::numberOfGroups() {
		return marketMargins_.size();
	}


	CmsCostFunction::CmsCostFunction(CalibrationHelperCms *helper, SabrRbsSmile *volCube, int noOptPillars, vector<double> optPillarTimes, int undPillarIndex, double maxMu, double maxNu, double minBeta, double maxBeta, int mode) :
		helper_(helper), volCube_(volCube), noOptPillars_(noOptPillars), optPillarTimes_(optPillarTimes), 
			undPillarIndex_(undPillarIndex), maxNu_(maxNu), maxMu_(maxMu),
			minBeta_(minBeta), maxBeta_(maxBeta), mode_(mode), useStdCube_(false)
	{
		m_=helper->numberOfGroups();
		weights_ = vector<double>(m_,1.0/sqrt((double)m_));
	}

	CmsCostFunction::CmsCostFunction(CalibrationHelperCms *helper, SwaptionVolCube4 *volCube2, int noOptPillars, vector<double> optPillarTimes, int skipOptPillars, Period underlying, double maxNu, double maxMu,
		double minBeta, double maxBeta, int mode, vector<double> weights) :
	helper_(helper), volCube2_(volCube2), noOptPillars_(noOptPillars), optPillarTimes_(optPillarTimes),skipOptPillars_(skipOptPillars),
		underlying_(underlying), maxNu_(maxNu), maxMu_(maxMu),
			minBeta_(minBeta), maxBeta_(maxBeta), mode_(mode), useStdCube_(true), weights_(weights)
	{
		m_=helper->numberOfGroups();
	}

	Real CmsCostFunction::value(const QuantLib::Array& x) const {
		Array res = values(x);
		double sum=0.0;
		for(int i=0;i<res.size();i++) {
			sum+=res[i]*res[i];
		}
		return sqrt(sum);
	}

	Disposable<Array> CmsCostFunction::values(const QuantLib::Array& x) const {

		Array result(m_+1);

		double beta1=0.0,beta2=0.0,nu1=0.0,nu2=0.0,lambda=0.0;
		bool sabrRecal;
		double t1=optPillarTimes_[skipOptPillars_];
		double t2=optPillarTimes_[noOptPillars_-1];
		if(mode_==0 || mode_==1) {
			beta1=(atan(x[0])+M_PI/2.0) / M_PI;
			beta2=(atan(x[1])+M_PI/2.0) / M_PI;
		}
		if(mode_==4 || mode_==5) {
			nu1=x[0]*x[0];
			nu2=x[1]*x[1];
		}
		if(mode_==1 || mode_==5) {
			lambda=x[2];
		}
		if(mode_==0 || mode_==1 || mode_==2 || mode_==3) {
			sabrRecal=true;
		}

#ifdef CMSHLOGGING
		FILE *out=fopen("costCms.log","a");
		fprintf(out,"UndPillar=%d nu=[%f,%f] beta=[%f,%f] lambda=%f\n", undPillarIndex_,nu1,nu2,beta1,beta2,lambda);
#endif

		double pen=0.0;
		std::vector<double> betaV(skipOptPillars_), nuV(skipOptPillars_);
		for(int i=skipOptPillars_;i<noOptPillars_;i++) {
			double nu=0.0,beta=0.0;
			if(mode_==0) {
				beta = beta1+(beta2-beta1)/(t2-t1)*(optPillarTimes_[i]-t1);
			}
			if(mode_==1) {
				beta = beta2+(beta1-beta2)*exp(-lambda*(optPillarTimes_[i]-t1));
			}
			if(mode_==2) {
				beta = (atan(x[i])+M_PI/2.0) / M_PI;
			}
			if(mode_==3) {
				beta = (x[0]+x[1]*(optPillarTimes_[i]-t1))*exp(-x[2]*(optPillarTimes_[i]-t1))+x[3];
				if(beta<0.01) beta=0.01;
				if(beta>0.99) beta=0.99;
				pen+= x[0]+x[3] > 0.0 ? 0.0 : exp(1.0-(x[0]+x[3]));
				pen+= x[3] > 0.0? 0.0 : exp(1.0-x[3]);
				pen+= x[2] > 0.0? 0.0 : exp(1.0-x[2]);
			}
			if(mode_==4) {
				nu = nu1+(nu2-nu1)/(t2-t1)*(optPillarTimes_[i]-t1);
			}
			if(mode_==5) {
				nu = nu2+(nu1-nu2)*exp(-lambda*(optPillarTimes_[i]-t1));
			}
			if(mode_==6) {
				nu = x[i]*x[i];
			}
			if(mode_==7) {
				nu = (x[0]+x[1]*(optPillarTimes_[i]-t1))*exp(-x[2]*(optPillarTimes_[i]-t1))+x[3];
				if(nu<0.0) nu=0.0;
				if(nu>maxNu_) nu=maxNu_;
				pen+= x[0]+x[3] > 0.0 ? 0.0 : exp(1.0-(x[0]+x[3]));
				pen+= x[3] > 0.0? 0.0 : exp(1.0-x[3]);
				pen+= x[2] > 0.0? 0.0 : exp(1.0-x[2]);
			}
			if(mode_==0 || mode_==1 || mode_==2 || mode_==3) {
				if(!useStdCube_) volCube_->setPillarBeta(i,undPillarIndex_,beta);
				else betaV.push_back(beta);
				pen += beta <=maxBeta_ ? 0.0 : exp(1.0+beta-maxBeta_);
				pen += beta >=minBeta_ ? 0.0 : exp(1.0+minBeta_-beta);
			}
			if(mode_==4 || mode_==5 || mode_==6 || mode_==7) {
				if(!useStdCube_) volCube_->setPillarNu(i,undPillarIndex_,nu);
				else nuV.push_back(nu);
				pen += nu <= maxNu_ ? 0.0 : exp(1.0+nu-maxNu_);
			}

#ifdef CMSHLOGGING
			fprintf(out,"    optPillar=%d nu=%1.12f, beta=%1.12f\n",i,nu,beta);
#endif

		}

		if(!useStdCube_) volCube_->recalibrate(noOptPillars_-1,undPillarIndex_,sabrRecal);
		else {
			// if opt pillars are skipped, fill up with flat values to the left
			for(int i=0;i<skipOptPillars_;i++) {
				if(mode_==0 || mode_==2) betaV[i]=betaV[skipOptPillars_];
				if(mode_==4 || mode_==6) nuV[i]=nuV[skipOptPillars_];
			}
			// if number of option pillars is lt vol cube option times, fill up with flat values to the right
			for(int i=noOptPillars_; i < volCube2_->optionTimes().size() ; i++) {
				if(mode_==0 || mode_==2) betaV.push_back(betaV[noOptPillars_-1]);
				if(mode_==4 || mode_==6) nuV.push_back(nuV[noOptPillars_-1]);
			}
			if(mode_==0 || mode_==2) volCube2_->recalibration(betaV,underlying_);
			if(mode_==4 || mode_==6) volCube2_->recalibrationNu(nuV,underlying_);
		}

		vector<double> mkt = helper_->marketMargins();
		for(int i=0;i<m_;i++) {
			double margin=helper_->margin(i);
			result[i] = weights_[i]*(margin - mkt[i]);// / mkt[optPillarIndex_];

#ifdef CMSHLOGGING
			fprintf(out,"     %f / %f\n",margin,mkt[i]);
#endif

		}

#ifdef CMSHLOGGING
		fprintf(out,"     %f\n",pen);
		fclose(out);
#endif

		result[m_] = pen;
		return result;

	}
		



}
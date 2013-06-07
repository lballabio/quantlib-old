#include <calibrationHelperCmsCorrelations.hpp>
#include <stdio.h>

namespace QuantLib {

	CalibrationHelperCmsCorrelations::CalibrationHelperCmsCorrelations(boost::shared_ptr<CmsSpreadOption> spreadOption,
				boost::shared_ptr<CmsPricer> pricer,
				long firstOptionlet,
				vector<long> optionLetNumbers,
				vector<double> prices,
				boost::shared_ptr<CorrelationTermStructure> corrTS,
				double lambda, bool useDiffStrike, double diffStrike, int diffFlavour) : 	spreadOption_(spreadOption), pricer_(pricer),
					corrTS_(corrTS), lambda_(lambda), firstOptionlet_(firstOptionlet),
					useDiffStrike_(useDiffStrike), diffStrike_(diffStrike), diffFlavour_(diffFlavour)
	{
		int pos=firstOptionlet_;
		for(int i=0;i<optionLetNumbers.size();i++) {
			pos+=optionLetNumbers[i];
			optGroupEnds_.push_back(pos);
		}
		for(int i=0;i<prices.size();i++) {
			double bef=0.0;
			if(i>0) bef = prices[i-1];
			marketPrices_.push_back(prices[i] - bef);
		}
		useSingleLook_=false;
		initDefaultValues();
	}

	CalibrationHelperCmsCorrelations::CalibrationHelperCmsCorrelations(vector<boost::shared_ptr<SingleLookSpreadOption>> singleLookSpreadOptions,
				boost::shared_ptr<CmsPricer> pricer,
				vector<double> prices,
				boost::shared_ptr<CorrelationTermStructure> corrTS,
				double lambda, bool useDiffStrike, double diffStrike, int diffFlavour) : pricer_(pricer),corrTS_(corrTS), lambda_(lambda), singleLookSpreadOptions_(singleLookSpreadOptions),marketPrices_(prices),
				useDiffStrike_(useDiffStrike), diffStrike_(diffStrike), diffFlavour_(diffFlavour)
	{
		useSingleLook_=true;
		initDefaultValues();
	}

	void CalibrationHelperCmsCorrelations::initDefaultValues() {
		OPTACCURACY_=1.0E-6; // parameter for end criterion optimization
		MAXIT_=5000;
		MAXST_=2500;
		MAXCORR_=0.99;       // maximum correlation
		MINPRICE_=0.00001;   // prices lower than this value are not calibrated (0 is returned in cost function)
		ONLYPOSITIVECORR_=false; // if true only positive correlations are allowed
		optimizer_=0;
	}

	bool CalibrationHelperCmsCorrelations::setParameters(double optAcc,long maxIt,long maxSt,double maxCorr,double minPrice,bool onlyPositiveCorr,int optimizer) {
		OPTACCURACY_=optAcc;
		MAXIT_=maxIt;
		MAXST_=maxSt;
		MAXCORR_=maxCorr;    
		MINPRICE_=minPrice;
		ONLYPOSITIVECORR_=onlyPositiveCorr;
		optimizer_=optimizer;
		return true;
	}

	double CalibrationHelperCmsCorrelations::calibrate() {
		
		CmsCorrelationCostFunction costfct(this,&(*corrTS_),lambda_,MINPRICE_,ONLYPOSITIVECORR_,MAXCORR_);
		CmsCorrelationConstraint constraint;

		int n = corrTS_->numberOfPillars();
		Array guess(n);
		for(int i=0;i<n;i++) {
			double corr=corrTS_->pillarCorrelation(i);
			if(corr>MAXCORR_) corr=MAXCORR_;
			if(corr<-MAXCORR_) corr=-MAXCORR_;
			if(ONLYPOSITIVECORR_) {
				if(corr<0.0) corr=0.0; // only allow positive correlations
				guess[i]=tan(M_PI/2.0*sqrt(corr)); // only allow positive correlations
			}
			else {
				guess[i]=tan(M_PI/2.0*corr);
			}
		}
		
		Problem prblm(costfct,constraint,guess);
		EndCriteria ec(MAXIT_, MAXST_, OPTACCURACY_, OPTACCURACY_, OPTACCURACY_);
		EndCriteria::Type ret;
		if(optimizer_==0) {
			Simplex method(0.05);
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
		//FILE* out=fopen("cost.log","a");
		//fprintf(out,"Start Optimization with maxit=%d, maxst=%d, acc=%f, maxCorr=%f, minPrice=%f, allowPosOnly=%d",
		//	MAXIT,MAXST,OPTACCURACY,MAXCORR,MINPRICE,ONLYPOSITIVECORR);
		//fclose(out);
		
		Array x=prblm.currentValue();
		double y=costfct.value(x);//prblm.functionValue(); to make sure, the model is set to optimal value

		if(ret==EndCriteria::None) QL_FAIL("Optimizer returns status none");
		if(ret==EndCriteria::MaxIterations) QL_FAIL("Optimizer returns status max iterations");
		if(ret==EndCriteria::Unknown) QL_FAIL("Optimizer returns status unknown");

		return y;

	}

	double CalibrationHelperCmsCorrelations::price(int group) {
		if(useSingleLook_) {
			return singleLookSpreadOptions_[group]->npv(pricer_,useDiffStrike_,diffStrike_,diffFlavour_,true); // use precomputed rates, if possible!
		}
		else {
			int lowerInd=firstOptionlet_, upperInd;
			if(group>0) lowerInd = optGroupEnds_[group-1];
			upperInd=optGroupEnds_[group];
			//FILE* out=fopen("cost.log","a");
			//fprintf(out,"computing price for group #%d (%d-%d)\n",group,lowerInd,upperInd);
			//fclose(out);
			return spreadOption_->npv(pricer_,lowerInd,0.0,upperInd,
				useDiffStrike_,diffStrike_,diffFlavour_,true); // use precomputed rates, if possible!
		}
	}

	double CalibrationHelperCmsCorrelations::marketPrice(int group) {
		return marketPrices_[group];
	}

	int CalibrationHelperCmsCorrelations::numberOfGroups() {
		return marketPrices_.size();
	}

	CmsCorrelationCostFunction::CmsCorrelationCostFunction(CalibrationHelperCmsCorrelations *helper, CorrelationTermStructure *corrTS,double lambda,double minPrice,bool onlyPositiveCorr,double MAXCORR) :
		helper_(helper), corrTS_(corrTS), lambda_(lambda), MINPRICE(minPrice), ONLYPOSITIVECORR(onlyPositiveCorr), MAXCORR_(MAXCORR)
	{
		n_=corrTS_->numberOfPillars();
		m_=helper_->numberOfGroups();
	}

	Real CmsCorrelationCostFunction::value(const QuantLib::Array& x) const {
		Array res = values(x);
		double sum=0.0;
		for(int i=0;i<res.size();i++) {
			sum+=res[i]*res[i];
		}
		return sqrt(sum/res.size());
	}

	Disposable<Array> CmsCorrelationCostFunction::values(const QuantLib::Array& x) const {
		// set parameters
		Array result(m_+1);
		// admissability
		// recalibrate model and compute error
		//FILE* out=fopen("CMSCORRCOST.log","a");
		double var=0.0,var2=0.0,corr=0.0,corrBef1=0.0,corrBef2=0.0,corrBef3=0.0;
		for(int i=0;i<n_;i++) {
			corrBef3=corrBef2;
			corrBef2=corrBef1;
			corrBef1=corr;
			corr=atan(x[i])*2.0/M_PI;
			if(ONLYPOSITIVECORR) {
				corr*=corr; // allow only correlations > 0%
			}
			corrTS_->setPillarCorrelation(i,corr);
			if(fabs(corr)>MAXCORR_) // dont let the search get lost in correlations near +-1
				var2+=1.0/(1.0-fabs(corr)) - 1.0/(1.0-MAXCORR_);
			if(i>2) {
				//var+=fabs(corr-2.0*corrBef1+corrBef2);
				if((corr-corrBef1)*(corrBef1-corrBef2)<0.0 && (corrBef1-corrBef2)*(corrBef2-corrBef3)<0.0)
					var+=(fabs(corr-2.0*corrBef1+corrBef2)+fabs(corrBef1-2.0*corrBef2+corrBef3));
			}
			//fprintf(out,"%f;",corr);
		}
		//fprintf(out,"---;");
		for(int i=0;i<m_;i++) {
			double price = helper_->price(i);
			double mkt = helper_->marketPrice(i);
			if(mkt<MINPRICE) { // if mkt price < MINPRICE return 0 in cost function
				result[i] = 0.0;
			}
			else {
				result[i] = (price - mkt) / mkt;
			}
			//fprintf(out,"%f;%f;%f;*;",price,mkt,result[i]);
		}
		//fprintf(out,"%f;%f\n",lambda_ * var * ((double)n_), var2);
		//fclose(out);
		result[m_] = lambda_ * var * ((double)n_) + var2;
		return result;
	}
		



}
#include <cmsSpreadOption.hpp>
#include <stdio.h>

namespace QuantLib {

	CmsSpreadOption::CmsSpreadOption(boost::shared_ptr<Schedule> fixingSchedule,
				boost::shared_ptr<Schedule> paymentSchedule,
				boost::shared_ptr<Schedule> calculationSchedule,
				boost::shared_ptr<SwapIndex> index1,
				boost::shared_ptr<SwapIndex> index2,
				DayCounter couponDayCounter,
				double strike,
				int flavour) : 	index1_(index1), index2_(index2),
								couponDayCounter_(couponDayCounter),
								strike_(strike),flavour_(flavour)
	{
		
		fixings_ = fixingSchedule->dates();
		payments_ = paymentSchedule->dates();
		calc_ = calculationSchedule->dates();
		rates1_=vector<double>(fixings_.size());
		rates2_=vector<double>(fixings_.size());
		adjustedRates1_=vector<double>(fixings_.size());
		adjustedRates2_=vector<double>(fixings_.size());
		spreads_=vector<double>(fixings_.size());
		areRatesComputed_=vector<bool>(fixings_.size(),false);
		QL_REQUIRE(fixings_.size() == payments_.size() && fixings_.size() == calc_.size()-1,
			"Number of fixings (" << fixings_.size() << "), payments (" << payments_.size() << ") and calculation schedules (" << calc_.size() << ") do not match.");		
	}

	CmsSpreadOption::CmsSpreadOption(boost::shared_ptr<Schedule> calculationSchedule,
				int fixingDays,
				boost::shared_ptr<SwapIndex> index1,
				boost::shared_ptr<SwapIndex> index2,
				DayCounter couponDayCounter,
				double strike,
				int flavour) : index1_(index1), index2_(index2),
				couponDayCounter_(couponDayCounter), strike_(strike), flavour_(flavour) {

		calc_ = calculationSchedule->dates();
		Calendar cal = calculationSchedule->calendar();
		BusinessDayConvention bdc = calculationSchedule->businessDayConvention();
		for(int i=1;i<calc_.size();i++) {
			payments_.push_back(calc_[i]);
			Date fix = calc_[i-1];
			fix = cal.advance(fix,-fixingDays,Days,bdc);
			fixings_.push_back(fix);
		}
		rates1_=vector<double>(fixings_.size());
		rates2_=vector<double>(fixings_.size());
		adjustedRates1_=vector<double>(fixings_.size());
		adjustedRates2_=vector<double>(fixings_.size());
		spreads_=vector<double>(fixings_.size());
		areRatesComputed_=vector<bool>(fixings_.size(),false);

		QL_REQUIRE(fixings_.size() == payments_.size() && fixings_.size() == calc_.size()-1,
			"Number of fixings (" << fixings_.size() << "), payments (" << payments_.size() << ") and calculation schedules (" << calc_.size() << ") do not match.");

	}

	Real CmsSpreadOption::npv(boost::shared_ptr<CmsPricer> pricer,Size preCalcFixings,double preCalcPrice, Size fixingUpperBound, 
		bool useOtherStrike, double otherStrike, int otherFlavour, bool usePreviousRates) {
		
		double price=0.0;
		for(Size i=preCalcFixings;i< (fixingUpperBound==0 ? fixings_.size() : fixingUpperBound);i++) {
			//FILE* out=fopen("CMSSO.log","a");
			//fprintf(out,"%d;%d;%d;%d;%d;%f;%d;%f;%f",i,calc_[i],calc_[i+1],fixings_[i],payments_[i],strike_,flavour_,pricer->correlationTermStructure()->pillarCorrelations()[0],pricer->correlationTermStructure()->pillarCorrelations()[1]);
			//fclose(out);
			if(!useOtherStrike) {
				otherStrike=strike_; otherFlavour=flavour_;
			}
			double p=pricer->cmssoPrice(calc_[i],calc_[i+1],fixings_[i],payments_[i],index1_,index2_,couponDayCounter_,
				otherStrike,otherFlavour,usePreviousRates & areRatesComputed_[i],rates1_[i],rates2_[i],adjustedRates1_[i],adjustedRates2_[i]);
			price+=p;
			if(!(usePreviousRates & areRatesComputed_[i])) {
				rates1_[i]=pricer->lastRate(true,false);
				rates2_[i]=pricer->lastRate(false,false);			
				adjustedRates1_[i]=pricer->lastRate(true,true);	
				adjustedRates2_[i]=pricer->lastRate(false,true);	
				areRatesComputed_[i]=true;
			}
			spreads_ [i]=pricer->lastSpread();
		}
		//FILE* out=fopen("CMSSO.log","a");
		//fprintf(out,"---\n");
		//fclose(out);

		return preCalcPrice+price;
	}


	Real CmsSpreadOption::impliedCorrelation(boost::shared_ptr<CmsPricer> pricer,const Period& pillar, const double price,Size preCalculatedFixings,double preCalculatedPrice) {
		Brent b;
		double res=b.solve(CmsSpreadOptionImplCorrHelper(this,pricer,pillar,price,preCalculatedFixings,preCalculatedPrice),CORRACC,pricer->correlationTermStructure()->correlation(pillar,true),CORRSTEP);
		double res2=atan(res)*2.0/M_PI;
		pricer->correlationTermStructure()->setPillarCorrelation(pillar,res2);
		return res2;
		/*CmsSpreadOptionImplCorrHelper h(boost::shared_ptr<CmsSpreadOption>(this),pricer,pillar,price);
		double x1=-MAXCORR,x2=MAXCORR,x;
		double f1=h(x1);
		double f2=h(x2);
		double f;
		double step=MAXCORR+MAXCORR;
		if(f1*f2>=0.0) QL_FAIL("No sign change for correlations -1,1");
		do {
			x=(x1+x2)/2.0;
			f=h(x);
			if(f1*f<0.0) x2=x; else x1=x;
			step/=step;
			pricer->correlationTermStructure()->setPillarCorrelation(pillar,x);
		} while(step>CORRACC);
		pricer->correlationTermStructure()->setPillarCorrelation(pillar,x);
		return x;*/
	}

	Real CmsSpreadOption::strike() { return strike_; }
	std::vector<Date> CmsSpreadOption::fixingSchedule() { return fixings_; }
	std::vector<Date> CmsSpreadOption::paymentSchedule() { return payments_; }
	std::vector<Date> CmsSpreadOption::calculationSchedule() { return calc_; }
	int CmsSpreadOption::flavour() { return flavour_; }

	std::vector<double> CmsSpreadOption::rates(bool first,bool adjusted) {
		if(adjusted) {
			if(first) return adjustedRates1_; else return adjustedRates2_;
		}
		else {
			if(first) return rates1_; else return rates2_;
		}
	}

	std::vector<double> CmsSpreadOption::spreads() {
		return spreads_;
	}

}
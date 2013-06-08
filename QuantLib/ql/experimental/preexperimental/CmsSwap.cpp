#include <cmsSwap.hpp>
#include <stdio.h>

namespace QuantLib {

	CmsSwap::CmsSwap(boost::shared_ptr<Schedule> fixingSchedule,
				boost::shared_ptr<Schedule> paymentSchedule,
				boost::shared_ptr<Schedule> calculationSchedule,
				boost::shared_ptr<SwapIndex> swapIndex,
				DayCounter couponDayCounter,
				boost::shared_ptr<Schedule> floatLegFixingSchedule,
				boost::shared_ptr<Schedule> floatLegPaymentSchedule,
				boost::shared_ptr<Schedule> floatLegCalculationSchedule,
				boost::shared_ptr<IborIndex> floatIndex,
				DayCounter floatLegCouponDayCounter,
				boost::shared_ptr<YieldTermStructure> floatDiscountCurve
				) :	swapIndex_(swapIndex), floatIndex_(floatIndex), 
					couponDayCounter_(couponDayCounter), 
					floatLegCouponDayCounter_(floatLegCouponDayCounter),
					floatDiscountCurve_(floatDiscountCurve)
	{

		strike_=0.0;
		flavour_=1;
		margin_=0.0;

		fixings_ = fixingSchedule->dates();
		payments_ = paymentSchedule->dates();
		calc_ = calculationSchedule->dates();
		floatFixings_ = floatLegFixingSchedule->dates();
		floatPayments_ = floatLegPaymentSchedule->dates();
		floatCalc_ = floatLegCalculationSchedule->dates();

		rates_=vector<double>(payments_.size(),0.0);
		adjustedRates_=vector<double>(payments_.size(),0.0);
		upperBound_=vector<double>(payments_.size(),0.0);

		QL_REQUIRE(fixings_.size() == payments_.size() && fixings_.size() == calc_.size()-1,
			"Number of fixings (" << fixings_.size() << "), payments (" << payments_.size() << ") and calculation schedules (" << calc_.size() << ") do not match.");		
		
		QL_REQUIRE(floatFixings_.size() == floatPayments_.size() && floatFixings_.size() == floatCalc_.size()-1,
			"Number of payments (" << floatPayments_.size() << ") and calculation schedules (" << floatCalc_.size() << ") do not match.");
	}

	CmsSwap::CmsSwap(boost::shared_ptr<Schedule> calculationSchedule,
				int fixingDays,
				boost::shared_ptr<SwapIndex> swapIndex,
				DayCounter couponDayCounter,
				boost::shared_ptr<Schedule> floatLegCalculationSchedule,
				int floatFixingDays,
				boost::shared_ptr<IborIndex> floatIndex,
				DayCounter floatLegCouponDayCounter,
				boost::shared_ptr<YieldTermStructure> floatDiscountCurve,
				double strike, int flavour) : swapIndex_(swapIndex), floatIndex_(floatIndex), 
				couponDayCounter_(couponDayCounter), floatLegCouponDayCounter_(floatLegCouponDayCounter),
				floatDiscountCurve_(floatDiscountCurve), strike_(strike), flavour_(flavour)
	{

		margin_=0.0;

		calc_ = calculationSchedule->dates();
		floatCalc_ = floatLegCalculationSchedule->dates();
		
		Calendar cal = calculationSchedule->calendar();
		Calendar cal2 = floatLegCalculationSchedule->calendar();
		BusinessDayConvention bdc = calculationSchedule->businessDayConvention();
		BusinessDayConvention bdc2 = floatLegCalculationSchedule->businessDayConvention();
		for(int i=1;i<calc_.size();i++) {
			payments_.push_back(calc_[i]);
			Date fix = calc_[i-1]; // i-1
			fix = cal.advance(fix,-fixingDays,Days,bdc);
			fixings_.push_back(fix);
		}
		for(int i=1;i<floatCalc_.size();i++) {
			floatPayments_.push_back(floatCalc_[i]);
			Date fix = floatCalc_[i-1];
			fix = cal.advance(fix,-floatFixingDays,Days,bdc);
			floatFixings_.push_back(fix);
		}

		rates_=vector<double>(payments_.size(),0.0);
		adjustedRates_=vector<double>(payments_.size(),0.0);
		upperBound_=vector<double>(payments_.size(),0.0);

		QL_REQUIRE(fixings_.size() == payments_.size() && fixings_.size() == calc_.size()-1,
			"Number of fixings (" << fixings_.size() << "), payments (" << payments_.size() << ") and calculation schedules (" << calc_.size() << ") do not match.");		
		
		QL_REQUIRE(floatFixings_.size() == floatPayments_.size() && floatFixings_.size() == floatCalc_.size()-1,
			"Number of payments (" << floatPayments_.size() << ") and calculation schedules (" << floatCalc_.size() << ") do not match.");

	}

	Real CmsSwap::npv(boost::shared_ptr<CmsPricer> pricer, int precomputedSwaplets,double precomputedPrice,int swapletUpperBound) {
		double price=0.0;
		
		//rates_.clear(); 
		//adjustedRates_.clear();
		//upperBound_.clear();

		if(swapletUpperBound==0) {  // price whole cms swap
			swapletUpperBound = fixings_.size();
		}
		
		for(Size i=precomputedSwaplets;i<swapletUpperBound;i++) {
			price+=pricer->cmsPrice(calc_[i],calc_[i+1],fixings_[i],payments_[i],swapIndex_,couponDayCounter_,strike_,flavour_,margin_);
			rates_[i]=pricer->lastRate(true,false);
			adjustedRates_[i]=pricer->lastRate(true,true);
			upperBound_[i]=pricer->lastUpperBound(true);
		}
		return price+precomputedPrice;
	}

	Real CmsSwap::totalNpv(boost::shared_ptr<CmsPricer> pricer) {
		// compute float NPV
		double sum=0.0,dcf,df;
		for(Size i=0;i<floatFixings_.size();i++) {
				dcf=floatLegCouponDayCounter_.yearFraction(floatCalc_[i],floatCalc_[i+1]);
				df=floatDiscountCurve_->discount(floatPayments_[i]);
				sum+=dcf*df*floatIndex_->fixing(floatFixings_[i],true);
		}
		// return total NPV
		return sum-npv(pricer,0,0.0,0);
	}
	
	std::vector<double> CmsSwap::rates(bool adjusted) {
		if(adjusted) {
			return adjustedRates_; 
		}
		else {
			return rates_; 
		}
	}

	std::vector<double> CmsSwap::upperBounds() {
		return upperBound_;
	}

	std::vector<Date> CmsSwap::fixingSchedule() { return fixings_; }
	std::vector<Date> CmsSwap::paymentSchedule() { return payments_; }
	std::vector<Date> CmsSwap::calculationSchedule() { return calc_; }
	
	Real CmsSwap::margin(boost::shared_ptr<CmsPricer> pricer, int precomputedSwaplets, double precomputedMargin, int swapletUpperBound, int precomputedFloatlets, int floatletUpperBound) {

		if(swapletUpperBound==0) {
			swapletUpperBound = fixings_.size();
		}

		if(floatletUpperBound==0) {
			floatletUpperBound = swapletUpperBound;
		}
		if(precomputedFloatlets==0) {
			precomputedFloatlets = precomputedSwaplets;
		}
		
		double prePrice=0.0;
		if(precomputedFloatlets>0) {
			// convert precomputed Margin to price
			double sum=0.0,dcf,df;
			double sumFlat=0.0;
			for(Size i=0;i<precomputedFloatlets;i++) {
				dcf=floatLegCouponDayCounter_.yearFraction(floatCalc_[i],floatCalc_[i+1]);
				df=floatDiscountCurve_->discount(floatPayments_[i]);
				sum+=dcf*df;
				sumFlat+=dcf*df*floatIndex_->fixing(floatFixings_[i],true);
			}
			prePrice=sumFlat+sum*precomputedMargin;
		}

		double pv = npv(pricer,precomputedSwaplets,prePrice,swapletUpperBound);
		//test: pv of 6m leg, check against basis swap quotations (sucessful)
		//double pv = 1.0 - pricer->yieldTermStructure()->discount(payments_[swapletUpperBound-1]);

		// float leg annuity
		double sum=0.0,dcf,df;
		double sumFlat=0.0;
		for(Size i=0;i<floatletUpperBound;i++) {
			dcf=floatLegCouponDayCounter_.yearFraction(floatCalc_[i],floatCalc_[i+1]);
			df=floatDiscountCurve_->discount(floatPayments_[i]);
			sum+=dcf*df;
			sumFlat+=dcf*df*floatIndex_->fixing(floatFixings_[i],true);
		}

		return ( pv - sumFlat ) / sum;

	}
}
/*
 Copyright (C) 2013 Peter Caspers

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it
 under the terms of the QuantLib license.  You should have received a
 copy of the license along with this program; if not, please email
 <quantlib-dev@lists.sf.net>. The license is also available online at
 <http://quantlib.org/license.shtml>.


 This program is distributed in the hope that it will be useful, but
 WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 or FITNESS FOR A PARTICULAR PURPOSE. See the license for more details.
 */

/*! \file cmsreplicationpricer.cpp
    \brief
*/

#include <ql/cashflows/cmsreplicationpricer.hpp>
#include <ql/cashflows/fixedratecoupon.hpp>
#include <ql/cashflows/iborcoupon.hpp>
#include <ql/cashflows/cmscoupon.hpp>
#include <ql/termstructures/volatility/smilesection.hpp>
#include <ql/termstructures/yieldtermstructure.hpp>
#include <ql/quotes/simplequote.hpp>
#include <ql/indexes/iborindex.hpp>
#include <ql/time/schedule.hpp>
#include <ql/instruments/vanillaswap.hpp>

namespace QuantLib {

    CmsReplicationPricer::CmsReplicationPricer(
                const Handle<SwaptionVolatilityStructure>& swaptionVol,
                const Handle<Quote>& meanReversion,
		const Handle<YieldTermStructure>& couponDiscountCurve,
		const Settings settings) : CmsCouponPricer(swaptionVol), meanReversion_(meanReversion), 
					   couponDiscountCurve_(couponDiscountCurve), settings_(settings) { 
          registerWith(meanReversion_);
	  if(!couponDiscountCurve_.empty())
	    registerWith(couponDiscountCurve_);
    }

    void CmsReplicationPricer::initialize(const FloatingRateCoupon& coupon){

        coupon_ =  dynamic_cast<const CmsCoupon*>(&coupon);
        QL_REQUIRE(coupon_, "CMS coupon needed");
        gearing_ = coupon_->gearing();
        spread_ = coupon_->spread();
        Time accrualPeriod = coupon_->accrualPeriod();
        QL_REQUIRE(accrualPeriod != 0.0, "null accrual period");

        fixingDate_ = coupon_->fixingDate();
        paymentDate_ = coupon_->date();
        swapIndex_ = coupon_->swapIndex();

	forwardCurve_ = swapIndex_->forwardingTermStructure();
	if(swapIndex_->exogenousDiscount())
	  discountCurve_ = swapIndex_->discountingTermStructure();
	else
	  discountCurve_ = forwardCurve_;
	
	// if no coupon discount curve is given just use the discounting curve from the swap index.
	// for rate calculation this curve cancels out in the computation, so e.g. the discounting
	// swap engine will produce correct results, even if the couponDiscountCurve is not set here.
	// only the price member function in this class will be dependent on the coupon discount curve.

	if(couponDiscountCurve_.empty())
	  couponDiscountCurve_ = discountCurve_;

	today_ = QuantLib::Settings::instance().evaluationDate();

        if(paymentDate_ > today_)
            discount_ = couponDiscountCurve_->discount(paymentDate_);
        else discount_= 1.;

        spreadLegValue_ = spread_ * accrualPeriod * discount_;

        if (fixingDate_ > today_) {

            swapTenor_ = swapIndex_->tenor();
            swap_ = swapIndex_->underlyingSwap(fixingDate_);

            swapRateValue_ = swap_->fairRate();

	    const Leg& fixedCoupons = swap_->fixedLeg();
	    fixedLegPaymentDates_ = std::vector<Date>(fixedCoupons.size());
	    fixedLegYearFractions_ = std::vector<Real>(fixedCoupons.size());
	    for(Size i=0; i<fixedCoupons.size(); i++) {
		boost::shared_ptr<FixedRateCoupon> coupon = 
		    boost::dynamic_pointer_cast<FixedRateCoupon>(fixedCoupons[i]);
		fixedLegPaymentDates_[i] = coupon->date();
		fixedLegYearFractions_[i] = coupon->accrualPeriod();
	    }

	    const Leg& floatingCoupons = swap_->floatingLeg();
	    floatingLegStartDates_ = floatingLegEndDates_ = floatingLegPaymentDates_ = 
		std::vector<Date>(floatingCoupons.size());
	    for(Size i=0; i<floatingCoupons.size(); i++) {
		boost::shared_ptr<IborCoupon> coupon = 
		    boost::dynamic_pointer_cast<IborCoupon>(floatingCoupons[i]);
		floatingLegStartDates_[i] = coupon->accrualStartDate();
		floatingLegEndDates_[i] = coupon->accrualEndDate();
		floatingLegPaymentDates_[i] = coupon->date();
	    }
            
         }
    }

    Real CmsReplicationPricer::hullWhiteScenario(Real dt, Real h) {
	
	Real e;

	if(close(meanReversion_->value(),0.0))
	    e = dt;
	else
	    e = (1.0 - std::exp( -dt * meanReversion_->value() ) ) / meanReversion_->value();

	return std::exp( h * e );

    }

    Real CmsReplicationPricer::annuity(Real h) {
	
	Real annuity=0.0;
	for(Size i=0; i<fixedLegYearFractions_.size(); i++) {
	    Real dt = discountCurve_->timeFromReference(fixedLegPaymentDates_[i]) -
		discountCurve_->timeFromReference(fixingDate_);
	    annuity += fixedLegYearFractions_[i] * discountCurve_->discount(fixedLegPaymentDates_[i])
		/ discountCurve_->discount(fixingDate_) * hullWhiteScenario(dt,h);
	}

	return annuity;

    }

    Real CmsReplicationPricer::floatingLegNpv(Real h) {
	
	Real npv=0.0;
	for(Size i=0; i<floatingLegStartDates_.size();i++) {
	    Real dt1 = forwardCurve_->timeFromReference(floatingLegEndDates_[i])-
		forwardCurve_->timeFromReference(floatingLegStartDates_[i]);
	    Real dt2 = discountCurve_->timeFromReference(floatingLegPaymentDates_[i])-
		discountCurve_->timeFromReference(fixingDate_);
	    // we use that the day counter of the floating leg is equal to that of the float index always for
	    // swapIndex underlying swaps
	    npv += (forwardCurve_->discount(floatingLegStartDates_[i]) / 
		    forwardCurve_->discount(floatingLegEndDates_[i]) / hullWhiteScenario(dt1,h) - 1.0) * 
		discountCurve_->discount(floatingLegPaymentDates_[i]) / discountCurve_->discount(fixingDate_) * 
		hullWhiteScenario(dt2,h);
	}

	return npv;

    }

    Real CmsReplicationPricer::swapRate(Real h) {

	return floatingLegNpv(h) / annuity(h);

    }

    Real CmsReplicationPricer::optionletPrice(
                                Option::Type optionType, Real strike) const {

	return 0.0;

    }

    Real CmsReplicationPricer::meanReversion() const { return meanReversion_->value();}

    Rate CmsReplicationPricer::swapletRate() const {
        return swapletPrice()/(coupon_->accrualPeriod()*discount_);
    }

    Real CmsReplicationPricer::capletPrice(Rate effectiveCap) const {
        // caplet is equivalent to call option on fixing
        if (fixingDate_ <= today_) {
            // the fixing is determined
            const Rate Rs =
                std::max(coupon_->swapIndex()->fixing(fixingDate_)-effectiveCap, 0.);
            Rate price = (gearing_*Rs)*(coupon_->accrualPeriod()*discount_);
            return price;
        } else {
            Real capletPrice = optionletPrice(Option::Call, effectiveCap);
            return gearing_ * capletPrice;
        }
    }

    Rate CmsReplicationPricer::capletRate(Rate effectiveCap) const {
        return capletPrice(effectiveCap)/(coupon_->accrualPeriod()*discount_);
    }

    Real CmsReplicationPricer::floorletPrice(Rate effectiveFloor) const {
        // floorlet is equivalent to put option on fixing
        if (fixingDate_ <= today_) {
            // the fixing is determined
            const Rate Rs =
                std::max(effectiveFloor-coupon_->swapIndex()->fixing(fixingDate_),0.);
            Rate price = (gearing_*Rs)*(coupon_->accrualPeriod()*discount_);
            return price;
        } else {
            Real floorletPrice = optionletPrice(Option::Put, effectiveFloor);
            return gearing_ * floorletPrice;
        }
    }

    Rate CmsReplicationPricer::floorletRate(Rate effectiveFloor) const {
        return floorletPrice(effectiveFloor)/(coupon_->accrualPeriod()*discount_);
    }

    Real CmsReplicationPricer::swapletPrice() const {

        if (fixingDate_ <= today_) {
            // the fixing is determined
            const Rate Rs = coupon_->swapIndex()->fixing(fixingDate_);
            Rate price = (gearing_*Rs + spread_)*(coupon_->accrualPeriod()*discount_);
            return price;
        } else {
            Real atmCapletPrice = optionletPrice(Option::Call, swapRateValue_);
            Real atmFloorletPrice = optionletPrice(Option::Put, swapRateValue_);
            return gearing_ *(coupon_->accrualPeriod()* discount_ * swapRateValue_
                             + atmCapletPrice - atmFloorletPrice)
                   + spreadLegValue_;
        }
    }

}

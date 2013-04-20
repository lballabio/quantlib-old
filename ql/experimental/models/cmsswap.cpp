/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2013 Peter Caspers

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it
 under the terms of the QuantLib license.  You should have received a
 copy of the license along with this program; if not, please email
 <quantlib-dev@lists.sf.net>. The license is also available online at
 <http://quantlib.org/license.shtml>.

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
*/

#include <ql/experimental/models/markovfunctional/cmsswap.hpp>
#include <ql/cashflows/iborcoupon.hpp>
#include <ql/cashflows/cmscoupon.hpp>
#include <ql/cashflows/capflooredcoupon.hpp>
#include <ql/cashflows/cashflowvectors.hpp>
#include <ql/cashflows/cashflows.hpp>
#include <ql/cashflows/couponpricer.hpp>
#include <ql/indexes/iborindex.hpp>
#include <ql/indexes/swapindex.hpp>
#include <ql/termstructures/yieldtermstructure.hpp>

namespace QuantLib {

    CmsSwap::CmsSwap(
            Type type,
            Real nominal,
            const Schedule& structuredSchedule,
			const boost::shared_ptr<SwapIndex>& swapIndex,
            Spread structuredSpread,
			Rate cappedRate,
			Rate flooredRate,
            const DayCounter& structuredDayCount,
            const Schedule& floatSchedule,
            const boost::shared_ptr<IborIndex>& iborIndex,
            Spread spread,
            const DayCounter& floatingDayCount,
            boost::optional<BusinessDayConvention> paymentConvention)

    : Swap(2), type_(type), nominal_(nominal),
      structuredSchedule_(structuredSchedule), swapIndex_(swapIndex), structuredSpread_(structuredSpread),
	  cappedRate_(cappedRate), flooredRate_(flooredRate),
      structuredDayCount_(structuredDayCount),
      floatingSchedule_(floatSchedule), iborIndex_(iborIndex), spread_(spread),
      floatingDayCount_(floatingDayCount) {

        if (paymentConvention)
            paymentConvention_ = *paymentConvention;
        else
            paymentConvention_ = floatingSchedule_.businessDayConvention();

		CmsLeg cmsLeg = CmsLeg(structuredSchedule_, swapIndex_).withNotionals(nominal_).withPaymentAdjustment(paymentConvention_);
		if(cappedRate_ != Null<Real>()) cmsLeg = cmsLeg.withCaps(cappedRate_);
		if(flooredRate_ != Null<Real>()) cmsLeg = cmsLeg.withFloors(flooredRate_);
        legs_[0] = cmsLeg;

		IborLeg iborLeg = IborLeg(floatingSchedule_,iborIndex_).withNotionals(nominal_).withPaymentDayCounter(floatingDayCount_).withPaymentAdjustment(paymentConvention_);
		if(spread_ != Null<Real>()) iborLeg = iborLeg.withSpreads(spread_);
		legs_[1] = iborLeg;

        for (Leg::const_iterator i = legs_[0].begin(); i < legs_[0].end(); ++i)
            registerWith(*i);

        for (Leg::const_iterator i = legs_[1].begin(); i < legs_[1].end(); ++i)
            registerWith(*i);

        switch (type_) {
          case Payer:
            payer_[0] = -1.0;
            payer_[1] = +1.0;
            break;
          case Receiver:
            payer_[0] = +1.0;
            payer_[1] = -1.0;
            break;
          default:
            QL_FAIL("Unknown cms-swap type");
        }
    }

    void CmsSwap::setupArguments(PricingEngine::arguments* args) const {

        Swap::setupArguments(args);

        CmsSwap::arguments* arguments =
            dynamic_cast<CmsSwap::arguments*>(args);

		QL_REQUIRE(arguments != 0, "argument type does not match");

        arguments->type = type_;
        arguments->nominal = nominal_;

        const Leg& structuredCoupons = structuredLeg();

        arguments->structuredResetDates = arguments->structuredPayDates = arguments->structuredFixingDates = std::vector<Date>(structuredCoupons.size());
		arguments->structuredAccrualTimes = std::vector<Time>(structuredCoupons.size());
		arguments->structuredSpreads = std::vector<Spread>(structuredCoupons.size());
		arguments->structuredCappedRates = arguments->structuredFlooredRates = std::vector<Rate>(structuredCoupons.size(),Null<Real>());

        for (Size i=0; i<structuredCoupons.size(); ++i) {
            boost::shared_ptr<CmsCoupon> coupon = boost::dynamic_pointer_cast<CmsCoupon>(structuredCoupons[i]);
			if(coupon) {
				arguments->structuredAccrualTimes[i] = coupon->accrualPeriod();
				arguments->structuredPayDates[i] = coupon->date();
				arguments->structuredResetDates[i] = coupon->accrualStartDate();
				arguments->structuredFixingDates[i] = coupon->fixingDate();
				arguments->structuredSpreads[i] = coupon->spread();
			}
			boost::shared_ptr<CappedFlooredCmsCoupon> cfcoupon = boost::dynamic_pointer_cast<CappedFlooredCmsCoupon>(structuredCoupons[i]);
			if(cfcoupon) {
				arguments->structuredAccrualTimes[i] = cfcoupon->accrualPeriod();
				arguments->structuredPayDates[i] = cfcoupon->date();
				arguments->structuredResetDates[i] = cfcoupon->accrualStartDate();
				arguments->structuredFixingDates[i] = cfcoupon->fixingDate();
				arguments->structuredSpreads[i] = cfcoupon->spread();
				arguments->structuredCappedRates[i] = cfcoupon->cap();
				arguments->structuredFlooredRates[i] = cfcoupon->floor();
			}
        }

        const Leg& floatingCoupons = floatingLeg();

        arguments->floatingResetDates = arguments->floatingPayDates = arguments->floatingFixingDates = std::vector<Date>(floatingCoupons.size());
        arguments->floatingAccrualTimes = std::vector<Time>(floatingCoupons.size());
        arguments->floatingSpreads = std::vector<Spread>(floatingCoupons.size());

		for (Size i=0; i<floatingCoupons.size(); ++i) {
            boost::shared_ptr<IborCoupon> coupon = boost::dynamic_pointer_cast<IborCoupon>(floatingCoupons[i]);
            arguments->floatingResetDates[i] = coupon->accrualStartDate();
            arguments->floatingPayDates[i] = coupon->date();
            arguments->floatingFixingDates[i] = coupon->fixingDate();
            arguments->floatingAccrualTimes[i] = coupon->accrualPeriod();
            arguments->floatingSpreads[i] = coupon->spread();

        }

    }

    void CmsSwap::setupExpired() const {
        Swap::setupExpired();
        legBPS_[0] = legBPS_[1] = 0.0;
    }

    void CmsSwap::fetchResults(const PricingEngine::results* r) const {
        Swap::fetchResults(r);
        const CmsSwap::results* results = dynamic_cast<const CmsSwap::results*>(r);
		QL_REQUIRE(results != 0, "result type doess not match");
		fairStructuredSpread_ = results->fairStructuredSpread;
    }

    void CmsSwap::arguments::validate() const {

        Swap::arguments::validate();
        
		QL_REQUIRE(nominal != Null<Real>(), "nominal null or not set");
        
		QL_REQUIRE(structuredResetDates.size() == structuredPayDates.size(),
                   "number of structured start dates different from "
                   "number of structured payment dates");
        QL_REQUIRE(structuredFixingDates.size() == structuredPayDates.size(),
                   "number of structured fixing dates different from "
                   "number of structured payment dates");
        QL_REQUIRE(structuredAccrualTimes.size() == structuredPayDates.size(),
                   "number of structured accrual Times different from "
                   "number of structured payment dates");
        QL_REQUIRE(structuredSpreads.size() == structuredPayDates.size(),
                   "number of structured spreads different from "
                   "number of structured payment dates");

		QL_REQUIRE(floatingResetDates.size() == floatingPayDates.size(),
                   "number of floating start dates different from "
                   "number of floating payment dates");
        QL_REQUIRE(floatingFixingDates.size() == floatingPayDates.size(),
                   "number of floating fixing dates different from "
                   "number of floating payment dates");
        QL_REQUIRE(floatingAccrualTimes.size() == floatingPayDates.size(),
                   "number of floating accrual Times different from "
                   "number of floating payment dates");
        QL_REQUIRE(floatingSpreads.size() == floatingPayDates.size(),
                   "number of floating spreads different from "
                   "number of floating payment dates");

		QL_REQUIRE(structuredCappedRates.size() == structuredPayDates.size(),
            "number of structured capped rates different from "
            "number of structured payment dates");
        QL_REQUIRE(structuredFlooredRates.size() == structuredPayDates.size(),
                   "number of structured floored rates different from "
                   "number of structured payment dates");

    }

    void CmsSwap::results::reset() {
        Swap::results::reset();
		fairStructuredSpread = Null<Spread>();
    }

}

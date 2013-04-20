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

#include <ql/experimental/models/nonstandardswap.hpp>
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

	NonstandardSwap::NonstandardSwap(const VanillaSwap& fromVanilla) :
		Swap(2), type_((NonstandardSwap::Type)fromVanilla.type()), fixedNominal_(std::vector<Real>(fromVanilla.fixedLeg().size(),fromVanilla.nominal())),
			floatingNominal_(std::vector<Real>(fromVanilla.floatingLeg().size(),fromVanilla.nominal())), fixedSchedule_(fromVanilla.fixedSchedule()), 
			fixedRate_(std::vector<Real>(fromVanilla.fixedLeg().size(),fromVanilla.fixedRate())),
			fixedDayCount_(fromVanilla.fixedDayCount()), floatingSchedule_(fromVanilla.floatingSchedule()), iborIndex_(fromVanilla.iborIndex()),
			spread_(fromVanilla.spread()), floatingDayCount_(fromVanilla.floatingDayCount()), paymentConvention_(fromVanilla.paymentConvention()) {

		init();
	}

    NonstandardSwap::NonstandardSwap(
            Type type,
            const std::vector<Real>& fixedNominal,
            const std::vector<Real>& floatingNominal,
            const Schedule& fixedSchedule,
            const std::vector<Real>& fixedRate,
            const DayCounter& fixedDayCount,
            const Schedule& floatingSchedule,
            const boost::shared_ptr<IborIndex>& iborIndex,
            Spread spread,
            const DayCounter& floatingDayCount,
            boost::optional<BusinessDayConvention> paymentConvention)
    : Swap(2), type_(type), fixedNominal_(fixedNominal), floatingNominal_(floatingNominal),
      fixedSchedule_(fixedSchedule), fixedRate_(fixedRate),
      fixedDayCount_(fixedDayCount),
      floatingSchedule_(floatingSchedule), iborIndex_(iborIndex), spread_(spread),
      floatingDayCount_(floatingDayCount) {

		QL_REQUIRE(fixedNominal.size() == fixedRate.size(),
						"Fixed nominal size (" << fixedNominal.size() << ") does not match fixed rate size (" << fixedRate.size() <<")");

		QL_REQUIRE(fixedNominal.size() == fixedSchedule.size()-1,
						"Fixed nominal size (" << fixedNominal.size() << ") does not match schedule size (" << fixedSchedule.size() << ") - 1");

		QL_REQUIRE(floatingNominal.size() == floatingSchedule.size()-1,
						"Floating nominal size (" << floatingNominal.size() << ") does not match schedule size (" << floatingSchedule.size() << ") - 1");

        if (paymentConvention)
            paymentConvention_ = *paymentConvention;
        else
            paymentConvention_ = floatingSchedule_.businessDayConvention();

		init();

	}

	void NonstandardSwap::init() {

        legs_[0] = FixedRateLeg(fixedSchedule_)
            .withNotionals(fixedNominal_)
            .withCouponRates(fixedRate_, fixedDayCount_)
            .withPaymentAdjustment(paymentConvention_);

        legs_[1] = IborLeg(floatingSchedule_, iborIndex_)
            .withNotionals(floatingNominal_)
            .withPaymentDayCounter(floatingDayCount_)
            .withPaymentAdjustment(paymentConvention_)
            .withSpreads(spread_);
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
            QL_FAIL("Unknown nonstandard-swap type");
        }
    }

    void NonstandardSwap::setupArguments(PricingEngine::arguments* args) const {

        Swap::setupArguments(args);

        NonstandardSwap::arguments* arguments =
            dynamic_cast<NonstandardSwap::arguments*>(args);

        if (!arguments) return ; // swap engine ... // QL_FAIL("argument types do not match");

        arguments->type = type_;
        arguments->fixedNominal = fixedNominal_;
        arguments->floatingNominal = floatingNominal_;
		arguments->fixedRate = fixedRate_;

        const Leg& fixedCoupons = fixedLeg();

        arguments->fixedResetDates = arguments->fixedPayDates =
            std::vector<Date>(fixedCoupons.size());
        arguments->fixedCoupons = std::vector<Real>(fixedCoupons.size());

        for (Size i=0; i<fixedCoupons.size(); ++i) {
            boost::shared_ptr<FixedRateCoupon> coupon =
                boost::dynamic_pointer_cast<FixedRateCoupon>(fixedCoupons[i]);

            arguments->fixedPayDates[i] = coupon->date();
            arguments->fixedResetDates[i] = coupon->accrualStartDate();
            arguments->fixedCoupons[i] = coupon->amount();
        }

        const Leg& floatingCoupons = floatingLeg();

        arguments->floatingResetDates = arguments->floatingPayDates =
            arguments->floatingFixingDates =
            std::vector<Date>(floatingCoupons.size());
        arguments->floatingAccrualTimes =
            std::vector<Time>(floatingCoupons.size());
        arguments->floatingSpreads =
            std::vector<Spread>(floatingCoupons.size());
        arguments->floatingCoupons = std::vector<Real>(floatingCoupons.size());
        for (Size i=0; i<floatingCoupons.size(); ++i) {
            boost::shared_ptr<IborCoupon> coupon =
                boost::dynamic_pointer_cast<IborCoupon>(floatingCoupons[i]);

            arguments->floatingResetDates[i] = coupon->accrualStartDate();
            arguments->floatingPayDates[i] = coupon->date();

            arguments->floatingFixingDates[i] = coupon->fixingDate();
            arguments->floatingAccrualTimes[i] = coupon->accrualPeriod();
            arguments->floatingSpreads[i] = coupon->spread();
            try {
                arguments->floatingCoupons[i] = coupon->amount();
            } catch (Error&) {
                arguments->floatingCoupons[i] = Null<Real>();
            }
        }
    }

    void NonstandardSwap::setupExpired() const {
        Swap::setupExpired();
    }

    void NonstandardSwap::fetchResults(const PricingEngine::results* r) const {

        Swap::fetchResults(r);

    }

    void NonstandardSwap::arguments::validate() const {
        Swap::arguments::validate();
        QL_REQUIRE(fixedNominal.size() == fixedPayDates.size(), "number of fixed leg nominals different from number of payment dates");
        QL_REQUIRE(floatingNominal.size() == floatingPayDates.size(), "number of float leg nominals different from number of payment dates");
        QL_REQUIRE(fixedResetDates.size() == fixedPayDates.size(),
                   "number of fixed start dates different from "
                   "number of fixed payment dates");
        QL_REQUIRE(fixedPayDates.size() == fixedCoupons.size(),
                   "number of fixed payment dates different from "
                   "number of fixed coupon amounts");
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
        QL_REQUIRE(floatingPayDates.size() == floatingCoupons.size(),
                   "number of floating payment dates different from "
                   "number of floating coupon amounts");
    }

    void NonstandardSwap::results::reset() {
        Swap::results::reset();
    }


}

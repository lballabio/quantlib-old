/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
	Copyright (C) 2013 Mehdi Bouassab

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


#include <ql/instruments/crosscurrencyswap.hpp>
#include <ql/cashflows/fixedratecoupon.hpp>
#include <ql/cashflows/iborcoupon.hpp>
#include <ql/cashflows/cashflowvectors.hpp>
#include <ql/cashflows/cashflows.hpp>
#include <ql/cashflows/couponpricer.hpp>
#include <ql/indexes/iborindex.hpp>
#include <ql/termstructures/yieldtermstructure.hpp>




namespace QuantLib {


	crosscurrencyswap::crosscurrencyswap(
		Type type,
		Currency fixedCurrency,
		Real fixedLegnominal,
		const Schedule& fixedSchedule,
		Rate fixedRate,
		const DayCounter& fixedDayCount,
		const boost::shared_ptr<IborIndex>& fixedIborIndex,
		Currency floatCurrency,
		Real floatLegnominal,
		const Schedule& floatSchedule,
		const boost::shared_ptr<IborIndex>& floatIborIndex,
		Spread spread,
		const DayCounter& floatingDayCount,
		boost::optional<BusinessDayConvention> paymentConvention)
		: Swap(2), type_(type), fixedCurrency_(fixedCurrency),Fixedlegnominal_(fixedLegnominal),
		fixedSchedule_(fixedSchedule), fixedRate_(fixedRate),
		fixedDayCount_(fixedDayCount),
		fixedIborIndex_(fixedIborIndex),
		floatCurrency_(floatCurrency),
		Floatlegnominal_(floatLegnominal),
		floatingSchedule_(floatSchedule), floatIborIndex_(floatIborIndex), spread_(spread),
		floatingDayCount_(floatingDayCount) {

			if (paymentConvention)
				paymentConvention_ = *paymentConvention;
			else
				paymentConvention_ = floatingSchedule_.businessDayConvention();

			legs_[0] = FixedRateLeg(fixedSchedule_)
				.withNotionals(Fixedlegnominal_)
				.withCouponRates(fixedRate_, fixedDayCount_)
				.withPaymentAdjustment(paymentConvention_);

			legs_[1] = IborLeg(floatingSchedule_, floatIborIndex_)
				.withNotionals(Floatlegnominal_)
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
			  QL_FAIL("Unknown Cross Currency Swap type");
			}
	}

	void crosscurrencyswap::setupArguments(PricingEngine::arguments* args) const {

		Swap::setupArguments(args);

		crosscurrencyswap::arguments* arguments =
			dynamic_cast<crosscurrencyswap::arguments*>(args);

		if (!arguments)  // it's a swap engine...
			return;

		arguments->type = type_;
		arguments->fixedCurrency=fixedCurrency_;
		arguments->floatCurrency=floatCurrency_;
		arguments->fixedLegnominal=Fixedlegnominal_;
		arguments->floatLegnominal=Floatlegnominal_;

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



	 
	void crosscurrencyswap::arguments::validate() const {
        Swap::arguments::validate();
        QL_REQUIRE(fixedLegnominal != Null<Real>(),  "fixed leg nominal null or not set");
		QL_REQUIRE(floatLegnominal != Null<Real>(),  "float leg nominal null or not set");
		QL_REQUIRE(!(fixedCurrency.empty()), "fixed leg currency null or not set");
		QL_REQUIRE(!(floatCurrency.empty()), "float leg currency null or not set");


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

    void crosscurrencyswap::results::reset() {
        Swap::results::reset();
        fairRate = Null<Rate>();
        fairSpread = Null<Spread>();
    }
 
	Rate crosscurrencyswap::fairRate() const {
		calculate();
		QL_REQUIRE(fairRate_ != Null<Rate>(), "result not available");
		return fairRate_;
	}

	Spread crosscurrencyswap::fairSpread() const {
		calculate();
		QL_REQUIRE(fairSpread_ != Null<Spread>(), "result not available");
		return fairSpread_;
	}

	Real crosscurrencyswap::fixedLegBPS() const {
		calculate();
		QL_REQUIRE(legBPS_[0] != Null<Real>(), "result not available");
		return legBPS_[0];
	}

	Real crosscurrencyswap::floatingLegBPS() const {
		calculate();
		QL_REQUIRE(legBPS_[1] != Null<Real>(), "result not available");
		return legBPS_[1];
	}

	Real crosscurrencyswap::fixedLegNPV() const {
		calculate();
		QL_REQUIRE(legNPV_[0] != Null<Real>(), "result not available");
		return legNPV_[0];
	}

	Real crosscurrencyswap::floatingLegNPV() const {
		calculate();
		QL_REQUIRE(legNPV_[1] != Null<Real>(), "result not available");
		return legNPV_[1];
	}

	void crosscurrencyswap::setupExpired() const {
		Swap::setupExpired();
		legBPS_[0] = legBPS_[1] = 0.0;
		fairRate_ = Null<Rate>();
		fairSpread_ = Null<Spread>();
	}

	void crosscurrencyswap::fetchResults(const PricingEngine::results* r) const {
		static const Spread basisPoint = 1.0e-4;

		Swap::fetchResults(r);

		const crosscurrencyswap::results* results =
			dynamic_cast<const crosscurrencyswap::results*>(r);
		if (results) { // might be a swap engine, so no error is thrown
			fairRate_ = results->fairRate;
			fairSpread_ = results->fairSpread;
		} else {
			fairRate_ = Null<Rate>();
			fairSpread_ = Null<Spread>();
		}

		if (fairRate_ == Null<Rate>()) {
			// calculate it from other results
			//Instead of NPV calculate the NPV from legs
			//compare converted_npv to NPV 

			Real converted_npv= fixedLegNPV()+floatingLegNPV();


			if (legBPS_[0] != Null<Real>())
				fairRate_ = fixedRate_ - NPV_/(legBPS_[0]/basisPoint);
		}
		if (fairSpread_ == Null<Spread>()) {
			// ditto
			if (legBPS_[1] != Null<Real>())
				fairSpread_ = spread_ - NPV_/(legBPS_[1]/basisPoint);
		}
	}

	std::ostream& operator<<(std::ostream& out,
		crosscurrencyswap::Type t){

			switch(t){
				case crosscurrencyswap::Receiver :
					return out << "Receiver";
				case crosscurrencyswap::Payer :
					return out << "Payer";
				default:
					QL_FAIL("unknown crosscurrencyswap::Type(" << Integer(t) << ")");
			}
	}




}



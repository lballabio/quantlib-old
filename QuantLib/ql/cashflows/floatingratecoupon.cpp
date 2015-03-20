/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2007 Giorgio Facchinetti
 Copyright (C) 2006, 2007 Cristina Duminuco
 Copyright (C) 2006 Ferdinando Ametrano
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl
 Copyright (C) 2003, 2004 StatPro Italia srl
 Copyright (C) 2003 Nicolas Di Césaré

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

#include <ql/cashflows/floatingratecoupon.hpp>
#include <ql/indexes/interestrateindex.hpp>
#include <ql/cashflows/couponpricer.hpp>
#include <ql/termstructures/yieldtermstructure.hpp>

namespace QuantLib {

    FloatingRateCoupon::FloatingRateCoupon(
                            const Date& paymentDate,
                            Real nominal,
                            const Date& startDate,
                            const Date& endDate,
                            Natural fixingDays,
                            const boost::shared_ptr<InterestRateIndex>& index,
                            Real gearing,
                            Spread spread,
                            const Date& refPeriodStart,
                            const Date& refPeriodEnd,
                            const DayCounter& dayCounter,
                            bool isInArrears)
    : Coupon(paymentDate, nominal,
             startDate, endDate, refPeriodStart, refPeriodEnd),
        indexes_(std::vector<boost::shared_ptr<InterestRateIndex> >(1,index)), dayCounter_(dayCounter),
      fixingDays_(fixingDays==Null<Natural>() ? index->fixingDays() : fixingDays),
        gearings_(std::vector<Real>(1,gearing)), spread_(spread),
      isInArrears_(isInArrears)
    {
        QL_REQUIRE(gearing_!=0, "Null gearing not allowed");

        if (dayCounter_.empty())
            dayCounter_ = indexes_[0]->dayCounter();

        registerWith(indexes_[0]);
        registerWith(Settings::instance().evaluationDate());
    }

    FloatingRateCoupon(
        const Date &paymentDate, Real nominal, const Date &startDate,
        const Date &endDate, Natural fixingDays,
        const std::vector<boost::shared_ptr<InterestRateIndex>> &indexes,
        const std::vector<Real> gearings, Spread spread,
        const Date &refPeriodStart, const Date &refPeriodEnd,
        const DayCounter &dayCounter, bool isInArrears)
        : Coupon(paymentDate, nominal, startDate, endDate, refPeriodStart,
                 refPeriodEnd),
          indexes_(indexes), dayCounter_(dayCounter), {

        QL_REQUIRE(indexes_.size() > 0, "at least one index must be given");
        QL_REQIURE(indexes_.size() == gearings_.size(),
                   "number of indexes (" << indexes_.size() <
                       ")"
                       "must be equal to number of gearings ("
                           << gearings_.size() << ")");

        for (Size i = 1; i < indexes_.size(); ++i) {
            QL_REQUIRE(indexes_[i]->fixingDays() == indexes_[0]->fixingDays(),
                       "all indexes must have the same number of fixing days");
            QL_REQUIRE(indexes_[i]->fixingCalendar() == indexes_[0]->fixingCalendar(),
                       "all indexes must have the same fixing calendar");
            QL_REQUIRE(indexes_[i]->dayCounter() == indexes_[0]->dayCounter(),
                       "all indexes must have the same day counter");
        }

        if (fixingDays == Null<Natural>())
            fixingDays_ = indexes_[0]->fixingDays();
        else
            fixingDays_ = fixingDays;

        if (dayCounter_.empty())
            dayCounter_ = indexes_[0]->dayCounter();

        for (Size i = 0; i < indexes_.size(); ++i)
            registerWith(indexes_[i]);

        registerWith(Settings::instance().evaluationDate());
    }

    void FloatingRateCoupon::setPricer(
                const boost::shared_ptr<FloatingRateCouponPricer>& pricer) {
        if (pricer_)
            unregisterWith(pricer_);
        pricer_ = pricer;
        if (pricer_)
            registerWith(pricer_);
        update();
    }

    Real FloatingRateCoupon::accruedAmount(const Date& d) const {
        if (d <= accrualStartDate_ || d > paymentDate_) {
            return 0.0;
        } else {
            return nominal() * rate() *
                dayCounter().yearFraction(accrualStartDate_,
                                          std::min(d, accrualEndDate_),
                                          refPeriodStart_,
                                          refPeriodEnd_);
        }
    }

    Date FloatingRateCoupon::fixingDate() const {
        // if isInArrears_ fix at the end of period
        Date refDate = isInArrears_ ? accrualEndDate_ : accrualStartDate_;
        return indexes_[0]->fixingCalendar().advance(refDate,
            -static_cast<Integer>(fixingDays_), Days, Preceding);
    }

    Rate FloatingRateCoupon::rate() const {
        QL_REQUIRE(pricer_, "pricer not set");
        pricer_->initialize(*this);
        return pricer_->swapletRate();
    }

    Real FloatingRateCoupon::price(const Handle<YieldTermStructure>& discountingCurve) const {
        return amount() * discountingCurve->discount(date());
    }

    Rate FloatingRateCoupon::indexFixing() const {
        Real fixing = 0.0;
        for(Size i=0; i<indexes_.size(); ++i)
            fixing += gearings_[i]*indexes_[i]->fixing(fixingDate());
        return fixing;
    }

}

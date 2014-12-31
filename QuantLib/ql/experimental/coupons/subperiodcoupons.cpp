/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2008 Toyin Akin

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

#include <ql/experimental/coupons/subperiodcoupons.hpp>
#include <ql/cashflows/cashflowvectors.hpp>
#include <ql/time/schedule.hpp>
#include <ql/indexes/iborindex.hpp>
#include <ql/termstructures/yieldtermstructure.hpp>
#include <cmath>

namespace QuantLib {

    SubPeriodsCoupon::SubPeriodsCoupon(
                                    const Date& paymentDate,
                                    Real nominal,
                                    const boost::shared_ptr<IborIndex>& index,
                                    const Date& startDate,
                                    const Date& endDate,
                                    Natural fixingDays,
                                    const DayCounter& dayCounter,
                                    Real gearing,
                                    Rate couponSpread,
                                    Rate rateSpread,
                                    const Date& refPeriodStart,
                                    const Date& refPeriodEnd)
    : FloatingRateCoupon(paymentDate, nominal, startDate, endDate,
                         fixingDays, index, gearing, couponSpread,
                         refPeriodStart, refPeriodEnd, dayCounter),
      rateSpread_(rateSpread) {
        const Handle<YieldTermStructure>& rateCurve =
            index->forwardingTermStructure();
        const Date& referenceDate = rateCurve->referenceDate();

        observationsSchedule_ = boost::shared_ptr<Schedule>(new
            Schedule(startDate, endDate,
                     index->tenor(),
                     NullCalendar(),
                     Unadjusted,
                     Unadjusted,
                     DateGeneration::Forward,
                     false));

        observationDates_ = observationsSchedule_->dates();
        observationDates_.pop_back();                       //remove end date
        observations_ = observationDates_.size();

        startTime_ = dayCounter.yearFraction(referenceDate, startDate);
        endTime_ = dayCounter.yearFraction(referenceDate, endDate);

        for (Size i=0; i<observations_; ++i) {
            observationTimes_.push_back(
                dayCounter.yearFraction(referenceDate, observationDates_[i]));
        }
     }

    void SubPeriodsCoupon::accept(AcyclicVisitor& v) {
        Visitor<SubPeriodsCoupon>* v1 =
            dynamic_cast<Visitor<SubPeriodsCoupon>*>(&v);
        if (v1 != 0)
            v1->visit(*this);
        else
            FloatingRateCoupon::accept(v);
    }



}


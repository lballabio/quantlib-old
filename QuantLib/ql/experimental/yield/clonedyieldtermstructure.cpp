/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2015 Peter Caspers

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

#include <ql/experimental/yield/clonedyieldtermstructure.hpp>
#include <ql/math/interpolations/loginterpolation.hpp>

#include <boost/make_shared.hpp>

namespace QuantLib {

ClonedYieldTermStructure::ClonedYieldTermStructure(
    const boost::shared_ptr<YieldTermStructure> &source,
    const ReactionToTimeDecay reactionToTimeDecay, const Processing processing,
    const Calendar calendar)
    : YieldTermStructure(source->dayCounter()),
      reactionToTimeDecay_(reactionToTimeDecay), processing_(processing),
      originalEvalDate_(Settings::instance().evaluationDate()),
      originalReferenceDate_(Date(source->referenceDate())),
      originalMaxDate_(source->maxDate()) {

    calendar_ = calendar.empty() ? source->calendar() : calendar;
    QL_REQUIRE(!calendar_.empty() || reactionToTimeDecay_ == FixedReferenceDate,
               "a floating termstructure needs a calendar, none given and "
               "source termstructures' calendar is empty, too");

    referenceDate_ = originalReferenceDate_;
    maxDate_ = originalMaxDate_;
    offset_ = 0.0;
    valid_ = true;

    instFwdMax_ =
        source->forwardRate(maxDate_, maxDate_, Actual365Fixed(), Continuous)
            .rate();

    if (reactionToTimeDecay != FixedReferenceDate) {
        QL_REQUIRE(originalReferenceDate_ >= originalEvalDate_,
                   "to construct a moving term structure the source term "
                   "structure must have a reference date ("
                       << originalReferenceDate_
                       << ") after the evaluation date ("
                       << originalEvalDate_
                       << ")");
        try {
            impliedSettlementDays_ = source->settlementDays();
        } catch (...) {
            // if the source ts has no settlement days we imply
            // them from the difference of the original reference
            // date and the original evaluation date
            impliedSettlementDays_ = this->calendar().businessDaysBetween(
                originalEvalDate_, originalReferenceDate_);
        }
    }

    logDiscounts_.resize(originalMaxDate_.serialNumber() -
                      originalReferenceDate_.serialNumber() + 1);
    times_.resize(logDiscounts_.size());

    for (BigInteger i = 0; i <= originalMaxDate_.serialNumber() -
                                    originalReferenceDate_.serialNumber();
         ++i) {
        Date d = Date(originalReferenceDate_.serialNumber() + i);
        logDiscounts_[i] = std::log(source->discount(d));
        times_[i] = timeFromReference(d);
        if (processing == PositiveYieldsAndForwards) {
            logDiscounts_[i] = std::min(0.0, logDiscounts_[i]);
        }
        if (processing == PositiveForwards ||
            processing == PositiveYieldsAndForwards) {
            if (i > 0)
                logDiscounts_[i] = std::min(logDiscounts_[i - 1], logDiscounts_[i]);
        }
    }

    if (reactionToTimeDecay_ != FixedReferenceDate) {
        registerWith(Settings::instance().evaluationDate());
    }
}

DiscountFactor ClonedYieldTermStructure::discountImpl(Time t) const {
    QL_REQUIRE(valid_, "termstructure not valid, evaluation date ("
                           << Settings::instance().evaluationDate()
                           << ") is before the evaluation date when the "
                              "termstructure was frozen ("
                           << originalEvalDate_);
    Time tMax = maxTime();
    Time tEff = t + offset_;
    if (tEff < tMax) {
        // also ok for offset_ = 0
        return interpolate(tEff) /
               interpolate(offset_);
    }

    // flat fwd extrapolation
    DiscountFactor dMax = std::exp(logDiscounts_.back());
    return dMax * std::exp(-instFwdMax_ * (tEff - tMax));
}

void ClonedYieldTermStructure::update() {
    YieldTermStructure::update();
    if (reactionToTimeDecay_ != FixedReferenceDate) {
        Date today = Settings::instance().evaluationDate();
        if (today < originalEvalDate_) {
            valid_ = false;
        } else {
            valid_ = true;
            referenceDate_ =
                calendar().advance(today, impliedSettlementDays_ * Days);
            if (reactionToTimeDecay_ == ForwardForward) {
                offset_ = dayCounter().yearFraction(originalReferenceDate_,
                                                    referenceDate_);
            }
            if (reactionToTimeDecay_ == ConstantZeroYields) {
                BigNatural dayOffset = referenceDate_ - originalReferenceDate_;
                maxDate_ = Date(std::min<BigNatural>(
                    originalMaxDate_.serialNumber() + dayOffset,
                    Date::maxDate().serialNumber()));
            }
        }
    }
}

} // namespace QuantLib

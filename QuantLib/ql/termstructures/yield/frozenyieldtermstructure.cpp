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

#include <ql/termstructures/yield/frozenyieldtermstructure.hpp>
#include <ql/math/interpolations/loginterpolation.hpp>

#include <boost/make_shared.hpp>

namespace QuantLib {

FrozenYieldTermStructure::FrozenYieldTermStructure(
    const boost::shared_ptr<YieldTermStructure> source,
    const ReactionToTimeDecay reactionToTimeDecay)
    : YieldTermStructure(source->dayCounter(), source->jumps(),
                         source->jumpDates()),
      reactionToTimeDecay_(reactionToTimeDecay),
      originalEvalDate_(Settings::instance().evaluationDate()),
      originalReferenceDate_(source->referenceDate()),
      originalMaxDate_(source->maxDate()) {

    referenceDate_ = originalReferenceDate_;
    maxDate_ = originalMaxDate_;
    offset_ = 0.0;
    valid_ = true;

    if (reactionToTimeDecay != FixedReferenceDate) {
        QL_REQUIRE(originalReferenceDate_ >= originalEvalDate_,
                   "to construct a moving term structure the source term "
                   "structure must have a reference date ("
                       << originalReferenceDate_
                       << ") after the evaluation date ("
                       << originalEvalDate_
                       << ")");
    }

    discounts_.resize(originalMaxDate_.serialNumber() -
                      originalReferenceDate_.serialNumber());
    times_.resize(discounts_.size());

    for (BigInteger i = originalReferenceDate_.serialNumber();
         i <= originalMaxDate_.serialNumber(); ++i) {
        Date d = Date(i);
        discounts_[i] = source->discount(d);
        times_[i] = timeFromReference(d);
    }

    interpolation_ = boost::make_shared<LogLinearInterpolation>(
        times_.begin(), times_.end(), discounts_.begin());
    interpolation_->update();

    if (reactionToTimeDecay_ != FixedReferenceDate) {
        registerWith(Settings::instance().evaluationDate());
    }
}

DiscountFactor FrozenYieldTermStructure::discountImpl(Time t) const {
    QL_REQUIRE(valid_, "termstructure not valid, evaluation date ("
                           << Settings::instance().evaluationDate()
                           << ") is before the evaluation date when the "
                              "termstructure was frozen ("
                           << originalEvalDate_);
    Time tMax = maxTime();
    Time tEff = t + offset_;
    if (tEff < tMax) {
        // also ok for offset_ = 0
        return interpolation_->operator()(tEff) /
               interpolation_->operator()(offset_);
    }

    // flat fwd extrapolation
    DiscountFactor dMax = discounts_.back();
    Rate instFwdMax = -interpolation_->derivative(tMax) / dMax;
    return dMax * std::exp(-instFwdMax * (tEff - tMax));
}

void FrozenYieldTermStructure::update() {
    if (reactionToTimeDecay_ != FixedReferenceDate) {
        Date today = Settings::instance().evaluationDate();
        if (today < originalEvalDate_) {
            valid_ = false;
        } else {
            valid_ = true;
            referenceDate_ = today + (originalReferenceDate_.serialNumber() -
                                      originalEvalDate_.serialNumber());
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

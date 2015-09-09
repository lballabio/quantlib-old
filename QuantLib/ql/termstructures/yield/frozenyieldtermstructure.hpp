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

/*! \file frozenyieldtermstructure.hpp
    \brief yield term structure with freezed rates
*/

#ifndef quantlib_freezed_yts_hpp
#define quantlib_freezed_yts_hpp

#include <ql/termstructures/yieldtermstructure.hpp>
#include <ql/math/interpolation.hpp>

namespace QuantLib {

/*! This class reproduces the source yield term structure and freezes
    it's discount factors on a daily basis until the maximum date.
    Extrapolation beyond this date is done assuming a constant
    instantaneous forward rate from the maximum date on.
    If intra day discount factors are requested (via Time based
    inspectors) a loglinear interpolation between the daily values
    is applied. The reference date of the copy can be fixed or moving
    with the evaluation date, assuming constant zero yields or
    rolling down the foward curve. */

class FrozenYieldTermStructure : public YieldTermStructure {
  public:
    enum ReactionToTimeDecay {
        FixedReferenceDate,
        ConstantZeroYields,
        ForwardForward
    };

    FrozenYieldTermStructure(
        const boost::shared_ptr<YieldTermStructure> source,
        const ReactionToTimeDecay reactionToTimeDecay = FixedReferenceDate);

    //! Observer interface
    void update();

    //! TermStructure interface
    const Date &referenceDate() const;
    Date maxDate() const;

  protected:
    //! YieldTermStructure interface
    DiscountFactor discountImpl(Time t) const;

  private:
    const ReactionToTimeDecay reactionToTimeDecay_;
    const Date originalEvalDate_, originalReferenceDate_, originalMaxDate_;
    boost::shared_ptr<Interpolation> interpolation_;
    std::vector<Time> times_;
    std::vector<Real> discounts_;
    Date referenceDate_, maxDate_;
    Time offset_;
    bool valid_;
};

// inline

inline Date FrozenYieldTermStructure::maxDate() const { return maxDate_; }

inline const Date &FrozenYieldTermStructure::referenceDate() const {
    return referenceDate_;
}

} // namespace QuantLib

#endif

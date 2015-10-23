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

/*! \file clonedyieldtermstructure.hpp
    \brief yield term structure that clones a source yield term structure
*/

#ifndef quantlib_freezed_yts_hpp
#define quantlib_freezed_yts_hpp

#include <ql/termstructures/yieldtermstructure.hpp>
#include <ql/math/interpolation.hpp>

namespace QuantLib {

/*! This class reproduces the source yield term structure and freezes
    it's discount factors on a daily basis until the maximum date.
    Extrapolation beyond this date is done assuming a constant
    instantaneous forward rate from the maximum date on (which is
    read from the source term structure).
    If intra day discount factors are requested (via Time based
    inspectors) a loglinear interpolation between the daily values
    is applied.
    The reference date of the copy can be fixed or moving
    with the evaluation date, assuming constant zero yields or
    rolling down the foward curve.
    The input yields can be processed to ensure positive
    instantaneous forwards. In addtion positive yields can be
    ensured.

    Warning: An instance of this class can produce a considerable
    memory footprint depending on the maxDate() of the source curve.
*/

class ClonedYieldTermStructure : public YieldTermStructure {
  public:
    enum ReactionToTimeDecay {
        FixedReferenceDate,
        ConstantZeroYields,
        ForwardForward
    };
    enum Processing { None, PositiveForwards, PositiveYieldsAndForwards };

    ClonedYieldTermStructure(
        const boost::shared_ptr<YieldTermStructure> &source,
        const ReactionToTimeDecay reactionToTimeDecay = FixedReferenceDate,
        const Processing processing = None,
        const Calendar calendar = Calendar());

    //! Observer interface
    void update();

    //! TermStructure interface
    const Date &referenceDate() const;
    Date maxDate() const;

  protected:
    //! YieldTermStructure interface
    DiscountFactor discountImpl(Time t) const;

  private:
    const Real interpolate(const Time t) const;
    const ReactionToTimeDecay reactionToTimeDecay_;
    const Processing processing_;
    const Date originalEvalDate_, originalReferenceDate_, originalMaxDate_;
    Natural impliedSettlementDays_;
    Real instFwdMax_;
    boost::shared_ptr<Interpolation> interpolation_;
    std::vector<Time> times_;
    std::vector<Real> logDiscounts_;
    Date referenceDate_, maxDate_;
    Time offset_;
    bool valid_;
};

// inline

inline Date ClonedYieldTermStructure::maxDate() const { return maxDate_; }

inline const Date &ClonedYieldTermStructure::referenceDate() const {
    return referenceDate_;
}

inline const Real ClonedYieldTermStructure::interpolate(const Real t) const {
    // we assume that the input fulfills 0 <= t < tmax
    // we do the interpolation by ourselves to avoid
    // two more vectors in the interpolation object
    // which would increase the memory footprint
    std::vector<Real>::const_iterator i =
        std::upper_bound(times_.begin(), times_.end(), t);
    Size idx = i - times_.begin();
    Real t0 = *(i - 1);
    Real t1 = *i;
    Real x0 = logDiscounts_[idx - 1];
    Real x1 = logDiscounts_[idx];
    return std::exp((x1 * (t - t0) + x0 * (t1 - t)) / (t1 - t0));
}

} // namespace QuantLib

#endif

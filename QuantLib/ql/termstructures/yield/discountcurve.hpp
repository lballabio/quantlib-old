/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2002, 2003 Decillion Pty(Ltd)
 Copyright (C) 2005, 2006, 2008, 2009 StatPro Italia srl
 Copyright (C) 2009 Ferdinando Ametrano

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

/*! \file discountcurve.hpp
    \brief interpolated discount factor structure
*/

#ifndef quantlib_discount_curve_hpp
#define quantlib_discount_curve_hpp

#include <ql/termstructures/yieldtermstructure.hpp>
#include <ql/termstructures/interpolatedcurve.hpp>
#include <ql/math/interpolations/loginterpolation.hpp>
#include <ql/math/comparison.hpp>
#include <utility>

namespace QuantLib {

//! YieldTermStructure based on interpolation of discount factors
/*! \ingroup yieldtermstructures */
template <template <class> class Interpolator, class T = Real>
class InterpolatedDiscountCurve_t
    : public YieldTermStructure_t<T>,
      protected InterpolatedCurve_t<Interpolator, T> {
  public:
    InterpolatedDiscountCurve_t(
        const std::vector<Date> &dates, const std::vector<T> &dfs,
        const DayCounter &dayCounter, const Calendar &cal = Calendar(),
        const std::vector<Handle<Quote_t<T> > > &jumps =
            std::vector<Handle<Quote_t<T> > >(),
        const std::vector<Date> &jumpDates = std::vector<Date>(),
        const Interpolator<T> &interpolator = Interpolator<T>());
    InterpolatedDiscountCurve_t(const std::vector<Date> &dates,
                                const std::vector<T> &dfs,
                                const DayCounter &dayCounter,
                                const Calendar &calendar,
                                const Interpolator<T> &interpolator);
    InterpolatedDiscountCurve_t(const std::vector<Date> &dates,
                                const std::vector<T> &dfs,
                                const DayCounter &dayCounter,
                                const Interpolator<T> &interpolator);
    //! \name TermStructure interface
    //@{
    Date maxDate() const;
    //@}
    //! \name other inspectors
    //@{
    const std::vector<Time> &times() const;
    const std::vector<Date> &dates() const;
    const std::vector<T> &data() const;
    const std::vector<T> &discounts() const;
    std::vector<std::pair<Date, T> > nodes() const;
    //@}
  protected:
    InterpolatedDiscountCurve_t(
        const DayCounter &, const std::vector<Handle<Quote_t<T> > > &jumps =
                                std::vector<Handle<Quote_t<T> > >(),
        const std::vector<Date> &jumpDates = std::vector<Date>(),
        const Interpolator<T> &interpolator = Interpolator<T>());
    InterpolatedDiscountCurve_t(
        const Date &referenceDate, const DayCounter &,
        const std::vector<Handle<Quote_t<T> > > &jumps =
            std::vector<Handle<Quote_t<T> > >(),
        const std::vector<Date> &jumpDates = std::vector<Date>(),
        const Interpolator<T> &interpolator = Interpolator<T>());
    InterpolatedDiscountCurve_t(
        Natural settlementDays, const Calendar &, const DayCounter &,
        const std::vector<Handle<Quote_t<T> > > &jumps =
            std::vector<Handle<Quote_t<T> > >(),
        const std::vector<Date> &jumpDates = std::vector<Date>(),
        const Interpolator<T> &interpolator = Interpolator<T>());
    //! \name YieldTermStructure implementation
    //@{
    T discountImpl(Time) const;
    //@}
    mutable std::vector<Date> dates_;

  private:
    void initialize();
};

//! Term structure based on log-linear interpolation of discount factors
/*! Log-linear interpolation guarantees piecewise-constant forward
    rates.

    \ingroup yieldtermstructures
*/
typedef InterpolatedDiscountCurve_t<LogLinear, Real> DiscountCurve;

// inline definitions

template <template <class> class Interpolator, class T>
inline Date InterpolatedDiscountCurve_t<Interpolator, T>::maxDate() const {
    return dates_.back();
}

template <template <class> class Interpolator, class T>
inline const std::vector<Time> &
InterpolatedDiscountCurve_t<Interpolator, T>::times() const {
    return this->times_;
}

template <template <class> class Interpolator, class T>
inline const std::vector<Date> &
InterpolatedDiscountCurve_t<Interpolator, T>::dates() const {
    return dates_;
}

template <template <class> class Interpolator, class T>
inline const std::vector<T> &
InterpolatedDiscountCurve_t<Interpolator, T>::data() const {
    return this->data_;
}

template <template <class> class Interpolator, class T>
inline const std::vector<T> &
InterpolatedDiscountCurve_t<Interpolator, T>::discounts() const {
    return this->data_;
}

template <template <class> class Interpolator, class T>
inline std::vector<std::pair<Date, T> >
InterpolatedDiscountCurve_t<Interpolator, T>::nodes() const {
    std::vector<std::pair<Date, T> > results(dates_.size());
    for (Size i = 0; i < dates_.size(); ++i)
        results[i] = std::make_pair(dates_[i], this->data_[i]);
    return results;
}

#ifndef __DOXYGEN__

// template definitions

template <template <class> class Interpolator, class T>
T InterpolatedDiscountCurve_t<Interpolator, T>::discountImpl(Time t) const {
    if (t <= this->times_.back())
        return this->interpolation_(t, true);

    // flat fwd extrapolation
    Time tMax = this->times_.back();
    DiscountFactor dMax = this->data_.back();
    Rate instFwdMax = -this->interpolation_.derivative(tMax) / dMax;
    return dMax * std::exp(-instFwdMax * (t - tMax));
}

template <template <class> class Interpolator, class T>
InterpolatedDiscountCurve_t<Interpolator, T>::InterpolatedDiscountCurve_t(
    const DayCounter &dayCounter,
    const std::vector<Handle<Quote_t<T> > > &jumps,
    const std::vector<Date> &jumpDates, const Interpolator<T> &interpolator)
    : YieldTermStructure_t<T>(dayCounter, jumps, jumpDates),
      InterpolatedCurve_t<Interpolator, T>(interpolator) {}

template <template <class> class Interpolator, class T>
InterpolatedDiscountCurve_t<Interpolator, T>::InterpolatedDiscountCurve_t(
    const Date &referenceDate, const DayCounter &dayCounter,
    const std::vector<Handle<Quote_t<T> > > &jumps,
    const std::vector<Date> &jumpDates, const Interpolator<T> &interpolator)
    : YieldTermStructure_t<T>(referenceDate, Calendar(), dayCounter, jumps,
                              jumpDates),
      InterpolatedCurve_t<Interpolator, T>(interpolator) {}

template <template <class> class Interpolator, class T>
InterpolatedDiscountCurve_t<Interpolator, T>::InterpolatedDiscountCurve_t(
    Natural settlementDays, const Calendar &calendar,
    const DayCounter &dayCounter,
    const std::vector<Handle<Quote_t<T> > > &jumps,
    const std::vector<Date> &jumpDates, const Interpolator<T> &interpolator)
    : YieldTermStructure_t<T>(settlementDays, calendar, dayCounter, jumps,
                              jumpDates),
      InterpolatedCurve_t<Interpolator, T>(interpolator) {}

template <template <class> class Interpolator, class T>
InterpolatedDiscountCurve_t<Interpolator, T>::InterpolatedDiscountCurve_t(
    const std::vector<Date> &dates, const std::vector<T> &discounts,
    const DayCounter &dayCounter, const Calendar &calendar,
    const std::vector<Handle<Quote_t<T> > > &jumps,
    const std::vector<Date> &jumpDates, const Interpolator<T> &interpolator)
    : YieldTermStructure_t<T>(dates.at(0), calendar, dayCounter, jumps,
                              jumpDates),
      InterpolatedCurve_t<Interpolator, T>(std::vector<Time>(), discounts,
                                           interpolator),
      dates_(dates) {
    initialize();
}

template <template <class> class Interpolator, class T>
InterpolatedDiscountCurve_t<Interpolator, T>::InterpolatedDiscountCurve_t(
    const std::vector<Date> &dates, const std::vector<T> &discounts,
    const DayCounter &dayCounter, const Calendar &calendar,
    const Interpolator<T> &interpolator)
    : YieldTermStructure_t<T>(dates.at(0), calendar, dayCounter),
      InterpolatedCurve_t<Interpolator, T>(std::vector<Time>(), discounts,
                                           interpolator),
      dates_(dates) {
    initialize();
}

template <template <class> class Interpolator, class T>
InterpolatedDiscountCurve_t<Interpolator, T>::InterpolatedDiscountCurve_t(
    const std::vector<Date> &dates, const std::vector<T> &discounts,
    const DayCounter &dayCounter, const Interpolator<T> &interpolator)
    : YieldTermStructure_t<T>(dates.at(0), Calendar(), dayCounter),
      InterpolatedCurve_t<Interpolator, T>(std::vector<Time>(), discounts,
                                           interpolator),
      dates_(dates) {
    initialize();
}

#endif

template <template <class> class Interpolator, class T>
void InterpolatedDiscountCurve_t<Interpolator, T>::initialize() {
    QL_REQUIRE(dates_.size() >= Interpolator<T>::requiredPoints,
               "not enough input dates given");
    QL_REQUIRE(this->data_.size() == dates_.size(),
               "dates/data count mismatch");
    QL_REQUIRE(this->data_[0] == 1.0,
               "the first discount must be == 1.0 "
               "to flag the corresponding date as reference date");

    this->times_.resize(dates_.size());
    this->times_[0] = 0.0;
    for (Size i = 1; i < dates_.size(); ++i) {
        QL_REQUIRE(dates_[i] > dates_[i - 1], "invalid date ("
                                                  << dates_[i] << ", vs "
                                                  << dates_[i - 1] << ")");
        this->times_[i] = this->dayCounter().yearFraction(dates_[0], dates_[i]);
        QL_REQUIRE(!close(this->times_[i], this->times_[i - 1]),
                   "two dates correspond to the same time "
                   "under this curve's day count convention");
        QL_REQUIRE(this->data_[i] > 0.0, "negative discount");
#if !defined(QL_NEGATIVE_RATES)
        QL_REQUIRE(this->data_[i] <= this->data_[i - 1],
                   "negative forward rate implied by the discount "
                       << this->data_[i] << " at " << dates_[i]
                       << " (t=" << this->times_[i] << ") after the discount "
                       << this->data_[i - 1] << " at " << dates_[i - 1]
                       << " (t=" << this->times_[i - 1] << ")");
#endif
    }

    this->interpolation_ = this->interpolator_.interpolate(
        this->times_.begin(), this->times_.end(), this->data_.begin());
    this->interpolation_.update();
}

template <template <class> class Interpolator>
struct InterpolatedDiscountCurve {
    typedef InterpolatedDiscountCurve_t<Interpolator, Real> Type;
};

} // namespace QuantLib

#endif

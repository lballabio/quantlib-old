/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2005, 2006, 2007, 2008, 2009 StatPro Italia srl
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

/*! \file forwardcurve.hpp
    \brief interpolated forward-rate structure
*/

#ifndef quantlib_forward_curve_hpp
#define quantlib_forward_curve_hpp

#include <ql/termstructures/yield/forwardstructure.hpp>
#include <ql/termstructures/interpolatedcurve.hpp>
#include <ql/math/interpolations/backwardflatinterpolation.hpp>
#include <ql/math/comparison.hpp>
#include <utility>

namespace QuantLib {

//! YieldTermStructure based on interpolation of forward rates
/*! \ingroup yieldtermstructures */
template <template <class> class Interpolator, class T = Real>
class InterpolatedForwardCurve_t
    : public ForwardRateStructure_t<T>,
      protected InterpolatedCurve_t<Interpolator, T> {
  public:
    // constructor
    InterpolatedForwardCurve_t(
        const std::vector<Date> &dates, const std::vector<T> &forwards,
        const DayCounter &dayCounter, const Calendar &cal = Calendar(),
        const std::vector<Handle<Quote_t<T> > > &jumps =
            std::vector<Handle<Quote_t<T> > >(),
        const std::vector<Date> &jumpDates = std::vector<Date>(),
        const Interpolator<T> &interpolator = Interpolator<T>());
    InterpolatedForwardCurve_t(const std::vector<Date> &dates,
                               const std::vector<T> &forwards,
                               const DayCounter &dayCounter,
                               const Calendar &calendar,
                               const Interpolator<T> &interpolator);
    InterpolatedForwardCurve_t(const std::vector<Date> &dates,
                               const std::vector<T> &forwards,
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
    const std::vector<Real> &data() const;
    const std::vector<Rate> &forwards() const;
    std::vector<std::pair<Date, Real> > nodes() const;
    //@}
  protected:
    InterpolatedForwardCurve_t(
        const DayCounter &, const std::vector<Handle<Quote_t<T> > > &jumps =
                                std::vector<Handle<Quote_t<T> > >(),
        const std::vector<Date> &jumpDates = std::vector<Date>(),
        const Interpolator<T> &interpolator = Interpolator<T>());
    InterpolatedForwardCurve_t(
        const Date &referenceDate, const DayCounter &,
        const std::vector<Handle<Quote_t<T> > > &jumps =
            std::vector<Handle<Quote_t<T> > >(),
        const std::vector<Date> &jumpDates = std::vector<Date>(),
        const Interpolator<T> &interpolator = Interpolator<T>());
    InterpolatedForwardCurve_t(
        Natural settlementDays, const Calendar &, const DayCounter &,
        const std::vector<Handle<Quote_t<T> > > &jumps =
            std::vector<Handle<Quote_t<T> > >(),
        const std::vector<Date> &jumpDates = std::vector<Date>(),
        const Interpolator<T> &interpolator = Interpolator<T>());
    //! \name ForwardRateStructure implementation
    //@{
    Rate forwardImpl(Time t) const;
    Rate zeroYieldImpl(Time t) const;
    //@}
    mutable std::vector<Date> dates_;

  private:
    void initialize();
};

//! Term structure based on flat interpolation of forward rates
/*! \ingroup yieldtermstructures */

typedef InterpolatedForwardCurve_t<BackwardFlat, Real> ForwardCurve;

// inline definitions

template <template <class> class Interpolator, class T>
inline Date InterpolatedForwardCurve_t<Interpolator, T>::maxDate() const {
    return dates_.back();
}

template <template <class> class Interpolator, class T>
inline const std::vector<Time> &
InterpolatedForwardCurve_t<Interpolator, T>::times() const {
    return this->times_;
}

template <template <class> class Interpolator, class T>
inline const std::vector<Date> &
InterpolatedForwardCurve_t<Interpolator, T>::dates() const {
    return dates_;
}

template <template <class> class Interpolator, class T>
inline const std::vector<Real> &
InterpolatedForwardCurve_t<Interpolator, T>::data() const {
    return this->data_;
}

template <template <class> class Interpolator, class T>
inline const std::vector<Rate> &
InterpolatedForwardCurve_t<Interpolator, T>::forwards() const {
    return this->data_;
}

template <template <class> class Interpolator, class T>
inline std::vector<std::pair<Date, Real> >
InterpolatedForwardCurve_t<Interpolator, T>::nodes() const {
    std::vector<std::pair<Date, Real> > results(dates_.size());
    for (Size i = 0; i < dates_.size(); ++i)
        results[i] = std::make_pair(dates_[i], this->data_[i]);
    return results;
}

#ifndef __DOXYGEN__

// template definitions

template <template <class> class Interpolator, class T>
Rate InterpolatedForwardCurve_t<Interpolator, T>::forwardImpl(Time t) const {
    if (t <= this->times_.back())
        return this->interpolation_(t, true);

    // flat fwd extrapolation
    return this->data_.back();
}

template <template <class> class Interpolator, class T>
Rate InterpolatedForwardCurve_t<Interpolator, T>::zeroYieldImpl(Time t) const {
    if (t == 0.0)
        return forwardImpl(0.0);

    Real integral;
    if (t <= this->times_.back()) {
        integral = this->interpolation_.primitive(t, true);
    } else {
        // flat fwd extrapolation
        integral = this->interpolation_.primitive(this->times_.back(), true) +
                   this->data_.back() * (t - this->times_.back());
    }
    return integral / t;
}

template <template <class> class Interpolator, class T>
InterpolatedForwardCurve_t<Interpolator, T>::InterpolatedForwardCurve_t(
    const DayCounter &dayCounter,
    const std::vector<Handle<Quote_t<T> > > &jumps,
    const std::vector<Date> &jumpDates, const Interpolator<T> &interpolator)
    : ForwardRateStructure_t<T>(dayCounter, jumps, jumpDates),
      InterpolatedCurve_t<Interpolator, T>(interpolator) {}

template <template <class> class Interpolator, class T>
InterpolatedForwardCurve_t<Interpolator, T>::InterpolatedForwardCurve_t(
    const Date &referenceDate, const DayCounter &dayCounter,
    const std::vector<Handle<Quote_t<T> > > &jumps,
    const std::vector<Date> &jumpDates, const Interpolator<T> &interpolator)
    : ForwardRateStructure_t<T>(referenceDate, Calendar(), dayCounter, jumps,
                                jumpDates),
      InterpolatedCurve_t<Interpolator, T>(interpolator) {}

template <template <class> class Interpolator, class T>
InterpolatedForwardCurve_t<Interpolator, T>::InterpolatedForwardCurve_t(
    Natural settlementDays, const Calendar &calendar,
    const DayCounter &dayCounter,
    const std::vector<Handle<Quote_t<T> > > &jumps,
    const std::vector<Date> &jumpDates, const Interpolator<T> &interpolator)
    : ForwardRateStructure_t<T>(settlementDays, calendar, dayCounter, jumps,
                                jumpDates),
      InterpolatedCurve_t<Interpolator, T>(interpolator) {}

template <template <class> class Interpolator, class T>
InterpolatedForwardCurve_t<Interpolator, T>::InterpolatedForwardCurve_t(
    const std::vector<Date> &dates, const std::vector<T> &forwards,
    const DayCounter &dayCounter, const Calendar &calendar,
    const std::vector<Handle<Quote_t<T> > > &jumps,
    const std::vector<Date> &jumpDates, const Interpolator<T> &interpolator)
    : ForwardRateStructure_t<T>(dates.at(0), calendar, dayCounter, jumps,
                                jumpDates),
      InterpolatedCurve_t<Interpolator, T>(std::vector<Time>(), forwards,
                                           interpolator),
      dates_(dates) {
    initialize();
}

template <template <class> class Interpolator, class T>
InterpolatedForwardCurve_t<Interpolator, T>::InterpolatedForwardCurve_t(
    const std::vector<Date> &dates, const std::vector<T> &forwards,
    const DayCounter &dayCounter, const Calendar &calendar,
    const Interpolator<T> &interpolator)
    : ForwardRateStructure_t<T>(dates.at(0), calendar, dayCounter),
      InterpolatedCurve_t<Interpolator, T>(std::vector<Time>(), forwards,
                                           interpolator),
      dates_(dates) {
    initialize();
}

template <template <class> class Interpolator, class T>
InterpolatedForwardCurve_t<Interpolator, T>::InterpolatedForwardCurve_t(
    const std::vector<Date> &dates, const std::vector<T> &forwards,
    const DayCounter &dayCounter, const Interpolator<T> &interpolator)
    : ForwardRateStructure(dates.at(0), Calendar(), dayCounter),
      InterpolatedCurve_t<Interpolator, T>(std::vector<Time>(), forwards,
                                           interpolator),
      dates_(dates) {
    initialize();
}

#endif

template <template <class> class Interpolator, class T>
void InterpolatedForwardCurve_t<Interpolator, T>::initialize() {
    QL_REQUIRE(dates_.size() >= Interpolator<T>::requiredPoints,
               "not enough input dates given");
    QL_REQUIRE(this->data_.size() == dates_.size(),
               "dates/data count mismatch");

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
#if !defined(QL_NEGATIVE_RATES)
        QL_REQUIRE(this->data_[i] >= 0.0, "negative forward");
#endif
    }

    this->interpolation_ = this->interpolator_.interpolate(
        this->times_.begin(), this->times_.end(), this->data_.begin());
    this->interpolation_.update();
}

template <template <class> class Interpolator> struct InterpolatedForwardCurve {
    typedef InterpolatedForwardCurve_t<Interpolator, Real> Type;
};

} // namespace QuantLib

#endif

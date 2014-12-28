/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008 StatPro Italia srl
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

/*! \file zerocurve.hpp
    \brief interpolated zero-rates structure
*/

#ifndef quantlib_zero_curve_hpp
#define quantlib_zero_curve_hpp

#include <ql/termstructures/yield/zeroyieldstructure.hpp>
#include <ql/termstructures/interpolatedcurve.hpp>
#include <ql/math/interpolations/linearinterpolation.hpp>
#include <ql/interestrate.hpp>
#include <ql/math/comparison.hpp>
#include <ql/utilities/dataformatters.hpp>
#include <utility>

namespace QuantLib {

//! YieldTermStructure based on interpolation of zero rates
/*! \ingroup yieldtermstructures */
template <class Interpolator, class T = Real>
class InterpolatedZeroCurve_t : public ZeroYieldStructure_t<T>,
                                protected InterpolatedCurve_t<Interpolator, T> {
  public:
    // constructor
    InterpolatedZeroCurve_t(
        const std::vector<Date> &dates, const std::vector<T> &yields,
        const DayCounter &dayCounter, const Calendar &calendar = Calendar(),
        const std::vector<Handle<Quote_t<T> > > &jumps =
            std::vector<Handle<Quote_t<T> > >(),
        const std::vector<Date> &jumpDates = std::vector<Date>(),
        const Interpolator &interpolator = Interpolator(),
        Compounding compounding = Continuous, Frequency frequency = Annual);
    InterpolatedZeroCurve_t(const std::vector<Date> &dates,
                            const std::vector<T> &yields,
                            const DayCounter &dayCounter,
                            const Calendar &calendar,
                            const Interpolator &interpolator,
                            Compounding compounding = Continuous,
                            Frequency frequency = Annual);
    InterpolatedZeroCurve_t(const std::vector<Date> &dates,
                            const std::vector<T> &yields,
                            const DayCounter &dayCounter,
                            const Interpolator &interpolator,
                            Compounding compounding = Continuous,
                            Frequency frequency = Annual);
    //! \name TermStructure interface
    //@{
    Date maxDate() const;
    //@}
    //! \name other inspectors
    //@{
    const std::vector<Time> &times() const;
    const std::vector<Date> &dates() const;
    const std::vector<T> &data() const;
    const std::vector<T> &zeroRates() const;
    std::vector<std::pair<Date, T> > nodes() const;
    //@}
  protected:
    InterpolatedZeroCurve_t(
        const DayCounter &, const std::vector<Handle<Quote_t<T> > > &jumps =
                                std::vector<Handle<Quote_t<T> > >(),
        const std::vector<Date> &jumpDates = std::vector<Date>(),
        const Interpolator &interpolator = Interpolator());
    InterpolatedZeroCurve_t(
        const Date &referenceDate, const DayCounter &,
        const std::vector<Handle<Quote_t<T> > > &jumps =
            std::vector<Handle<Quote_t<T> > >(),
        const std::vector<Date> &jumpDates = std::vector<Date>(),
        const Interpolator &interpolator = Interpolator());
    InterpolatedZeroCurve_t(
        Natural settlementDays, const Calendar &, const DayCounter &,
        const std::vector<Handle<Quote_t<T> > > &jumps =
            std::vector<Handle<Quote_t<T> > >(),
        const std::vector<Date> &jumpDates = std::vector<Date>(),
        const Interpolator &interpolator = Interpolator());
    //! \name ZeroYieldStructure implementation
    //@{
    T zeroYieldImpl(Time t) const;
    //@}
    mutable std::vector<Date> dates_;

  private:
    void initialize(const Compounding &compounding, const Frequency &frequency);
};

//! Term structure based on linear interpolation of zero yields
/*! \ingroup yieldtermstructures */
typedef InterpolatedZeroCurve_t<Linear, Real> ZeroCurve;

// inline definitions

template <class Interpolator, class T>
inline Date InterpolatedZeroCurve_t<Interpolator, T>::maxDate() const {
    return dates_.back();
}

template <class Interpolator, class T>
inline const std::vector<Time> &
InterpolatedZeroCurve_t<Interpolator, T>::times() const {
    return this->times_;
}

template <class Interpolator, class T>
inline const std::vector<Date> &
InterpolatedZeroCurve_t<Interpolator, T>::dates() const {
    return dates_;
}

template <class Interpolator, class T>
inline const std::vector<T> &
InterpolatedZeroCurve_t<Interpolator, T>::data() const {
    return this->data_;
}

template <class Interpolator, class T>
inline const std::vector<T> &
InterpolatedZeroCurve_t<Interpolator, T>::zeroRates() const {
    return this->data_;
}

template <class Interpolator, class T>
inline std::vector<std::pair<Date, T> >
InterpolatedZeroCurve_t<Interpolator, T>::nodes() const {
    std::vector<std::pair<Date, T> > results(dates_.size());
    for (Size i = 0; i < dates_.size(); ++i)
        results[i] = std::make_pair(dates_[i], this->data_[i]);
    return results;
}

#ifndef __DOXYGEN__

// template definitions

template <class Interpolator, class T>
T InterpolatedZeroCurve_t<Interpolator, T>::zeroYieldImpl(Time t) const {
    if (t <= this->times_.back())
        return this->interpolation_(t, true);

    // flat fwd extrapolation
    Time tMax = this->times_.back();
    T zMax = this->data_.back();
    T instFwdMax = zMax + tMax * this->interpolation_.derivative(tMax);
    return (zMax * tMax + instFwdMax * (t - tMax)) / t;
}

template <class Interpolator, class T>
InterpolatedZeroCurve_t<Interpolator, T>::InterpolatedZeroCurve_t(
    const DayCounter &dayCounter,
    const std::vector<Handle<Quote_t<T> > > &jumps,
    const std::vector<Date> &jumpDates, const Interpolator &interpolator)
    : ZeroYieldStructure_t<T>(dayCounter, jumps, jumpDates),
      InterpolatedCurve_t<Interpolator, T>(interpolator) {}

template <class Interpolator, class T>
InterpolatedZeroCurve_t<Interpolator, T>::InterpolatedZeroCurve_t(
    const Date &referenceDate, const DayCounter &dayCounter,
    const std::vector<Handle<Quote_t<T> > > &jumps,
    const std::vector<Date> &jumpDates, const Interpolator &interpolator)
    : ZeroYieldStructure_t<T>(referenceDate, Calendar(), dayCounter, jumps,
                              jumpDates),
      InterpolatedCurve_t<Interpolator, T>(interpolator) {}

template <class Interpolator, class T>
InterpolatedZeroCurve_t<Interpolator, T>::InterpolatedZeroCurve_t(
    Natural settlementDays, const Calendar &calendar,
    const DayCounter &dayCounter,
    const std::vector<Handle<Quote_t<T> > > &jumps,
    const std::vector<Date> &jumpDates, const Interpolator &interpolator)
    : ZeroYieldStructure_t<T>(settlementDays, calendar, dayCounter, jumps,
                              jumpDates),
      InterpolatedCurve_t<Interpolator, T>(interpolator) {}

template <class Interpolator, class T>
InterpolatedZeroCurve_t<Interpolator, T>::InterpolatedZeroCurve_t(
    const std::vector<Date> &dates, const std::vector<T> &yields,
    const DayCounter &dayCounter, const Calendar &calendar,
    const std::vector<Handle<Quote_t<T> > > &jumps,
    const std::vector<Date> &jumpDates, const Interpolator &interpolator,
    Compounding compounding, Frequency frequency)
    : ZeroYieldStructure_t<T>(dates.at(0), calendar, dayCounter, jumps,
                              jumpDates),
      InterpolatedCurve_t<Interpolator, T>(std::vector<Time>(), yields,
                                           interpolator),
      dates_(dates) {
    initialize(compounding, frequency);
}

template <class Interpolator, class T>
InterpolatedZeroCurve_t<Interpolator, T>::InterpolatedZeroCurve_t(
    const std::vector<Date> &dates, const std::vector<T> &yields,
    const DayCounter &dayCounter, const Calendar &calendar,
    const Interpolator &interpolator, Compounding compounding,
    Frequency frequency)
    : ZeroYieldStructure_t<T>(dates.at(0), calendar, dayCounter),
      InterpolatedCurve_t<Interpolator, T>(std::vector<Time>(), yields,
                                           interpolator),
      dates_(dates) {
    initialize(compounding, frequency);
}

template <class Interpolator, class T>
InterpolatedZeroCurve_t<Interpolator, T>::InterpolatedZeroCurve_t(
    const std::vector<Date> &dates, const std::vector<T> &yields,
    const DayCounter &dayCounter, const Interpolator &interpolator,
    Compounding compounding, Frequency frequency)
    : ZeroYieldStructure_t<T>(dates.at(0), Calendar(), dayCounter),
      InterpolatedCurve_t<Interpolator, T>(std::vector<Time>(), yields,
                                           interpolator),
      dates_(dates) {
    initialize(compounding, frequency);
}

#endif

template <class Interpolator, class T>
void InterpolatedZeroCurve_t<Interpolator, T>::initialize(
    const Compounding &compounding, const Frequency &frequency) {
    QL_REQUIRE(dates_.size() >= Interpolator::requiredPoints,
               "not enough input dates given");
    QL_REQUIRE(this->data_.size() == dates_.size(),
               "dates/data count mismatch");

    this->times_.resize(dates_.size());
    this->times_[0] = 0.0;
    if (compounding != Continuous) {
        // We also have to convert the first rate.
        // The first time is 0.0, so we can't use it.
        // We fall back to about one day.
        Time dt = 1.0 / 365;
        InterestRate_t<T> r(this->data_[0], this->dayCounter(), compounding,
                            frequency);
        this->data_[0] = r.equivalentRate(Continuous, NoFrequency, dt);
#if !defined(QL_NEGATIVE_RATES)
        QL_REQUIRE(this->data_[0] > 0.0, "non-positive yield");
#endif
    }

    for (Size i = 1; i < dates_.size(); ++i) {
        QL_REQUIRE(dates_[i] > dates_[i - 1], "invalid date ("
                                                  << dates_[i] << ", vs "
                                                  << dates_[i - 1] << ")");
        this->times_[i] = this->dayCounter().yearFraction(dates_[0], dates_[i]);
        QL_REQUIRE(!close(this->times_[i], this->times_[i - 1]),
                   "two dates correspond to the same time "
                   "under this curve's day count convention");

        // adjusting zero rates to match continuous compounding
        if (compounding != Continuous) {
            InterestRate_t<T> r(this->data_[i], this->dayCounter(), compounding,
                                frequency);
            this->data_[i] =
                r.equivalentRate(Continuous, NoFrequency, this->times_[i]);
        }

#if !defined(QL_NEGATIVE_RATES)
        QL_REQUIRE(this->data_[i] > 0.0, "non-positive yield");
        // positive yields are not enough to ensure non-negative fwd rates
        // so here's a stronger requirement
        QL_REQUIRE(this->data_[i] * this->times_[i] -
                           this->data_[i - 1] * this->times_[i - 1] >=
                       0.0,
                   "negative forward rate implied by the zero yield "
                       << io::rate(this->data_[i]) << " at " << dates_[i]
                       << " (t=" << this->times_[i] << ") after the zero yield "
                       << io::rate(this->data_[i - 1]) << " at "
                       << dates_[i - 1] << " (t=" << this->times_[i - 1]
                       << ")");
#endif
    }

    this->interpolation_ = this->interpolator_.interpolate(
        this->times_.begin(), this->times_.end(), this->data_.begin());
    this->interpolation_.update();
}

template <class Interpolator> struct InterpolatedZeroCurve {
    typedef InterpolatedZeroCurve_t<Interpolator, Real> Type;
};

} // namespace QuantLib

#endif

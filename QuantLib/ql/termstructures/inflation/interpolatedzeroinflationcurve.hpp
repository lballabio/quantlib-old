/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2007, 2008 Chris Kenyon
 Copyright (C) 2009 StatPro Italia srl
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

/*! \file interpolatedzeroinflationcurve.hpp
    \brief Inflation term structure based on the interpolation of zero rates.
*/

#ifndef quantlib_interpolated_zeroinflationcurve_hpp
#define quantlib_interpolated_zeroinflationcurve_hpp

#include <ql/termstructures/inflationtermstructure.hpp>
#include <ql/termstructures/interpolatedcurve.hpp>
#include <ql/math/interpolations/linearinterpolation.hpp>
#include <ql/math/comparison.hpp>
#include <utility>

namespace QuantLib {

//! Inflation term structure based on the interpolation of zero rates.
/*! \ingroup inflationtermstructures */
template <template <class> class Interpolator, class T>
class InterpolatedZeroInflationCurve_t
    : public ZeroInflationTermStructure_t<T>,
      protected InterpolatedCurve_t<Interpolator, T> {
  public:
    InterpolatedZeroInflationCurve_t(
        const Date &referenceDate, const Calendar &calendar,
        const DayCounter &dayCounter, const Period &lag, Frequency frequency,
        bool indexIsInterpolated, const Handle<YieldTermStructure_t<T> > &yTS,
        const std::vector<Date> &dates, const std::vector<Rate> &rates,
        const Interpolator<T> &interpolator = Interpolator<T>());

    //! \name InflationTermStructure interface
    //@{
    Date baseDate() const;
    Date maxDate() const;
    //@}

    //! \name Inspectors
    //@{
    const std::vector<Date> &dates() const;
    const std::vector<Time> &times() const;
    const std::vector<T> &data() const;
    const std::vector<T> &rates() const;
    std::vector<std::pair<Date, T> > nodes() const;
    //@}

  protected:
    //! \name ZeroInflationTermStructure Interface
    //@{
    T zeroRateImpl(Time t) const;
    //@}
    mutable std::vector<Date> dates_;

    /*! Protected version for use when descendents don't want to
        (or can't) provide the points for interpolation on
        construction.
    */
    InterpolatedZeroInflationCurve_t(
        const Date &referenceDate, const Calendar &calendar,
        const DayCounter &dayCounter, const Period &lag, Frequency frequency,
        bool indexIsInterpolated, T baseZeroRate,
        const Handle<YieldTermStructure_t<T> > &yTS,
        const Interpolator<T> &interpolator = Interpolator<T>());
};

typedef InterpolatedZeroInflationCurve_t<Linear, Real> ZeroInflationCurve;

template <template <class> class I> struct InterpolatedZeroInflationCurve {
    typedef InterpolatedZeroInflationCurve_t<I, Real> Type;
};

// template definitions

template <template <class> class Interpolator, class T>
InterpolatedZeroInflationCurve_t<Interpolator, T>::
    InterpolatedZeroInflationCurve_t(
        const Date &referenceDate, const Calendar &calendar,
        const DayCounter &dayCounter, const Period &lag, Frequency frequency,
        bool indexIsInterpolated, const Handle<YieldTermStructure_t<T> > &yTS,
        const std::vector<Date> &dates, const std::vector<Rate> &rates,
        const Interpolator<T> &interpolator)
    : ZeroInflationTermStructure_t<T>(referenceDate, calendar, dayCounter,
                                      rates[0], lag, frequency,
                                      indexIsInterpolated, yTS),
      InterpolatedCurve_t<Interpolator, T>(std::vector<Time>(), rates,
                                           interpolator),
      dates_(dates) {

    QL_REQUIRE(dates_.size() > 1, "too few dates: " << dates_.size());

    // check that the data starts from the beginning,
    // i.e. referenceDate - lag, at least must be in the relevant
    // period
    std::pair<Date, Date> lim = inflationPeriod(
        yTS->referenceDate() - this->observationLag(), frequency);
    QL_REQUIRE(lim.first <= dates_[0] && dates_[0] <= lim.second,
               "first data date is not in base period, date: "
                   << dates_[0] << " not within [" << lim.first << ","
                   << lim.second << "]");

    // by convention, if the index is not interpolated we pull all the dates
    // back to the start of their inflationPeriods
    // otherwise the time calculations will be inconsistent
    if (!this->indexIsInterpolated_) {
        for (Size i = 0; i < dates_.size(); i++) {
            dates_[i] = inflationPeriod(dates_[i], frequency).first;
        }
    }

    QL_REQUIRE(this->data_.size() == dates_.size(),
               "indices/dates count mismatch: " << this->data_.size() << " vs "
                                                << dates_.size());

    this->times_.resize(dates_.size());
    this->times_[0] = this->timeFromReference(dates_[0]);
    for (Size i = 1; i < dates_.size(); i++) {
        QL_REQUIRE(dates_[i] > dates_[i - 1], "dates not sorted");

        // but must be greater than -1
        QL_REQUIRE(this->data_[i] > -1.0, "zero inflation data < -100 %");

        // this can be negative
        this->times_[i] = this->timeFromReference(dates_[i]);
        QL_REQUIRE(!close(this->times_[i], this->times_[i - 1]),
                   "two dates correspond to the same time "
                   "under this curve's day count convention");
    }

    this->interpolation_ = this->interpolator_.interpolate(
        this->times_.begin(), this->times_.end(), this->data_.begin());
    this->interpolation_.update();
}

template <template <class> class Interpolator, class T>
InterpolatedZeroInflationCurve_t<Interpolator, T>::
    InterpolatedZeroInflationCurve_t(
        const Date &referenceDate, const Calendar &calendar,
        const DayCounter &dayCounter, const Period &lag, Frequency frequency,
        bool indexIsInterpolated, T baseZeroRate,
        const Handle<YieldTermStructure_t<T> > &yTS,
        const Interpolator<T> &interpolator)
    : ZeroInflationTermStructure_t<T>(referenceDate, calendar, dayCounter,
                                      baseZeroRate, lag, frequency,
                                      indexIsInterpolated, yTS),
      InterpolatedCurve_t<Interpolator, T>(interpolator) {}

template <template <class> class I, class T>
Date InterpolatedZeroInflationCurve_t<I, T>::baseDate() const {
    // if indexIsInterpolated we fixed the dates in the constructor
    return dates_.front();
}

template <template <class> class I, class T>
Date InterpolatedZeroInflationCurve_t<I, T>::maxDate() const {
    Date d;
    if (this->indexIsInterpolated()) {
        d = dates_.back();
    } else {
        d = inflationPeriod(dates_.back(), this->frequency()).second;
    }
    return d;
}

template <template <class> class I, class T>
inline T InterpolatedZeroInflationCurve_t<I, T>::zeroRateImpl(Time t) const {
    return this->interpolation_(t, true);
}

template <template <class> class I, class T>
inline const std::vector<Time> &
InterpolatedZeroInflationCurve_t<I, T>::times() const {
    return this->times_;
}

template <template <class> class I, class T>
inline const std::vector<Date> &
InterpolatedZeroInflationCurve_t<I, T>::dates() const {
    return dates_;
}

template <template <class> class I, class T>
inline const std::vector<T> &
InterpolatedZeroInflationCurve_t<I, T>::rates() const {
    return this->data_;
}

template <template <class> class I, class T>

inline const std::vector<T> &
InterpolatedZeroInflationCurve_t<I, T>::data() const {
    return this->data_;
}

template <template <class> class I, class T>
inline std::vector<std::pair<Date, T> >
InterpolatedZeroInflationCurve_t<I, T>::nodes() const {
    std::vector<std::pair<Date, T> > results(dates_.size());
    for (Size i = 0; i < dates_.size(); ++i)
        results[i] = std::make_pair(dates_[i], this->data_[i]);
    return results;
}
}

#endif

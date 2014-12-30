/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2007, 2009 Chris Kenyon
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

/*! \file inflationtermstructure.hpp
    \brief Base classes for inflation term structures.
*/

#ifndef quantlib_inflation_termstructure_hpp
#define quantlib_inflation_termstructure_hpp

#include <ql/termstructures/yieldtermstructure.hpp>
#include <ql/termstructures/inflation/seasonality.hpp>

namespace QuantLib {

class InflationIndex;

template <class T> class Seasonality_t;

//! Interface for inflation term structures.
/*! \ingroup inflationtermstructures */
template <class T = Real>
class InflationTermStructure_t : public TermStructure {
  public:
    //! \name Constructors
    //@{
    InflationTermStructure_t(
        T baseRate, const Period &observationLag, Frequency frequency,
        bool indexIsInterpolated, const Handle<YieldTermStructure_t<T> > &yTS,
        const DayCounter &dayCounter = DayCounter(),
        const boost::shared_ptr<Seasonality_t<T> > &seasonality =
            boost::shared_ptr<Seasonality_t<T> >());
    InflationTermStructure_t(
        const Date &referenceDate, T baseRate, const Period &observationLag,
        Frequency frequency, bool indexIsInterpolated,
        const Handle<YieldTermStructure_t<T> > &yTS,
        const Calendar &calendar = Calendar(),
        const DayCounter &dayCounter = DayCounter(),
        const boost::shared_ptr<Seasonality_t<T> > &seasonality =
            boost::shared_ptr<Seasonality_t<T> >());
    InflationTermStructure_t(
        Natural settlementDays, const Calendar &calendar, T baseRate,
        const Period &observationLag, Frequency frequency,
        bool indexIsInterpolated, const Handle<YieldTermStructure_t<T> > &yTS,
        const DayCounter &dayCounter = DayCounter(),
        const boost::shared_ptr<Seasonality_t<T> > &seasonality =
            boost::shared_ptr<Seasonality_t<T> >());
    //@}

    //! \name Inflation interface
    //@{
    //! The TS observes with a lag that is usually different from the
    //! availability lag of the index.  An inflation rate is given,
    //! by default, for the maturity requested assuming this lag.
    virtual Period observationLag() const;
    virtual Frequency frequency() const;
    virtual bool indexIsInterpolated() const;
    virtual T baseRate() const;
    virtual Handle<YieldTermStructure_t<T> > nominalTermStructure() const;

    //! minimum (base) date
    /*! Important in inflation since it starts before nominal
        reference date.  Changes depending whether index is
        interpolated or not.  When interpolated the base date
        is just observation lag before nominal.  When not
        interpolated it is the beginning of the relevant period
        (hence it is easy to create interpolated fixings from
         a not-interpolated curve because interpolation, usually,
         of fixings is forward looking).
    */
    virtual Date baseDate() const = 0;
    //@}

    //! Functions to set and get seasonality.
    /*! Calling setSeasonality with no arguments means unsetting
        as the default is used to choose unsetting.
    */
    void
    setSeasonality(const boost::shared_ptr<Seasonality_t<T> > &seasonality =
                       boost::shared_ptr<Seasonality_t<T> >());
    boost::shared_ptr<Seasonality_t<T> > seasonality() const;
    bool hasSeasonality() const;

  protected:
    // This next part is required for piecewise- constructors
    // because, for inflation, they need more than just the
    // instruments to build the term structure, since the rate at
    // time 0-lag is non-zero, since we deal (effectively) with
    // "forwards".
    virtual void setBaseRate(const T &r) { baseRate_ = r; }

    // range-checking
    void checkRange(const Date &, bool extrapolate) const;
    void checkRange(Time t, bool extrapolate) const;

    boost::shared_ptr<Seasonality_t<T> > seasonality_;
    Period observationLag_;
    Frequency frequency_;
    bool indexIsInterpolated_;
    mutable T baseRate_;
    Handle<YieldTermStructure_t<T> > nominalTermStructure_;
};

typedef InflationTermStructure_t<Real> InflationTermStructure;

//! Interface for zero inflation term structures.
// Child classes use templates but do not want that exposed to
// general users.
template <class T = Real>
class ZeroInflationTermStructure_t : public InflationTermStructure_t<T> {
  public:
    //! \name Constructors
    //@{
    ZeroInflationTermStructure_t(
        const DayCounter &dayCounter, T baseZeroRate, const Period &lag,
        Frequency frequency, bool indexIsInterpolated,
        const Handle<YieldTermStructure_t<T> > &yTS,
        const boost::shared_ptr<Seasonality_t<T> > &seasonality =
            boost::shared_ptr<Seasonality_t<T> >());

    ZeroInflationTermStructure_t(
        const Date &referenceDate, const Calendar &calendar,
        const DayCounter &dayCounter, T baseZeroRate, const Period &lag,
        Frequency frequency, const bool indexIsInterpolated,
        const Handle<YieldTermStructure_t<T> > &yTS,
        const boost::shared_ptr<Seasonality_t<T> > &seasonality =
            boost::shared_ptr<Seasonality_t<T> >());

    ZeroInflationTermStructure_t(
        Natural settlementDays, const Calendar &calendar,
        const DayCounter &dayCounter, T baseZeroRate, const Period &lag,
        Frequency frequency, bool indexIsInterpolated,
        const Handle<YieldTermStructure_t<T> > &yTS,
        const boost::shared_ptr<Seasonality_t<T> > &seasonality =
            boost::shared_ptr<Seasonality_t<T> >());
    //@}

    //! \name Inspectors
    //@{
    //! zero-coupon inflation rate.
    /*! Essentially the fair rate for a zero-coupon inflation swap
        (by definition), i.e. the zero term structure uses yearly
        compounding, which is assumed for ZCIIS instrument quotes.

        \note by default you get the same as lag and interpolation
        as the term structure.
        If you want to get predictions of RPI/CPI/etc then use an
        index.
    */
    T zeroRate(const Date &d, const Period &instObsLag = Period(-1, Days),
               bool forceLinearInterpolation = false,
               bool extrapolate = false) const;
    //! zero-coupon inflation rate.
    /*! \warning Since inflation is highly linked to dates (lags,
                 interpolation, months for seasonality, etc) this
                 method cannot account for all effects.  If you
                 call it, You'll have to manage lag, seasonality
                 etc. yourself.
    */
    T zeroRate(Time t, bool extrapolate = false) const;
    //@}
  protected:
    //! to be defined in derived classes
    virtual T zeroRateImpl(Time t) const = 0;
};

typedef ZeroInflationTermStructure_t<Real> ZeroInflationTermStructure;

//! Base class for year-on-year inflation term structures.

template <class T>
class YoYInflationTermStructure_t : public InflationTermStructure_t<T> {
  public:
    //! \name Constructors
    //@{
    YoYInflationTermStructure_t(
        const DayCounter &dayCounter, T baseYoYRate, const Period &lag,
        Frequency frequency, bool indexIsInterpolated,
        const Handle<YieldTermStructure_t<T> > &yieldTS,
        const boost::shared_ptr<Seasonality_t<T> > &seasonality =
            boost::shared_ptr<Seasonality_t<T> >());

    YoYInflationTermStructure_t(
        const Date &referenceDate, const Calendar &calendar,
        const DayCounter &dayCounter, T baseYoYRate, const Period &lag,
        Frequency frequency, bool indexIsInterpolated,
        const Handle<YieldTermStructure_t<T> > &yieldTS,
        const boost::shared_ptr<Seasonality_t<T> > &seasonality =
            boost::shared_ptr<Seasonality_t<T> >());

    YoYInflationTermStructure_t(
        Natural settlementDays, const Calendar &calendar,
        const DayCounter &dayCounter, T baseYoYRate, const Period &lag,
        Frequency frequency, bool indexIsInterpolated,
        const Handle<YieldTermStructure_t<T> > &yieldTS,
        const boost::shared_ptr<Seasonality_t<T> > &seasonality =
            boost::shared_ptr<Seasonality_t<T> >());
    //@}

    //! \name Inspectors
    //@{
    //! year-on-year inflation rate.
    /*! The forceLinearInterpolation parameter is relative to the
        frequency of the TS.

        \note this is not the year-on-year swap (YYIIS) rate.
    */
    T yoyRate(const Date &d, const Period &instObsLag = Period(-1, Days),
              bool forceLinearInterpolation = false,
              bool extrapolate = false) const;
    //! year-on-year inflation rate.
    /*! \warning Since inflation is highly linked to dates (lags,
                 interpolation, months for seasonality, etc) this
                 method cannot account for all effects.  If you
                 call it, You'll have to manage lag, seasonality
                 etc. yourself.
    */
    T yoyRate(Time t, bool extrapolate = false) const;
    //@}
  protected:
    //! to be defined in derived classes
    virtual T yoyRateImpl(Time time) const = 0;
};

typedef YoYInflationTermStructure_t<Real> YoYInflationTermStructure;

//! utility function giving the inflation period for a given date
std::pair<Date, Date> inflationPeriod(const Date &, Frequency);

//! utility function giving the time between two dates depending on
//! index frequency and interpolation, and a day counter
Time inflationYearFraction(Frequency, bool indexIsInterpolated,
                           const DayCounter &, const Date &, const Date &);

// inline

template <class T>
inline Period InflationTermStructure_t<T>::observationLag() const {
    return observationLag_;
}

template <class T>
inline Frequency InflationTermStructure_t<T>::frequency() const {
    return frequency_;
}

template <class T>
inline bool InflationTermStructure_t<T>::indexIsInterpolated() const {
    return indexIsInterpolated_;
}

template <class T> inline T InflationTermStructure_t<T>::baseRate() const {
    return baseRate_;
}

template <class T>
inline Handle<YieldTermStructure_t<T> >
InflationTermStructure_t<T>::nominalTermStructure() const {
    return nominalTermStructure_;
}

template <class T>
inline boost::shared_ptr<Seasonality_t<T> >
InflationTermStructure_t<T>::seasonality() const {
    return seasonality_;
}

template <class T>
inline bool InflationTermStructure_t<T>::hasSeasonality() const {
    return static_cast<bool>(seasonality_);
}

// implementation

template <class T>
InflationTermStructure_t<T>::InflationTermStructure_t(
    T baseRate, const Period &observationLag, Frequency frequency,
    bool indexIsInterpolated, const Handle<YieldTermStructure_t<T> > &yTS,
    const DayCounter &dayCounter,
    const boost::shared_ptr<Seasonality_t<T> > &seasonality)
    : TermStructure(dayCounter), observationLag_(observationLag),
      frequency_(frequency), indexIsInterpolated_(indexIsInterpolated),
      baseRate_(baseRate), nominalTermStructure_(yTS) {
    registerWith(nominalTermStructure_);
    setSeasonality(seasonality);
}

template <class T>
InflationTermStructure_t<T>::InflationTermStructure_t(
    const Date &referenceDate, T baseRate, const Period &observationLag,
    Frequency frequency, const bool indexIsInterpolated,
    const Handle<YieldTermStructure_t<T> > &yTS, const Calendar &calendar,
    const DayCounter &dayCounter,
    const boost::shared_ptr<Seasonality_t<T> > &seasonality)
    : TermStructure(referenceDate, calendar, dayCounter),
      observationLag_(observationLag), frequency_(frequency),
      indexIsInterpolated_(indexIsInterpolated), baseRate_(baseRate),
      nominalTermStructure_(yTS) {
    registerWith(nominalTermStructure_);
    setSeasonality(seasonality);
}

template <class T>
InflationTermStructure_t<T>::InflationTermStructure_t(
    Natural settlementDays, const Calendar &calendar, T baseRate,
    const Period &observationLag, Frequency frequency, bool indexIsInterpolated,
    const Handle<YieldTermStructure_t<T> > &yTS, const DayCounter &dayCounter,
    const boost::shared_ptr<Seasonality_t<T> > &seasonality)
    : TermStructure(settlementDays, calendar, dayCounter),
      observationLag_(observationLag), frequency_(frequency),
      indexIsInterpolated_(indexIsInterpolated), baseRate_(baseRate),
      nominalTermStructure_(yTS) {
    registerWith(nominalTermStructure_);
    setSeasonality(seasonality);
}

template <class T>
void InflationTermStructure_t<T>::setSeasonality(
    const boost::shared_ptr<Seasonality_t<T> > &seasonality) {
    // always reset, whether with null or new pointer
    seasonality_ = seasonality;
    if (seasonality_) {
        QL_REQUIRE(seasonality_->isConsistent(*this),
                   "Seasonality inconsistent with "
                   "inflation term structure");
    }
    notifyObservers();
}

template <class T>
void InflationTermStructure_t<T>::checkRange(const Date &d,
                                             bool extrapolate) const {
    QL_REQUIRE(d >= baseDate(), "date (" << d << ") is before base date");
    QL_REQUIRE(extrapolate || allowsExtrapolation() || d <= maxDate(),
               "date (" << d << ") is past max curve date (" << maxDate()
                        << ")");
}

template <class T>
void InflationTermStructure_t<T>::checkRange(Time t, bool extrapolate) const {
    QL_REQUIRE(t >= timeFromReference(baseDate()),
               "time (" << t << ") is before base date");
    QL_REQUIRE(extrapolate || allowsExtrapolation() || t <= maxTime(),
               "time (" << t << ") is past max curve time (" << maxTime()
                        << ")");
}

template <class T>
ZeroInflationTermStructure_t<T>::ZeroInflationTermStructure_t(
    const DayCounter &dayCounter, T baseZeroRate, const Period &observationLag,
    Frequency frequency, bool indexIsInterpolated,
    const Handle<YieldTermStructure_t<T> > &yTS,
    const boost::shared_ptr<Seasonality_t<T> > &seasonality)
    : InflationTermStructure_t<T>(baseZeroRate, observationLag, frequency,
                                  indexIsInterpolated, yTS, dayCounter,
                                  seasonality) {}

template <class T>
ZeroInflationTermStructure_t<T>::ZeroInflationTermStructure_t(
    const Date &referenceDate, const Calendar &calendar,
    const DayCounter &dayCounter, T baseZeroRate, const Period &observationLag,
    Frequency frequency, bool indexIsInterpolated,
    const Handle<YieldTermStructure_t<T> > &yTS,
    const boost::shared_ptr<Seasonality_t<T> > &seasonality)
    : InflationTermStructure_t<T>(referenceDate, baseZeroRate, observationLag,
                                  frequency, indexIsInterpolated, yTS, calendar,
                                  dayCounter, seasonality) {}

template <class T>
ZeroInflationTermStructure_t<T>::ZeroInflationTermStructure_t(
    Natural settlementDays, const Calendar &calendar,
    const DayCounter &dayCounter, T baseZeroRate, const Period &observationLag,
    Frequency frequency, bool indexIsInterpolated,
    const Handle<YieldTermStructure_t<T> > &yTS,
    const boost::shared_ptr<Seasonality_t<T> > &seasonality)
    : InflationTermStructure_t<T>(
          settlementDays, calendar, baseZeroRate, observationLag, frequency,
          indexIsInterpolated, yTS, dayCounter, seasonality) {}

template <class T>
T ZeroInflationTermStructure_t<T>::zeroRate(const Date &d,
                                            const Period &instObsLag,
                                            bool forceLinearInterpolation,
                                            bool extrapolate) const {

    Period useLag = instObsLag;
    if (instObsLag == Period(-1, Days)) {
        useLag = this->observationLag();
    }

    T zeroRate;
    if (forceLinearInterpolation) {
        std::pair<Date, Date> dd =
            inflationPeriod(d - useLag, this->frequency());
        dd.second = dd.second + Period(1, Days);
        Real dp = dd.second - dd.first;
        Real dt = d - dd.first;
        // if we are interpolating we only check the exact point
        // this prevents falling off the end at curve maturity
        InflationTermStructure_t<T>::checkRange(d, extrapolate);
        Time t1 = this->timeFromReference(dd.first);
        Time t2 = this->timeFromReference(dd.second);
        T z1 = this->zeroRateImpl(t1);
        T z2 = this->zeroRateImpl(t2);
        zeroRate = z1 + (z2 - z1) * (dt / dp);
    } else {
        if (this->indexIsInterpolated()) {
            InflationTermStructure_t<T>::checkRange(d - useLag, extrapolate);
            Time t = this->timeFromReference(d - useLag);
            zeroRate = zeroRateImpl(t);
        } else {
            std::pair<Date, Date> dd =
                inflationPeriod(d - useLag, this->frequency());
            InflationTermStructure_t<T>::checkRange(dd.first, extrapolate);
            Time t = this->timeFromReference(dd.first);
            zeroRate = this->zeroRateImpl(t);
        }
    }

    if (this->hasSeasonality()) {
        zeroRate =
            this->seasonality()->correctZeroRate(d - useLag, zeroRate, *this);
    }
    return zeroRate;
}

template <class T>
T ZeroInflationTermStructure_t<T>::zeroRate(Time t, bool extrapolate) const {
    this->checkRange(t, extrapolate);
    return this->zeroRateImpl(t);
}

template <class T>
YoYInflationTermStructure_t<T>::YoYInflationTermStructure_t(
    const DayCounter &dayCounter, T baseYoYRate, const Period &observationLag,
    Frequency frequency, bool indexIsInterpolated,
    const Handle<YieldTermStructure_t<T> > &yTS,
    const boost::shared_ptr<Seasonality_t<T> > &seasonality)
    : InflationTermStructure_t<T>(baseYoYRate, observationLag, frequency,
                                  indexIsInterpolated, yTS, dayCounter,
                                  seasonality) {}

template <class T>
YoYInflationTermStructure_t<T>::YoYInflationTermStructure_t(
    const Date &referenceDate, const Calendar &calendar,
    const DayCounter &dayCounter, T baseYoYRate, const Period &observationLag,
    Frequency frequency, bool indexIsInterpolated,
    const Handle<YieldTermStructure_t<T> > &yTS,
    const boost::shared_ptr<Seasonality_t<T> > &seasonality)
    : InflationTermStructure_t<T>(referenceDate, baseYoYRate, observationLag,
                                  frequency, indexIsInterpolated, yTS, calendar,
                                  dayCounter, seasonality) {}

template <class T>
YoYInflationTermStructure_t<T>::YoYInflationTermStructure_t(
    Natural settlementDays, const Calendar &calendar,
    const DayCounter &dayCounter, T baseYoYRate, const Period &observationLag,
    Frequency frequency, bool indexIsInterpolated,
    const Handle<YieldTermStructure_t<T> > &yTS,
    const boost::shared_ptr<Seasonality_t<T> > &seasonality)
    : InflationTermStructure_t<T>(
          settlementDays, calendar, baseYoYRate, observationLag, frequency,
          indexIsInterpolated, yTS, dayCounter, seasonality) {}

template <class T>
T YoYInflationTermStructure_t<T>::yoyRate(const Date &d,
                                          const Period &instObsLag,
                                          bool forceLinearInterpolation,
                                          bool extrapolate) const {

    Period useLag = instObsLag;
    if (instObsLag == Period(-1, Days)) {
        useLag = this->observationLag();
    }

    T yoyRate;
    if (forceLinearInterpolation) {
        std::pair<Date, Date> dd =
            inflationPeriod(d - useLag, this->frequency());
        dd.second = dd.second + Period(1, Days);
        Real dp = dd.second - dd.first;
        Real dt = (d - useLag) - dd.first;
        // if we are interpolating we only check the exact point
        // this prevents falling off the end at curve maturity
        InflationTermStructure::checkRange(d, extrapolate);
        Time t1 = this->timeFromReference(dd.first);
        Time t2 = this->timeFromReference(dd.second);
        T y1 = this->yoyRateImpl(t1);
        T y2 = this->yoyRateImpl(t2);
        yoyRate = y1 + (y2 - y1) * (dt / dp);
    } else {
        if (this->indexIsInterpolated()) {
            InflationTermStructure_t<T>::checkRange(d - useLag, extrapolate);
            Time t = this->timeFromReference(d - useLag);
            yoyRate = this->yoyRateImpl(t);
        } else {
            std::pair<Date, Date> dd =
                inflationPeriod(d - useLag, this->frequency());
            InflationTermStructure_t<T>::checkRange(dd.first, extrapolate);
            Time t = this->timeFromReference(dd.first);
            yoyRate = this->yoyRateImpl(t);
        }
    }

    if (this->hasSeasonality()) {
        yoyRate =
            this->seasonality()->correctYoYRate(d - useLag, yoyRate, *this);
    }
    return yoyRate;
}

template <class T>
T YoYInflationTermStructure_t<T>::yoyRate(Time t, bool extrapolate) const {
    this->checkRange(t, extrapolate);
    return yoyRateImpl(t);
}

std::pair<Date, Date> inflationPeriod(const Date &d, Frequency frequency);

Time inflationYearFraction(Frequency f, bool indexIsInterpolated,
                           const DayCounter &dayCounter, const Date &d1,
                           const Date &d2);

} // namespace QuantLib

#endif

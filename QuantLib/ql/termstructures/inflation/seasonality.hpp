/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2008 Piero Del Boca
 Copyright (C) 2009 Chris Kenyon
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

#ifndef quantlib_seasonality_hpp
#define quantlib_seasonality_hpp

#include <ql/time/daycounter.hpp>
#include <ql/time/frequency.hpp>
#include <ql/termstructures/inflationtermstructure.hpp>
#include <boost/shared_ptr.hpp>
#include <vector>

namespace QuantLib {

//! A transformation of an existing inflation swap rate.
/*! This is an abstract class and contains the functions
    correctXXXRate which returns rates with the seasonality
    correction.  Currently only the price multiplicative version
    is implemented, but this covers stationary (1-year) and
    non-stationary (multi-year) seasonality depending on how many
    years of factors are given.  Seasonality is piecewise
    constant, hence it will work with un-interpolated inflation
    indices.

    A seasonality assumption can be used to fill in inflation swap
    curves between maturities that are usually given in integer
    numbers of years, e.g. 8,9,10,15,20, etc.  Historical
    seasonality may be observed in reported CPI values,
    alternatively it may be affected by known future events, e.g.
    announced changes in VAT rates.  Thus seasonality may be
    stationary or non-stationary.

    If seasonality is additive then both swap rates will show
    affects.  Additive seasonality is not implemented.
*/

using std::abs;

template <class T> class InflationTermStructure_t;
std::pair<Date, Date> inflationPeriod(const Date &d, Frequency frequency);

template <class T> class Seasonality_t {

  public:
    //! \name Seasonality interface
    //@{
    virtual T correctZeroRate(const Date &d, const Rate r,
                              const InflationTermStructure_t<T> &iTS) const = 0;
    virtual T correctYoYRate(const Date &d, const Rate r,
                             const InflationTermStructure_t<T> &iTS) const = 0;
    /*! It is possible for multi-year seasonalities to be
        inconsistent with the inflation term structure they are
        given to.  This method enables testing - but programmers
        are not required to implement it.  E.g. for price
        seasonality the corrections at whole years after the
        inflation curve base date should be the same or else there
        can be an inconsistency with quoted instruments.
        Alternatively, the seasonality can be set _before_ the
        inflation curve is bootstrapped.
    */
    virtual bool isConsistent(const InflationTermStructure_t<T> &iTS) const;
    //@}

    virtual ~Seasonality_t() {}
};

//! Multiplicative seasonality in the price index (CPI/RPI/HICP/etc).

/*! Stationary multiplicative seasonality in CPI/RPI/HICP (i.e. in
    price) implies that zero inflation swap rates are affected,
    but that year-on-year inflation swap rates show no effect.  Of
    course, if the seasonality in CPI/RPI/HICP is non-stationary
    then both swap rates will be affected.

    Factors must be in multiples of the minimum required for one
    year, e.g. 12 for monthly, and these factors are reused for as
    long as is required, i.e. they wrap around.  So, for example,
    if 24 factors are given this repeats every two years.  True
    stationary seasonality can be obtained by giving the same
    number of factors as the frequency dictates e.g. 12 for
    monthly seasonality.

    \warning Multi-year seasonality (i.e. non-stationary) is
             fragile: the user <b>must</b> ensure that corrections
             at whole years before and after the inflation term
             structure base date are the same.  Otherwise there
             can be an inconsistency with quoted rates.  This is
             enforced if the frequency is lower than daily.  This
             is not enforced for daily seasonality because this
             will always be inconsistent due to weekends,
             holidays, leap years, etc.  If you use multi-year
             daily seasonality it is up to you to check.

    \note Factors are normalized relative to their appropriate
          reference dates.  For zero inflation this is the
          inflation curve true base date: since you have a fixing
          for that date the seasonality factor must be one.  For
          YoY inflation the reference is always one year earlier.

    Seasonality is treated as piecewise constant, hence it works
    correctly with uninterpolated indices if the seasonality
    correction factor frequency is the same as the index frequency
    (or less).
*/

typedef Seasonality_t<Real> Seasonality;

template <class T>
class MultiplicativePriceSeasonality_t : public Seasonality_t<T> {

  private:
    Date seasonalityBaseDate_;
    Frequency frequency_;
    std::vector<T> seasonalityFactors_;

  public:
    // Constructors
    //
    MultiplicativePriceSeasonality_t();

    MultiplicativePriceSeasonality_t(const Date &seasonalityBaseDate,
                                     const Frequency frequency,
                                     const std::vector<T> seasonalityFactors);

    virtual void set(const Date &seasonalityBaseDate, const Frequency frequency,
                     const std::vector<T> seasonalityFactors);

    //! inspectors
    //@{
    virtual Date seasonalityBaseDate() const;
    virtual Frequency frequency() const;
    virtual std::vector<T> seasonalityFactors() const;
    //! The factor returned is NOT normalized relative to ANYTHING.
    virtual T seasonalityFactor(const Date &d) const;
    //@}

    //! \name Seasonality interface
    //@{
    virtual T correctZeroRate(const Date &d, const T r,
                              const InflationTermStructure_t<T> &iTS) const;
    virtual T correctYoYRate(const Date &d, const T r,
                             const InflationTermStructure_t<T> &iTS) const;
    virtual bool isConsistent(const InflationTermStructure_t<T> &iTS) const;
    //@}

    // Destructor
    virtual ~MultiplicativePriceSeasonality_t(){};

  protected:
    virtual void validate() const;
    virtual T seasonalityCorrection(T r, const Date &d, const DayCounter &dc,
                                    const Date &curveBaseDate,
                                    bool isZeroRate) const;
};

typedef MultiplicativePriceSeasonality_t<Real> MultiplicativePriceSeasonality;

// implementation

template <class T>
bool Seasonality_t<T>::isConsistent(const InflationTermStructure_t<T> &) const {
    return true;
}

// Multiplicative Seasonality on price = on CPI/RPI/HICP/etc
//
template <class T>
MultiplicativePriceSeasonality_t<T>::MultiplicativePriceSeasonality_t() {}

template <class T> void MultiplicativePriceSeasonality_t<T>::validate() const {
    switch (this->frequency()) {
    case Semiannual:       // 2
    case EveryFourthMonth: // 3
    case Quarterly:        // 4
    case Bimonthly:        // 6
    case Monthly:          // 12
    case Biweekly:         // etc.
    case Weekly:
    case Daily:
        QL_REQUIRE((this->seasonalityFactors().size() % this->frequency()) == 0,
                   "For frequency "
                       << this->frequency() << " require multiple of "
                       << ((int)this->frequency()) << " factors "
                       << this->seasonalityFactors().size() << " were given.");
        break;
    default:
        QL_FAIL("bad frequency specified: "
                << this->frequency()
                << ", only semi-annual through daily permitted.");
        break;
    }
}

template <class T>
bool MultiplicativePriceSeasonality_t<T>::isConsistent(
    const InflationTermStructure_t<T> &iTS) const {
    // If multi-year is the specification consistent with the term structure
    // start date?
    // We do NOT test daily seasonality because this will, in general, never be
    // consistent
    // given weekends, holidays, leap years, etc.
    if (this->frequency() == Daily)
        return true;
    if (Size(this->frequency()) == seasonalityFactors().size())
        return true;

    // how many years do you need to test?
    Size nTest = seasonalityFactors().size() / this->frequency();
    // ... relative to the start of the inflation curve
    std::pair<Date, Date> lim =
        inflationPeriod(iTS.baseDate(), iTS.frequency());
    Date curveBaseDate = lim.second;
    T factorBase = this->seasonalityFactor(curveBaseDate);

    Real eps = 0.00001;
    for (Size i = 1; i < nTest; i++) {
        Real factorAt =
            this->seasonalityFactor(curveBaseDate + Period(i, Years));
        QL_REQUIRE(std::fabs(factorAt - factorBase) < eps,
                   "seasonality is inconsistent with inflation term structure, "
                   "factors "
                       << factorBase << " and later factor " << factorAt << ", "
                       << i << " years later from inflation curve "
                       << " with base date at " << curveBaseDate);
    }

    return true;
}

template <class T>
MultiplicativePriceSeasonality_t<T>::MultiplicativePriceSeasonality_t(
    const Date &seasonalityBaseDate, const Frequency frequency,
    const std::vector<T> seasonalityFactors) {
    set(seasonalityBaseDate, frequency, seasonalityFactors);
}

template <class T>
void MultiplicativePriceSeasonality_t<T>::set(
    const Date &seasonalityBaseDate, const Frequency frequency,
    const std::vector<T> seasonalityFactors) {
    frequency_ = frequency;
    seasonalityFactors_ = std::vector<T>(seasonalityFactors.size());
    for (Size i = 0; i < seasonalityFactors.size(); i++) {
        seasonalityFactors_[i] = seasonalityFactors[i];
    }
    seasonalityBaseDate_ = seasonalityBaseDate;
    validate();
}

template <class T>
Date MultiplicativePriceSeasonality_t<T>::seasonalityBaseDate() const {
    return seasonalityBaseDate_;
}

template <class T>
Frequency MultiplicativePriceSeasonality_t<T>::frequency() const {
    return frequency_;
}

template <class T>
std::vector<T>
MultiplicativePriceSeasonality_t<T>::seasonalityFactors() const {
    return seasonalityFactors_;
}

template <class T>
T MultiplicativePriceSeasonality_t<T>::correctZeroRate(
    const Date &d, const T r, const InflationTermStructure_t<T> &iTS) const {
    std::pair<Date, Date> lim =
        inflationPeriod(iTS.baseDate(), iTS.frequency());
    Date curveBaseDate = lim.second;
    return seasonalityCorrection(r, d, iTS.dayCounter(), curveBaseDate, true);
}

template <class T>
T MultiplicativePriceSeasonality_t<T>::correctYoYRate(
    const Date &d, const T r, const InflationTermStructure_t<T> &iTS) const {
    std::pair<Date, Date> lim =
        inflationPeriod(iTS.baseDate(), iTS.frequency());
    Date curveBaseDate = lim.second;
    return seasonalityCorrection(r, d, iTS.dayCounter(), curveBaseDate, false);
}

template <class T>
T MultiplicativePriceSeasonality_t<T>::seasonalityFactor(const Date &to) const {

    Date from = this->seasonalityBaseDate();
    Frequency factorFrequency = this->frequency();
    Size nFactors = this->seasonalityFactors().size();
    Period factorPeriod(factorFrequency);
    Size which = 0;
    if (from == to) {
        which = 0;
    } else {
        // days, weeks, months, years are the only time unit possibilities
        Integer diffDays = abs(to - from); // in days
        Integer dir = 1;
        if (from > to)
            dir = -1;
        Integer diff;
        if (factorPeriod.units() == Days) {
            diff = dir * diffDays;
        } else if (factorPeriod.units() == Weeks) {
            diff = dir * (diffDays / 7);
        } else if (factorPeriod.units() == Months) {
            std::pair<Date, Date> lim = inflationPeriod(to, factorFrequency);
            diff = diffDays / (31 * factorPeriod.length());
            Date go = from + dir * diff * factorPeriod;
            while (!(lim.first <= go && go <= lim.second)) {
                go += dir * factorPeriod;
                diff++;
            }
            diff = dir * diff;
        } else if (factorPeriod.units() == Years) {
            QL_FAIL("seasonality period time unit is not allowed to be : "
                    << factorPeriod.units());
        } else {
            QL_FAIL("Unknown time unit: " << factorPeriod.units());
        }
        // now adjust to the available number of factors, direction dependent

        if (dir == 1) {
            which = diff % nFactors;
        } else {
            which = (nFactors - (-diff % nFactors)) % nFactors;
        }
    }

    return seasonalityFactors()[which];
}

template <class T>
T MultiplicativePriceSeasonality_t<T>::seasonalityCorrection(
    T rate, const Date &atDate, const DayCounter &dc, const Date &curveBaseDate,
    const bool isZeroRate) const {
    // need _two_ corrections in order to get: seasonality =
    // factor[atDate-seasonalityBase] / factor[reference-seasonalityBase]
    // i.e. for ZERO inflation rates you have the true fixing at the curve base
    // so this factor must be normalized to one
    //      for YoY inflation rates your reference point is the year before

    T factorAt = this->seasonalityFactor(atDate);

    // Getting seasonality correction for either ZC or YoY
    T f;
    if (isZeroRate) {
        T factorBase = this->seasonalityFactor(curveBaseDate);
        T seasonalityAt = factorAt / factorBase;
        Time timeFromCurveBase = dc.yearFraction(curveBaseDate, atDate);
        f = std::pow(seasonalityAt, 1 / timeFromCurveBase);
    } else {
        T factor1Ybefore = this->seasonalityFactor(atDate - Period(1, Years));
        f = factorAt / factor1Ybefore;
    }

    return (rate + 1) * f - 1;
}

} // end of namespace QuantLib
#endif

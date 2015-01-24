/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2007 Ferdinando Ametrano
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

/*! \file voltermstructure.hpp
    \brief Volatility term structure
*/

#ifndef quantlib_vol_term_structure_hpp
#define quantlib_vol_term_structure_hpp

#include <ql/termstructure.hpp>

namespace QuantLib {

//! Volatility term structure
/*! This abstract class defines the interface of concrete
    volatility structures which will be derived from this one.

*/
template <class T> class VolatilityTermStructure_t : public TermStructure {
  public:
    /*! \name Constructors
        See the TermStructure documentation for issues regarding
        constructors.
    */
    //@{
    /*! \warning term structures initialized by means of this
                 constructor must manage their own reference date
                 by overriding the referenceDate() method.

        \deprecated
    */
    QL_DEPRECATED
    VolatilityTermStructure_t(const Calendar &cal, BusinessDayConvention bdc,
                              const DayCounter &dc = DayCounter());

    /*! \warning term structures initialized by means of this
                 constructor must manage their own reference date
                 by overriding the referenceDate() method.
    */
    VolatilityTermStructure_t(BusinessDayConvention bdc,
                              const DayCounter &dc = DayCounter());
    //! initialize with a fixed reference date
    VolatilityTermStructure_t(const Date &referenceDate, const Calendar &cal,
                              BusinessDayConvention bdc,
                              const DayCounter &dc = DayCounter());
    //! calculate the reference date based on the global evaluation date
    VolatilityTermStructure_t(Natural settlementDays, const Calendar &cal,
                              BusinessDayConvention bdc,
                              const DayCounter &dc = DayCounter());
    //@}
    //! the business day convention used in tenor to date conversion
    virtual BusinessDayConvention businessDayConvention() const;
    //! period/date conversion
    Date optionDateFromTenor(const Period &) const;
    //! the minimum strike for which the term structure can return vols
    virtual T minStrike() const = 0;
    //! the maximum strike for which the term structure can return vols
    virtual T maxStrike() const = 0;

  protected:
    //! strike-range check
    void checkStrike(T strike, bool extrapolate) const;

  private:
    BusinessDayConvention bdc_;
};

typedef VolatilityTermStructure_t<Real> VolatilityTermStructure;

// inline definitions

template <class T>
inline BusinessDayConvention
VolatilityTermStructure_t<T>::businessDayConvention() const {
    return bdc_;
}

template <class T>
inline Date
VolatilityTermStructure_t<T>::optionDateFromTenor(const Period &p) const {
    // swaption style
    return calendar().advance(referenceDate(), p, businessDayConvention());
}

// implementation

template <class T>
VolatilityTermStructure_t<T>::VolatilityTermStructure_t(
    const Calendar &cal, BusinessDayConvention bdc, const DayCounter &dc)
    : TermStructure(dc), bdc_(bdc) {
    calendar_ = cal;
}

template <class T>
VolatilityTermStructure_t<T>::VolatilityTermStructure_t(
    BusinessDayConvention bdc, const DayCounter &dc)
    : TermStructure(dc), bdc_(bdc) {}

template <class T>
VolatilityTermStructure_t<T>::VolatilityTermStructure_t(
    const Date &referenceDate, const Calendar &cal, BusinessDayConvention bdc,
    const DayCounter &dc)
    : TermStructure(referenceDate, cal, dc), bdc_(bdc) {}

template <class T>
VolatilityTermStructure_t<T>::VolatilityTermStructure_t(
    Natural settlementDays, const Calendar &cal, BusinessDayConvention bdc,
    const DayCounter &dc)
    : TermStructure(settlementDays, cal, dc), bdc_(bdc) {}

template <class T>
void VolatilityTermStructure_t<T>::checkStrike(T k, bool extrapolate) const {
    QL_REQUIRE(extrapolate || allowsExtrapolation() ||
                   (k >= minStrike() && k <= maxStrike()),
               "strike (" << k << ") is outside the curve domain ["
                          << minStrike() << "," << maxStrike() << "]");
}
}

#endif

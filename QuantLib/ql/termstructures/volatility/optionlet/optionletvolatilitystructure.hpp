/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2002, 2003 RiskMap srl
 Copyright (C) 2003, 2004, 2005, 2006 StatPro Italia srl
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

/*! \file optionletvolatilitystructure.hpp
    \brief optionlet (caplet/floorlet) volatility structure
*/

#ifndef quantlib_optionlet_volatility_structure_hpp
#define quantlib_optionlet_volatility_structure_hpp

#include <ql/termstructures/voltermstructure.hpp>

namespace QuantLib {

template <class T> class SmileSection_t;
typedef SmileSection_t<Real> SmileSection;

//! Optionlet (caplet/floorlet) volatility structure
/*! This class is purely abstract and defines the interface of
    concrete structures which will be derived from this one.
*/
template <class T>
class OptionletVolatilityStructure_t : public VolatilityTermStructure_t<T> {
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
    OptionletVolatilityStructure_t(const Calendar &cal,
                                   BusinessDayConvention bdc = Following,
                                   const DayCounter &dc = DayCounter());

    //! default constructor
    /*! \warning term structures initialized by means of this
                 constructor must manage their own reference date
                 by overriding the referenceDate() method.
    */
    OptionletVolatilityStructure_t(BusinessDayConvention bdc = Following,
                                   const DayCounter &dc = DayCounter());
    //! initialize with a fixed reference date
    OptionletVolatilityStructure_t(const Date &referenceDate,
                                   const Calendar &cal,
                                   BusinessDayConvention bdc,
                                   const DayCounter &dc = DayCounter());
    //! calculate the reference date based on the global evaluation date
    OptionletVolatilityStructure_t(Natural settlementDays, const Calendar &,
                                   BusinessDayConvention bdc,
                                   const DayCounter &dc = DayCounter());
    //@}
    virtual ~OptionletVolatilityStructure_t() {}
    //! \name Volatility and Variance
    //@{
    //! returns the volatility for a given option tenor and strike rate
    T volatility(const Period &optionTenor, T strike,
                 bool extrapolate = false) const;
    //! returns the volatility for a given option date and strike rate
    T volatility(const Date &optionDate, T strike,
                 bool extrapolate = false) const;
    //! returns the volatility for a given option time and strike rate
    T volatility(Time optionTime, T strike, bool extrapolate = false) const;

    //! returns the Black variance for a given option tenor and strike rate
    T blackVariance(const Period &optionTenor, T strike,
                       bool extrapolate = false) const;
    //! returns the Black variance for a given option date and strike rate
    T blackVariance(const Date &optionDate, T strike,
                       bool extrapolate = false) const;
    //! returns the Black variance for a given option time and strike rate
    T blackVariance(Time optionTime, T strike,
                       bool extrapolate = false) const;

    //! returns the smile for a given option tenor
    boost::shared_ptr<SmileSection_t<T> >
    smileSection(const Period &optionTenor, bool extr = false) const;
    //! returns the smile for a given option date
    boost::shared_ptr<SmileSection_t<T> > smileSection(const Date &optionDate,
                                                       bool extr = false) const;
    //! returns the smile for a given option time
    boost::shared_ptr<SmileSection_t<T> > smileSection(Time optionTime,
                                                       bool extr = false) const;
    //@}
  protected:
    virtual boost::shared_ptr<SmileSection_t<T> >
    smileSectionImpl(const Date &optionDate) const;
    //! implements the actual smile calculation in derived classes
    virtual boost::shared_ptr<SmileSection_t<T> >
    smileSectionImpl(Time optionTime) const = 0;
    virtual T volatilityImpl(const Date &optionDate, T strike) const;
    //! implements the actual volatility calculation in derived classes
    virtual T volatilityImpl(Time optionTime, T strike) const = 0;
};

typedef OptionletVolatilityStructure_t<Real> OptionletVolatilityStructure;

// inline definitions

// 1. Period-based methods convert Period to Date and then
//    use the equivalent Date-based methods
template <class T>
inline T OptionletVolatilityStructure_t<T>::volatility(
    const Period &optionTenor, T strike, bool extrapolate) const {
    Date optionDate = this->optionDateFromTenor(optionTenor);
    return volatility(optionDate, strike, extrapolate);
}

template <class T>
inline T OptionletVolatilityStructure_t<T>::blackVariance(
    const Period &optionTenor, T strike, bool extrapolate) const {
    Date optionDate = this->optionDateFromTenor(optionTenor);
    return blackVariance(optionDate, strike, extrapolate);
}

template <class T>
inline boost::shared_ptr<SmileSection_t<T> >
OptionletVolatilityStructure_t<T>::smileSection(const Period &optionTenor,
                                                bool extrapolate) const {
    Date optionDate = this->optionDateFromTenor(optionTenor);
    return smileSection(optionDate, extrapolate);
}

// 2. blackVariance methods rely on volatility methods
template <class T>
inline T OptionletVolatilityStructure_t<T>::blackVariance(
    const Date &optionDate, T strike, bool extrapolate) const {
    T v = volatility(optionDate, strike, extrapolate);
    Time t = this->timeFromReference(optionDate);
    return v * v * t;
}

template <class T>
inline T
OptionletVolatilityStructure_t<T>::blackVariance(Time optionTime, T strike,
                                                 bool extrapolate) const {
    T v = volatility(optionTime, strike, extrapolate);
    return v * v * optionTime;
}

// 3. relying on xxxImpl methods
template <class T>
inline T OptionletVolatilityStructure_t<T>::volatility(const Date &optionDate,
                                                       T strike,
                                                       bool extrapolate) const {
    this->checkRange(optionDate, extrapolate);
    this->checkStrike(strike, extrapolate);
    return volatilityImpl(optionDate, strike);
}

template <class T>
inline T OptionletVolatilityStructure_t<T>::volatility(Time optionTime,
                                                       T strike,
                                                       bool extrapolate) const {
    this->checkRange(optionTime, extrapolate);
    this->checkStrike(strike, extrapolate);
    return volatilityImpl(optionTime, strike);
}

template <class T>
inline boost::shared_ptr<SmileSection_t<T> >
OptionletVolatilityStructure_t<T>::smileSection(const Date &optionDate,
                                                bool extrapolate) const {
    this->checkRange(optionDate, extrapolate);
    return smileSectionImpl(optionDate);
}

template <class T>
inline boost::shared_ptr<SmileSection_t<T> >
OptionletVolatilityStructure_t<T>::smileSection(Time optionTime,
                                                bool extrapolate) const {
    this->checkRange(optionTime, extrapolate);
    return smileSectionImpl(optionTime);
}

// 4. default implementation of Date-based xxxImpl methods
//    relying on the equivalent Time-based methods
template <class T>
inline boost::shared_ptr<SmileSection_t<T> >
OptionletVolatilityStructure_t<T>::smileSectionImpl(
    const Date &optionDate) const {
    return smileSectionImpl(this->timeFromReference(optionDate));
}

template <class T>
inline T
OptionletVolatilityStructure_t<T>::volatilityImpl(const Date &optionDate,
                                                  T strike) const {
    return volatilityImpl(this->timeFromReference(optionDate), strike);
}

// implementation

template <class T>
OptionletVolatilityStructure_t<T>::OptionletVolatilityStructure_t(
    const Calendar &cal, BusinessDayConvention bdc, const DayCounter &dc)
    : VolatilityTermStructure(bdc, dc) {
    this->calendar_ = cal;
}

template <class T>
OptionletVolatilityStructure_t<T>::OptionletVolatilityStructure_t(
    BusinessDayConvention bdc, const DayCounter &dc)
    : VolatilityTermStructure(bdc, dc) {}

template <class T>
OptionletVolatilityStructure_t<T>::OptionletVolatilityStructure_t(
    const Date &referenceDate, const Calendar &cal, BusinessDayConvention bdc,
    const DayCounter &dc)
    : VolatilityTermStructure(referenceDate, cal, bdc, dc) {}

template <class T>
OptionletVolatilityStructure_t<T>::OptionletVolatilityStructure_t(
    Natural settlementDays, const Calendar &cal, BusinessDayConvention bdc,
    const DayCounter &dc)
    : VolatilityTermStructure(settlementDays, cal, bdc, dc) {}
}

#endif

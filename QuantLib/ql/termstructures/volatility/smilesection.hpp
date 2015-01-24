/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2006 Mario Pucci
 Copyright (C) 2013, 2015 Peter Caspers

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

/*! \file smilesection.hpp
    \brief Smile section base class
*/

#ifndef quantlib_smile_section_hpp
#define quantlib_smile_section_hpp

#include <ql/patterns/observable.hpp>
#include <ql/time/daycounter.hpp>
#include <ql/utilities/null.hpp>
#include <ql/option.hpp>
#include <ql/pricingengines/blackformula.hpp>
#include <ql/settings.hpp>

namespace QuantLib {

//! interest rate volatility smile section
/*! This abstract class provides volatility smile section interface */
template <class T>
class SmileSection_t : public virtual Observable, public virtual Observer {
  public:
    SmileSection_t(const Date &d, const DayCounter &dc = DayCounter(),
                   const Date &referenceDate = Date());
    SmileSection_t(Time exerciseTime, const DayCounter &dc = DayCounter());
    SmileSection_t() {}

    virtual ~SmileSection_t() {}

    virtual void update();
    virtual T minStrike() const = 0;
    virtual T maxStrike() const = 0;
    T variance(T strike) const;
    T volatility(T strike) const;
    virtual T atmLevel() const = 0;
    virtual const Date &exerciseDate() const { return exerciseDate_; }
    virtual const Date &referenceDate() const;
    virtual Time exerciseTime() const { return exerciseTime_; }
    virtual const DayCounter &dayCounter() const { return dc_; }
    virtual T optionPrice(T strike, Option::Type type = Option::Call,
                          T discount = 1.0) const;
    virtual T digitalOptionPrice(T strike, Option::Type type = Option::Call,
                                 T discount = 1.0, T gap = 1.0e-5) const;
    virtual T vega(T strike, T discount = 1.0) const;
    virtual T density(T strike, T discount = 1.0, T gap = 1.0E-4) const;

  protected:
    virtual void initializeExerciseTime() const;
    virtual T varianceImpl(T strike) const;
    virtual T volatilityImpl(T strike) const = 0;

  private:
    bool isFloating_;
    mutable Date referenceDate_;
    Date exerciseDate_;
    DayCounter dc_;
    mutable Time exerciseTime_;
};

typedef SmileSection_t<Real> SmileSection;

// inline definitions

template <class T> inline T SmileSection_t<T>::variance(T strike) const {
    return varianceImpl(strike);
}

template <class T> inline T SmileSection_t<T>::volatility(T strike) const {
    return volatilityImpl(strike);
}

template <class T> inline const Date &SmileSection_t<T>::referenceDate() const {
    QL_REQUIRE(referenceDate_ != Date(),
               "referenceDate not available for this instance");
    return referenceDate_;
}

template <class T> inline T SmileSection_t<T>::varianceImpl(T strike) const {
    T v = volatilityImpl(strike);
    return v * v * exerciseTime();
}

// implementation

template <class T> void SmileSection_t<T>::update() {
    if (isFloating_) {
        referenceDate_ = Settings::instance().evaluationDate();
        initializeExerciseTime();
    }
}

template <class T> void SmileSection_t<T>::initializeExerciseTime() const {
    QL_REQUIRE(exerciseDate_ >= referenceDate_,
               "expiry date (" << exerciseDate_
                               << ") must be greater than reference date ("
                               << referenceDate_ << ")");
    exerciseTime_ = dc_.yearFraction(referenceDate_, exerciseDate_);
}

template <class T>
SmileSection_t<T>::SmileSection_t(const Date &d, const DayCounter &dc,
                                  const Date &referenceDate)
    : exerciseDate_(d), dc_(dc) {
    isFloating_ = referenceDate == Date();
    if (isFloating_) {
        registerWith(Settings::instance().evaluationDate());
        referenceDate_ = Settings::instance().evaluationDate();
    } else
        referenceDate_ = referenceDate;
    initializeExerciseTime();
}

template <class T>
SmileSection_t<T>::SmileSection_t(Time exerciseTime, const DayCounter &dc)
    : isFloating_(false), referenceDate_(Date()), dc_(dc),
      exerciseTime_(exerciseTime) {
    QL_REQUIRE(exerciseTime_ >= 0.0, "expiry time must be positive: "
                                         << exerciseTime_ << " not allowed");
}

template <class T>
T SmileSection_t<T>::optionPrice(T strike, Option::Type type,
                                 T discount) const {
    T atm = atmLevel();
    QL_REQUIRE(atm != Null<T>(),
               "smile section must provide atm level to compute option price");
    // for zero strike, return option price even if outside
    // minstrike, maxstrike interval
    return blackFormula(
        type, strike, atm,
        fabs(strike) < QL_EPSILON ? 0.2 : sqrt(variance(strike)), discount);
}

template <class T>
T SmileSection_t<T>::digitalOptionPrice(T strike, Option::Type type, T discount,
                                        T gap) const {
    T kl = std::max(strike - gap / 2.0, 0.0);
    T kr = kl + gap;
    return (type == Option::Call ? 1.0 : -1.0) *
           (optionPrice(kl, type, discount) - optionPrice(kr, type, discount)) /
           gap;
}

template <class T>
T SmileSection_t<T>::density(T strike, T discount, T gap) const {
    T kl = std::max(strike - gap / 2.0, 0.0);
    T kr = kl + gap;
    return (digitalOptionPrice(kl, Option::Call, discount, gap) -
            digitalOptionPrice(kr, Option::Call, discount, gap)) /
           gap;
}

template <class T> T SmileSection_t<T>::vega(T strike, T discount) const {
    T atm = atmLevel();
    QL_REQUIRE(atm != Null<T>(),
               "smile section must provide atm level to compute option price");
    return blackFormulaVolDerivative(strike, atm, sqrt(variance(strike)),
                                     exerciseTime(), discount) *
           0.01;
}
}

#endif

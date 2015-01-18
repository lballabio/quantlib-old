/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl
 Copyright (C) 2003, 2004, 2005, 2007 StatPro Italia srl
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

/*! \file flatforward.hpp
    \brief flat forward rate term structure
*/

#ifndef quantlib_flat_forward_curve_hpp
#define quantlib_flat_forward_curve_hpp

#include <ql/patterns/lazyobject.hpp>
#include <ql/termstructures/yieldtermstructure.hpp>
#include <ql/quotes/simplequote.hpp>

namespace QuantLib {

//! Flat interest-rate curve
/*! \ingroup yieldtermstructures */
template <class T>
class FlatForward_t : public YieldTermStructure_t<T>, public LazyObject {
  public:
    //! \name Constructors
    //@{
    FlatForward_t(const Date &referenceDate, const Handle<Quote_t<T> > &forward,
                  const DayCounter &dayCounter,
                  Compounding compounding = Continuous,
                  Frequency frequency = Annual);
    FlatForward_t(const Date &referenceDate, T forward,
                  const DayCounter &dayCounter,
                  Compounding compounding = Continuous,
                  Frequency frequency = Annual);
    FlatForward_t(Natural settlementDays, const Calendar &calendar,
                  const Handle<Quote_t<T> > &forward,
                  const DayCounter &dayCounter,
                  Compounding compounding = Continuous,
                  Frequency frequency = Annual);
    FlatForward_t(Natural settlementDays, const Calendar &calendar, T forward,
                  const DayCounter &dayCounter,
                  Compounding compounding = Continuous,
                  Frequency frequency = Annual);
    //@}

    // inspectors
    Compounding compounding() const { return compounding_; }
    Frequency compoundingFrequency() const { return frequency_; }

    //! \name TermStructure interface
    //@{
    Date maxDate() const { return Date::maxDate(); }
    //@}

    //! \name Observer interface
    //@{
    void update();
    //@}
  private:
    //! \name LazyObject interface
    //@{
    void performCalculations() const;
    //@}

    //! \name YieldTermStructure implementation
    //@{
    T discountImpl(Time) const;
    //@}

    Handle<Quote_t<T> > forward_;
    Compounding compounding_;
    Frequency frequency_;
    mutable InterestRate rate_;
};

typedef FlatForward_t<Real> FlatForward;

// inline definitions

template <class T> inline void FlatForward_t<T>::update() {
    LazyObject::update();
    YieldTermStructure_t<T>::update();
}

template <class T> inline T FlatForward_t<T>::discountImpl(Time t) const {
    this->calculate();
    return rate_.discountFactor(t);
}

template <class T> inline void FlatForward_t<T>::performCalculations() const {
    rate_ = InterestRate(forward_->value(), this->dayCounter(), compounding_,
                         frequency_);
}

// implementation

template <class T>
FlatForward_t<T>::FlatForward_t(const Date &referenceDate,
                                const Handle<Quote_t<T> > &forward,
                                const DayCounter &dayCounter,
                                Compounding compounding, Frequency frequency)
    : YieldTermStructure_t<T>(referenceDate, Calendar(), dayCounter),
      forward_(forward), compounding_(compounding), frequency_(frequency) {
    this->registerWith(forward_);
}

template <class T>
FlatForward_t<T>::FlatForward_t(const Date &referenceDate, T forward,
                                const DayCounter &dayCounter,
                                Compounding compounding, Frequency frequency)
    : YieldTermStructure_t<T>(referenceDate, Calendar(), dayCounter),
      forward_(boost::shared_ptr<Quote_t<T> >(new SimpleQuote_t<T>(forward))),
      compounding_(compounding), frequency_(frequency) {}

template <class T>
FlatForward_t<T>::FlatForward_t(Natural settlementDays,
                                const Calendar &calendar,
                                const Handle<Quote_t<T> > &forward,
                                const DayCounter &dayCounter,
                                Compounding compounding, Frequency frequency)
    : YieldTermStructure_t<T>(settlementDays, calendar, dayCounter),
      forward_(forward), compounding_(compounding), frequency_(frequency) {
    registerWith(forward_);
}

template <class T>
FlatForward_t<T>::FlatForward_t(Natural settlementDays,
                                const Calendar &calendar, T forward,
                                const DayCounter &dayCounter,
                                Compounding compounding, Frequency frequency)
    : YieldTermStructure_t<T>(settlementDays, calendar, dayCounter),
      forward_(boost::shared_ptr<Quote_t<T> >(new SimpleQuote_t<T>(forward))),
      compounding_(compounding), frequency_(frequency) {}
}

#endif

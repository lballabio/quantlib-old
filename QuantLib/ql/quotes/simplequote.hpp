/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2007 Ferdinando Ametrano
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl
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

/*! \file simplequote.hpp
    \brief simple quote class
*/

#ifndef quantlib_simple_quote_hpp
#define quantlib_simple_quote_hpp

#include <ql/quote.hpp>

namespace QuantLib {

//! market element returning a stored value
template <class T = Real> class SimpleQuote_t : public Quote_t<T> {
  public:
    SimpleQuote_t(T value = Null<Real>());
    //! \name Quote interface
    //@{
    T value() const;
    bool isValid() const;
    //@}
    //! \name Modifiers
    //@{
    //! returns the difference between the new value and the old value
    T setValue(T value = Null<Real>());
    void reset();
    //@}
  private:
    T value_;
};

typedef SimpleQuote_t<Real> SimpleQuote;

// inline definitions

template <class T>
inline SimpleQuote_t<T>::SimpleQuote_t(T value)
    : value_(value) {}

template <class T> inline T SimpleQuote_t<T>::value() const {
    QL_ENSURE(isValid(), "invalid SimpleQuote");
    return value_;
}

template <class T> inline bool SimpleQuote_t<T>::isValid() const {
    return value_ != Null<Real>();
}

template <class T> inline T SimpleQuote_t<T>::setValue(T value) {
    T diff = value - value_;
    if (diff != 0.0) {
        value_ = value;
        SimpleQuote_t<T>::notifyObservers();
    }
    return diff;
}

template <class T> inline void SimpleQuote_t<T>::reset() {
    setValue(Null<Real>());
}
} // namespace QuantLib

#endif

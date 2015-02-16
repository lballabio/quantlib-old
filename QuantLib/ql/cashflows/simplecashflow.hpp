/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl
 Copyright (C) 2010 StatPro Italia srl
 Copyright (C) 2015 Cheng Li

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

/*! \file simplecashflow.hpp
    \brief Predetermined cash flow
*/

#ifndef quantlib_simple_cash_flow_hpp
#define quantlib_simple_cash_flow_hpp

#include <ql/patterns/visitor.hpp>
#include <ql/cashflow.hpp>

namespace QuantLib {

//! Predetermined cash flow
/*! This cash flow pays a predetermined amount at a given date. */
template <class T = Real> class SimpleCashFlow_t : public CashFlow_t<T> {
  public:
    SimpleCashFlow_t(T amount, const Date &date);
    //! \name Event interface
    //@{
    Date date() const { return date_; }
    void setDate(const Date &newDate) { date_ = newDate; }
    //@}
    //! \name CashFlow interface
    //@{
    T amount() const { return amount_; }

    //@}
    //! \name Visitability
    //@{
    virtual void accept(AcyclicVisitor &);
    //@}
  private:
    T amount_;
    Date date_;
};

typedef SimpleCashFlow_t<Real> SimpleCashFlow;

template <class T>
SimpleCashFlow_t<T>::SimpleCashFlow_t(T amount, const Date &date)
    : amount_(amount), date_(date) {
    QL_REQUIRE(date_ != Date(), "null date SimpleCashFlow");

    QL_REQUIRE(amount_ != Null<Real>(), "null amount SimpleCashFlow");
}

//! Bond redemption
/*! This class specializes SimpleCashFlow so that visitors
    can perform more detailed cash-flow analysis.
*/

template <class T = Real> class Redemption_t : public SimpleCashFlow_t<T> {
  public:
    Redemption_t(T amount, const Date &date)
        : SimpleCashFlow_t<T>(amount, date) {}
    //! \name Visitability
    //@{
    virtual void accept(AcyclicVisitor &);
    //@}
};

typedef Redemption_t<Real> Redemption;

//! Amortizing payment
/*! This class specializes SimpleCashFlow so that visitors
    can perform more detailed cash-flow analysis.
*/
template <class T = Real>
class AmortizingPayment_t : public SimpleCashFlow_t<T> {
  public:
    AmortizingPayment_t(T amount, const Date &date)
        : SimpleCashFlow_t<T>(amount, date) {}
    //! \name Visitability
    //@{
    virtual void accept(AcyclicVisitor &);
    //@}
};

typedef AmortizingPayment_t<Real> AmortizingPayment;

// inline definitions
template <class T> inline void SimpleCashFlow_t<T>::accept(AcyclicVisitor &v) {
    Visitor<SimpleCashFlow_t<T> > *v1 =
        dynamic_cast<Visitor<SimpleCashFlow_t<T> > *>(&v);
    if (v1 != 0)
        v1->visit(*this);
    else
        CashFlow_t<T>::accept(v);
}

template <class T> inline void Redemption_t<T>::accept(AcyclicVisitor &v) {
    Visitor<Redemption_t<T> > *v1 =
        dynamic_cast<Visitor<Redemption_t<T> > *>(&v);
    if (v1 != 0)
        v1->visit(*this);
    else
        SimpleCashFlow_t<T>::accept(v);
}

template <class T>
inline void AmortizingPayment_t<T>::accept(AcyclicVisitor &v) {
    Visitor<AmortizingPayment_t<T> > *v1 =
        dynamic_cast<Visitor<AmortizingPayment_t<T> > *>(&v);
    if (v1 != 0)
        v1->visit(*this);
    else
        SimpleCashFlow_t<T>::accept(v);
}
}

#endif

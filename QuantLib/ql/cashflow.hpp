/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl
 Copyright (C) 2003, 2004, 2005, 2006, 2007, 2009 StatPro Italia srl
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

/*! \file cashflow.hpp
    \brief Base class for cash flows
*/

#ifndef quantlib_cash_flow_hpp
#define quantlib_cash_flow_hpp

#include <ql/event.hpp>
#include <ql/math/comparison.hpp>
#include <ql/settings.hpp>
#include <ql/patterns/visitor.hpp>

#include <vector>

namespace QuantLib {

//! Base class for cash flows
/*! This class is purely virtual and acts as a base class for the
    actual cash flow implementations.
*/
template <class T> class CashFlow_t : public Event {
  public:
    virtual ~CashFlow_t() {}
    //! \name Event interface
    //@{
    //! \note This is inherited from the event class
    virtual Date date() const = 0;
    //! returns true if an event has already occurred before a date
    /*! overloads Event::hasOccurred in order to take QL_TODAYS_PAYMENTS
        in account
    */
    bool hasOccurred(const Date &refDate = Date(),
                     boost::optional<bool> includeRefDate = boost::none) const;
    //@}
    //! \name CashFlow interface
    //@{
    //! returns the amount of the cash flow
    /*! \note The amount is not discounted, i.e., it is the actual
              amount paid at the cash flow date.
    */
    virtual T amount() const = 0;
    //! returns the date that the cash flow trades exCoupon
    virtual Date exCouponDate() const { return Date(); };
    //! returns true if the cashflow is trading ex-coupon on the refDate
    bool tradingExCoupon(const Date &refDate = Date()) const;

    //@}
    //! \name Visitability
    //@{
    virtual void accept(AcyclicVisitor &);
    //@}
};

typedef CashFlow_t<Real> CashFlow;

//! Sequence of cash-flows

template <class T> struct Leg_t {
    typedef std::vector<boost::shared_ptr<CashFlow_t<T> > > Type;
};

typedef Leg_t<Real>::Type Leg;

template <class T>
struct earlier_than<CashFlow_t<T> >
    : public std::binary_function<CashFlow_t<T>, CashFlow_t<T>, bool> {
    bool operator()(const CashFlow_t<T> &c1, const CashFlow_t<T> &c2) {
        return c1.date() < c2.date();
    }
};

// implementation

template <class T>
bool CashFlow_t<T>::hasOccurred(const Date &refDate,
                                boost::optional<bool> includeRefDate) const {

    // easy and quick handling of most cases
    if (refDate != Date()) {
        Date cf = date();
        if (refDate < cf)
            return false;
        if (cf < refDate)
            return true;
    }

    if (refDate == Date() || refDate == Settings::instance().evaluationDate()) {
        // today's date; we override the bool with the one
        // specified in the settings (if any)
        boost::optional<bool> includeToday =
            Settings::instance().includeTodaysCashFlows();
        if (includeToday)
            includeRefDate = *includeToday;
    }
    return Event::hasOccurred(refDate, includeRefDate);
}

template <class T>
bool CashFlow_t<T>::tradingExCoupon(const Date &refDate) const {

    Date ecd = exCouponDate();
    if (ecd == Date())
        return false;

    Date ref =
        refDate != Date() ? refDate : Settings::instance().evaluationDate();

    return ecd <= ref;
}

template <class T> void CashFlow_t<T>::accept(AcyclicVisitor &v) {
    Visitor<CashFlow_t<T> > *v1 = dynamic_cast<Visitor<CashFlow_t<T> > *>(&v);
    if (v1 != 0)
        v1->visit(*this);
    else
        Event::accept(v);
}
}

#endif

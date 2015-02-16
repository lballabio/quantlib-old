/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2005, 2006 StatPro Italia srl
 Copyright (C) 2005 Charles Whitmore
 Copyright (C) 2007, 2008, 2009, 2010, 2011 Ferdinando Ametrano
 Copyright (C) 2008 Toyin Akin
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

/*! \file cashflows.hpp
    \brief Cash-flow analysis functions
*/

#ifndef quantlib_cashflows_hpp
#define quantlib_cashflows_hpp

#include <ql/cashflows/duration.hpp>
#include <ql/cashflow.hpp>
#include <ql/interestrate.hpp>
#include <boost/shared_ptr.hpp>
#include <ql/termstructures/yieldtermstructure.hpp>
#include <ql/cashflows/coupon.hpp>
#include <ql/termstructures/yield/flatforward.hpp>
#include <ql/math/solvers1d/brent.hpp>
#include <ql/math/solvers1d/newtonsafe.hpp>
#include <ql/cashflows/couponpricerbase.hpp>
#include <ql/patterns/visitor.hpp>
#include <ql/quotes/simplequote.hpp>
#include <ql/termstructures/yield/zerospreadedtermstructure.hpp>

using boost::shared_ptr;
using boost::dynamic_pointer_cast;

namespace QuantLib {

//! %cashflow-analysis functions
/*! \todo add tests */
class CashFlows {
  private:
    CashFlows();
    CashFlows(const CashFlows &);

  public:
    //! \name Date functions
    //@{
    template <class T>
    static Date startDate(const typename Leg_t<T>::Type &leg);
    template <class T>
    static Date maturityDate(const typename Leg_t<T>::Type &leg);
    template <class T>
    static bool isExpired(const typename Leg_t<T>::Type &leg,
                          bool includeSettlementDateFlows,
                          Date settlementDate = Date());
    //@}

    //! \name CashFlow functions
    //@{
    //! the last cashflow paying before or at the given date
    template <class T>
    static typename Leg_t<T>::Type::const_reverse_iterator
    previousCashFlow(const typename Leg_t<T>::Type &leg,
                     bool includeSettlementDateFlows,
                     Date settlementDate = Date());
    //! the first cashflow paying after the given date
    template <class T>
    static typename Leg_t<T>::Type::const_iterator
    nextCashFlow(const typename Leg_t<T>::Type &leg,
                 bool includeSettlementDateFlows, Date settlementDate = Date());
    template <class T>
    static Date previousCashFlowDate(const typename Leg_t<T>::Type &leg,
                                     bool includeSettlementDateFlows,
                                     Date settlementDate = Date());
    template <class T>
    static Date nextCashFlowDate(const typename Leg_t<T>::Type &leg,
                                 bool includeSettlementDateFlows,
                                 Date settlementDate = Date());
    template <class T>
    static T previousCashFlowAmount(const typename Leg_t<T>::Type &leg,
                                    bool includeSettlementDateFlows,
                                    Date settlementDate = Date());
    template <class T>
    static T nextCashFlowAmount(const typename Leg_t<T>::Type &leg,
                                bool includeSettlementDateFlows,
                                Date settlementDate = Date());
    //@}

    //! \name Coupon inspectors
    //@{
    template <class T>
    static T previousCouponRate(const typename Leg_t<T>::Type &leg,
                                bool includeSettlementDateFlows,
                                Date settlementDate = Date());
    template <class T>
    static T nextCouponRate(const typename Leg_t<T>::Type &leg,
                            bool includeSettlementDateFlows,
                            Date settlementDate = Date());

    template <class T>
    static T nominal(const typename Leg_t<T>::Type &leg,
                     bool includeSettlementDateFlows, Date settlDate = Date());
    template <class T>
    static Date accrualStartDate(const typename Leg_t<T>::Type &leg,
                                 bool includeSettlementDateFlows,
                                 Date settlDate = Date());
    template <class T>
    static Date accrualEndDate(const typename Leg_t<T>::Type &leg,
                               bool includeSettlementDateFlows,
                               Date settlementDate = Date());
    template <class T>
    static Date referencePeriodStart(const typename Leg_t<T>::Type &leg,
                                     bool includeSettlementDateFlows,
                                     Date settlDate = Date());
    template <class T>
    static Date referencePeriodEnd(const typename Leg_t<T>::Type &leg,
                                   bool includeSettlementDateFlows,
                                   Date settlDate = Date());
    template <class T>
    static Time accrualPeriod(const typename Leg_t<T>::Type &leg,
                              bool includeSettlementDateFlows,
                              Date settlementDate = Date());
    template <class T>
    static BigInteger accrualDays(const typename Leg_t<T>::Type &leg,
                                  bool includeSettlementDateFlows,
                                  Date settlementDate = Date());
    template <class T>
    static Time accruedPeriod(const typename Leg_t<T>::Type &leg,
                              bool includeSettlementDateFlows,
                              Date settlementDate = Date());
    template <class T>
    static BigInteger accruedDays(const typename Leg_t<T>::Type &leg,
                                  bool includeSettlementDateFlows,
                                  Date settlementDate = Date());
    template <class T>
    static T accruedAmount(const typename Leg_t<T>::Type &leg,
                           bool includeSettlementDateFlows,
                           Date settlementDate = Date());
    //@}

    //! \name YieldTermStructure functions
    //@{
    //! NPV of the cash flows.
    /*! The NPV is the sum of the cash flows, each discounted
        according to the given term structure.
    */
    template <class T>
    static T npv(const typename Leg_t<T>::Type &leg,
                 const YieldTermStructure_t<T> &discountCurve,
                 bool includeSettlementDateFlows, Date settlementDate = Date(),
                 Date npvDate = Date());
    //! Basis-point sensitivity of the cash flows.
    /*! The result is the change in NPV due to a uniform
        1-basis-point change in the rate paid by the cash
        flows. The change for each coupon is discounted according
        to the given term structure.
    */
    template <class T>
    static T bps(const typename Leg_t<T>::Type &leg,
                 const YieldTermStructure_t<T> &discountCurve,
                 bool includeSettlementDateFlows, Date settlementDate = Date(),
                 Date npvDate = Date());

    //@{
    //! NPV and BPS of the cash flows.
    /*! The NPV and BPS of the cash flows calculated
        together for performance reason
    */
    template <class T>
    static void npvbps(const typename Leg_t<T>::Type &leg,
                       const YieldTermStructure_t<T> &discountCurve,
                       bool includeSettlementDateFlows, Date settlementDate,
                       Date npvDate, T &npv, T &bps);

    //! At-the-money rate of the cash flows.
    /*! The result is the fixed rate for which a fixed rate cash flow
        vector, equivalent to the input vector, has the required NPV
        according to the given term structure. If the required NPV is
        not given, the input cash flow vector's NPV is used instead.
    */
    template <class T>
    static T atmRate(const typename Leg_t<T>::Type &leg,
                     const YieldTermStructure_t<T> &discountCurve,
                     bool includeSettlementDateFlows,
                     Date settlementDate = Date(), Date npvDate = Date(),
                     T npv = Null<Real>());
    //@}

    //! \name Yield (a.k.a. Internal Rate of Return, i.e. IRR) functions
    /*! The IRR is the interest rate at which the NPV of the cash
        flows equals the dirty price.
    */
    //@{
    //! NPV of the cash flows.
    /*! The NPV is the sum of the cash flows, each discounted
        according to the given constant interest rate.  The result
        is affected by the choice of the interest-rate compounding
        and the relative frequency and day counter.
    */
    template <class T>
    static T npv(const typename Leg_t<T>::Type &leg,
                 const InterestRate_t<T> &yield,
                 bool includeSettlementDateFlows, Date settlementDate = Date(),
                 Date npvDate = Date());
    template <class T>
    static T npv(const typename Leg_t<T>::Type &leg, T yield,
                 const DayCounter &dayCounter, Compounding compounding,
                 Frequency frequency, bool includeSettlementDateFlows,
                 Date settlementDate = Date(), Date npvDate = Date());
    //! Basis-point sensitivity of the cash flows.
    /*! The result is the change in NPV due to a uniform
        1-basis-point change in the rate paid by the cash
        flows. The change for each coupon is discounted according
        to the given constant interest rate.  The result is
        affected by the choice of the interest-rate compounding
        and the relative frequency and day counter.
    */
    template <class T>
    static T bps(const typename Leg_t<T>::Type &leg,
                 const InterestRate_t<T> &yield,
                 bool includeSettlementDateFlows, Date settlementDate = Date(),
                 Date npvDate = Date());
    template <class T>
    static T bps(const typename Leg_t<T>::Type &leg, T yield,
                 const DayCounter &dayCounter, Compounding compounding,
                 Frequency frequency, bool includeSettlementDateFlows,
                 Date settlementDate = Date(), Date npvDate = Date());
    //! Implied internal rate of return.
    /*! The function verifies
        the theoretical existance of an IRR and numerically
        establishes the IRR to the desired precision.
    */
    template <class T>
    static T yield(const typename Leg_t<T>::Type &leg, T npv,
                   const DayCounter &dayCounter, Compounding compounding,
                   Frequency frequency, bool includeSettlementDateFlows,
                   Date settlementDate = Date(), Date npvDate = Date(),
                   T accuracy = 1.0e-10, Size maxIterations = 100,
                   T guess = 0.05);

    //! Cash-flow duration.
    /*! The simple duration of a string of cash flows is defined as
        \f[
        D_{\mathrm{simple}} = \frac{\sum t_i c_i B(t_i)}{\sum c_i B(t_i)}
        \f]
        where \f$ c_i \f$ is the amount of the \f$ i \f$-th cash
        flow, \f$ t_i \f$ is its payment time, and \f$ B(t_i) \f$
        is the corresponding discount according to the passed yield.

        The modified duration is defined as
        \f[
        D_{\mathrm{modified}} = -\frac{1}{P} \frac{\partial P}{\partial y}
        \f]
        where \f$ P \f$ is the present value of the cash flows
        according to the given IRR \f$ y \f$.

        The Macaulay duration is defined for a compounded IRR as
        \f[
        D_{\mathrm{Macaulay}} = \left( 1 + \frac{y}{N} \right)
                                D_{\mathrm{modified}}
        \f]
        where \f$ y \f$ is the IRR and \f$ N \f$ is the number of
        cash flows per year.
    */
    template <class T>
    static Time duration(const typename Leg_t<T>::Type &leg,
                         const InterestRate &yield, Duration::Type type,
                         bool includeSettlementDateFlows,
                         Date settlementDate = Date(), Date npvDate = Date());
    template <class T>
    static Time duration(const typename Leg_t<T>::Type &leg, T yield,
                         const DayCounter &dayCounter, Compounding compounding,
                         Frequency frequency, Duration::Type type,
                         bool includeSettlementDateFlows,
                         Date settlementDate = Date(), Date npvDate = Date());

    //! Cash-flow convexity
    /*! The convexity of a string of cash flows is defined as
        \f[
        C = \frac{1}{P} \frac{\partial^2 P}{\partial y^2}
        \f]
        where \f$ P \f$ is the present value of the cash flows
        according to the given IRR \f$ y \f$.
    */
    template <class T>
    static T convexity(const typename Leg_t<T>::Type &leg,
                       const InterestRate_t<T> &yield,
                       bool includeSettlementDateFlows,
                       Date settlementDate = Date(), Date npvDate = Date());
    template <class T>
    static T convexity(const typename Leg_t<T>::Type &leg, T yield,
                       const DayCounter &dayCounter, Compounding compounding,
                       Frequency frequency, bool includeSettlementDateFlows,
                       Date settlementDate = Date(), Date npvDate = Date());

    //! Basis-point value
    /*! Obtained by setting dy = 0.0001 in the 2nd-order Taylor
        series expansion.
    */
    template <class T>
    static T basisPointValue(const typename Leg_t<T>::Type &leg,
                             const InterestRate_t<T> &yield,
                             bool includeSettlementDateFlows,
                             Date settlementDate = Date(),
                             Date npvDate = Date());
    template <class T>
    static T
    basisPointValue(const typename Leg_t<T>::Type &leg, T yield,
                    const DayCounter &dayCounter, Compounding compounding,
                    Frequency frequency, bool includeSettlementDateFlows,
                    Date settlementDate = Date(), Date npvDate = Date());

    //! Yield value of a basis point
    /*! The yield value of a one basis point change in price is
        the derivative of the yield with respect to the price
        multiplied by 0.01
    */
    template <class T>
    static T yieldValueBasisPoint(const typename Leg_t<T>::Type &leg,
                                  const InterestRate_t<T> &yield,
                                  bool includeSettlementDateFlows,
                                  Date settlementDate = Date(),
                                  Date npvDate = Date());
    template <class T>
    static T
    yieldValueBasisPoint(const typename Leg_t<T>::Type &leg, T yield,
                         const DayCounter &dayCounter, Compounding compounding,
                         Frequency frequency, bool includeSettlementDateFlows,
                         Date settlementDate = Date(), Date npvDate = Date());
    //@}

    //! \name Z-spread functions
    /*! For details on z-spread refer to:
        "Credit Spreads Explained", Lehman Brothers European Fixed
        Income Research - March 2004, D. O'Kane
    */
    //@{
    //! NPV of the cash flows.
    /*! The NPV is the sum of the cash flows, each discounted
        according to the z-spreaded term structure.  The result
        is affected by the choice of the z-spread compounding
        and the relative frequency and day counter.
    */
    template <class T>
    static T npv(const typename Leg_t<T>::Type &leg,
                 const boost::shared_ptr<YieldTermStructure_t<T> > &discount,
                 T zSpread, const DayCounter &dayCounter,
                 Compounding compounding, Frequency frequency,
                 bool includeSettlementDateFlows, Date settlementDate = Date(),
                 Date npvDate = Date());
    //! implied Z-spread.
    template <class T>
    static T zSpread(const typename Leg_t<T>::Type &leg, T npv,
                     const boost::shared_ptr<YieldTermStructure_t<T> > &,
                     const DayCounter &dayCounter, Compounding compounding,
                     Frequency frequency, bool includeSettlementDateFlows,
                     Date settlementDate = Date(), Date npvDate = Date(),
                     T accuracy = 1.0e-10, Size maxIterations = 100,
                     T guess = 0.0);
    //! deprecated implied Z-spread.
    template <class T>
    static T
    zSpread(const typename Leg_t<T>::Type &leg,
            const boost::shared_ptr<YieldTermStructure_t<T> > &d, T npv,
            const DayCounter &dayCounter, Compounding compounding,
            Frequency frequency, bool includeSettlementDateFlows,
            Date settlementDate = Date(), Date npvDate = Date(),
            T accuracy = 1.0e-10, Size maxIterations = 100, T guess = 0.0) {
        return zSpread(leg, npv, d, dayCounter, compounding, frequency,
                       includeSettlementDateFlows, settlementDate, npvDate,
                       accuracy, maxIterations, guess);
    }
    //@}
}; // CashFLows_t

// implementaiton

// Date inspectors

template <class T>
Date CashFlows::startDate(const typename Leg_t<T>::Type &leg) {
    QL_REQUIRE(!leg.empty(), "empty leg");

    Date d = Date::maxDate();
    for (Size i = 0; i < leg.size(); ++i) {
        boost::shared_ptr<Coupon_t<T> > c = dynamic_pointer_cast<Coupon_t<T> >(leg[i]);
        if (c)
            d = std::min(d, c->accrualStartDate());
        else
            d = std::min(d, leg[i]->date());
    }
    return d;
}

template <class T>
Date CashFlows::maturityDate(const typename Leg_t<T>::Type &leg) {
    QL_REQUIRE(!leg.empty(), "empty leg");

    Date d = Date::minDate();
    for (Size i = 0; i < leg.size(); ++i) {
        boost::shared_ptr<Coupon_t<T> > c = dynamic_pointer_cast<Coupon_t<T> >(leg[i]);
        if (c)
            d = std::max(d, c->accrualEndDate());
        else
            d = std::max(d, leg[i]->date());
    }
    return d;
}

template <class T>
bool CashFlows::isExpired(const typename Leg_t<T>::Type &leg,
                          bool includeSettlementDateFlows,
                          Date settlementDate) {
    if (leg.empty())
        return true;

    if (settlementDate == Date())
        settlementDate = Settings::instance().evaluationDate();

    for (Size i = leg.size(); i > 0; --i)
        if (!leg[i - 1]->hasOccurred(settlementDate,
                                     includeSettlementDateFlows))
            return false;
    return true;
}

template <class T>
typename Leg_t<T>::Type::const_reverse_iterator
CashFlows::previousCashFlow(const typename Leg_t<T>::Type &leg,
                            bool includeSettlementDateFlows,
                            Date settlementDate) {
    if (leg.empty())
        return leg.rend();

    if (settlementDate == Date())
        settlementDate = Settings::instance().evaluationDate();

    typename Leg_t<T>::Type::const_reverse_iterator i;
    for (i = leg.rbegin(); i < leg.rend(); ++i) {
        if ((*i)->hasOccurred(settlementDate, includeSettlementDateFlows))
            return i;
    }
    return leg.rend();
}

template <class T>
typename Leg_t<T>::Type::const_iterator
CashFlows::nextCashFlow(const typename Leg_t<T>::Type &leg,
                        bool includeSettlementDateFlows, Date settlementDate) {
    if (leg.empty())
        return leg.end();

    if (settlementDate == Date())
        settlementDate = Settings::instance().evaluationDate();

    typename Leg_t<T>::Type::const_iterator i;
    for (i = leg.begin(); i < leg.end(); ++i) {
        if (!(*i)->hasOccurred(settlementDate, includeSettlementDateFlows))
            return i;
    }
    return leg.end();
}

template <class T>
Date CashFlows::previousCashFlowDate(const typename Leg_t<T>::Type &leg,
                                     bool includeSettlementDateFlows,
                                     Date settlementDate) {
    typename Leg_t<T>::Type::const_reverse_iterator cf;
    cf = previousCashFlow<T>(leg, includeSettlementDateFlows, settlementDate);

    if (cf == leg.rend())
        return Date();

    return (*cf)->date();
}

template <class T>
Date CashFlows::nextCashFlowDate(const typename Leg_t<T>::Type &leg,
                                 bool includeSettlementDateFlows,
                                 Date settlementDate) {
    typename Leg_t<T>::Type::const_iterator cf;
    cf = nextCashFlow<T>(leg, includeSettlementDateFlows, settlementDate);

    if (cf == leg.end())
        return Date();

    return (*cf)->date();
}

template <class T>
T CashFlows::previousCashFlowAmount(const typename Leg_t<T>::Type &leg,
                                    bool includeSettlementDateFlows,
                                    Date settlementDate) {
    typename Leg_t<T>::Type::const_reverse_iterator cf;
    cf = previousCashFlow<T>(leg, includeSettlementDateFlows, settlementDate);

    if (cf == leg.rend())
        return T();

    Date paymentDate = (*cf)->date();
    T result = 0.0;
    for (; cf < leg.rend() && (*cf)->date() == paymentDate; ++cf)
        result += (*cf)->amount();
    return result;
}

template <class T>
T CashFlows::nextCashFlowAmount(const typename Leg_t<T>::Type &leg,
                                bool includeSettlementDateFlows,
                                Date settlementDate) {
    typename Leg_t<T>::Type::const_iterator cf;
    cf = nextCashFlow<T>(leg, includeSettlementDateFlows, settlementDate);

    if (cf == leg.end())
        return T();

    Date paymentDate = (*cf)->date();
    T result = 0.0;
    for (; cf < leg.end() && (*cf)->date() == paymentDate; ++cf)
        result += (*cf)->amount();
    return result;
}

// Coupon utility functions
namespace {

template <typename Iter, class T>
T aggregateRate(const typename Leg_t<T>::Type &leg, Iter first, Iter last) {
    if (first == last)
        return 0.0;

    Date paymentDate = (*first)->date();
    bool firstCouponFound = false;
    T nominal = 0.0;
    Time accrualPeriod = 0.0;
    DayCounter dc;
    T result = 0.0;
    for (; first < last && (*first)->date() == paymentDate; ++first) {
        boost::shared_ptr<Coupon_t<T> > cp =
            dynamic_pointer_cast<Coupon_t<T> >(*first);
        if (cp) {
            if (firstCouponFound) {
                QL_REQUIRE(nominal == cp->nominal() &&
                               accrualPeriod == cp->accrualPeriod() &&
                               dc == cp->dayCounter(),
                           "cannot aggregate two different coupons on "
                               << paymentDate);
            } else {
                firstCouponFound = true;
                nominal = cp->nominal();
                accrualPeriod = cp->accrualPeriod();
                dc = cp->dayCounter();
            }
            result += cp->rate();
        }
    }
    QL_ENSURE(firstCouponFound, "no coupon paid at cashflow date "
                                    << paymentDate);
    return result;
}

} // anonymous namespace ends here

template <class T>
T CashFlows::previousCouponRate(const typename Leg_t<T>::Type &leg,
                                bool includeSettlementDateFlows,
                                Date settlementDate) {
    typename Leg_t<T>::Type::const_reverse_iterator cf;
    cf = previousCashFlow<T>(leg, includeSettlementDateFlows, settlementDate);

    return aggregateRate<typename Leg_t<T>::Type::const_reverse_iterator, T>(
        leg, cf, leg.rend());
}

template <class T>
T CashFlows::nextCouponRate(const typename Leg_t<T>::Type &leg,
                            bool includeSettlementDateFlows,
                            Date settlementDate) {
    typename Leg_t<T>::Type::const_iterator cf;
    cf = nextCashFlow<T>(leg, includeSettlementDateFlows, settlementDate);
    return aggregateRate<typename Leg_t<T>::Type::const_iterator, T>(leg, cf,
                                                                     leg.end());
}

template <class T>
T CashFlows::nominal(const typename Leg_t<T>::Type &leg,
                     bool includeSettlementDateFlows, Date settlementDate) {
    typename Leg_t<T>::Type::const_iterator cf =
        nextCashFlow<T>(leg, includeSettlementDateFlows, settlementDate);
    if (cf == leg.end())
        return 0.0;

    Date paymentDate = (*cf)->date();
    for (; cf < leg.end() && (*cf)->date() == paymentDate; ++cf) {
        boost::shared_ptr<Coupon_t<T> > cp = dynamic_pointer_cast<Coupon_t<T> >(*cf);
        if (cp)
            return cp->nominal();
    }
    return 0.0;
}

template <class T>
Date CashFlows::accrualStartDate(const typename Leg_t<T>::Type &leg,
                                 bool includeSettlementDateFlows,
                                 Date settlementDate) {
    typename Leg_t<T>::Type::const_iterator cf =
        nextCashFlow<T>(leg, includeSettlementDateFlows, settlementDate);
    if (cf == leg.end())
        return Date();

    Date paymentDate = (*cf)->date();
    for (; cf < leg.end() && (*cf)->date() == paymentDate; ++cf) {
        boost::shared_ptr<Coupon_t<T> > cp = dynamic_pointer_cast<Coupon_t<T> >(*cf);
        if (cp)
            return cp->accrualStartDate();
    }
    return Date();
}

template <class T>
Date CashFlows::accrualEndDate(const typename Leg_t<T>::Type &leg,
                               bool includeSettlementDateFlows,
                               Date settlementDate) {
    typename Leg_t<T>::Type::const_iterator cf =
        nextCashFlow<T>(leg, includeSettlementDateFlows, settlementDate);
    if (cf == leg.end())
        return Date();

    Date paymentDate = (*cf)->date();
    for (; cf < leg.end() && (*cf)->date() == paymentDate; ++cf) {
        boost::shared_ptr<Coupon_t<T> > cp = dynamic_pointer_cast<Coupon_t<T> >(*cf);
        if (cp)
            return cp->accrualEndDate();
    }
    return Date();
}

template <class T>
Date CashFlows::referencePeriodStart(const typename Leg_t<T>::Type &leg,
                                     bool includeSettlementDateFlows,
                                     Date settlementDate) {
    typename Leg_t<T>::Type::const_iterator cf =
        nextCashFlow<T>(leg, includeSettlementDateFlows, settlementDate);
    if (cf == leg.end())
        return Date();

    Date paymentDate = (*cf)->date();
    for (; cf < leg.end() && (*cf)->date() == paymentDate; ++cf) {
        boost::shared_ptr<Coupon_t<T> > cp = dynamic_pointer_cast<Coupon_t<T> >(*cf);
        if (cp)
            return cp->referencePeriodStart();
    }
    return Date();
}

template <class T>
Date CashFlows::referencePeriodEnd(const typename Leg_t<T>::Type &leg,
                                   bool includeSettlementDateFlows,
                                   Date settlementDate) {
    typename Leg_t<T>::Type::const_iterator cf =
        nextCashFlow<T>(leg, includeSettlementDateFlows, settlementDate);
    if (cf == leg.end())
        return Date();

    Date paymentDate = (*cf)->date();
    for (; cf < leg.end() && (*cf)->date() == paymentDate; ++cf) {
        boost::shared_ptr<Coupon_t<T> > cp = dynamic_pointer_cast<Coupon_t<T> >(*cf);
        if (cp)
            return cp->referencePeriodEnd();
    }
    return Date();
}

template <class T>
Time CashFlows::accrualPeriod(const typename Leg_t<T>::Type &leg,
                              bool includeSettlementDateFlows,
                              Date settlementDate) {
    typename Leg_t<T>::Type::const_iterator cf =
        nextCashFlow<T>(leg, includeSettlementDateFlows, settlementDate);
    if (cf == leg.end())
        return 0;

    Date paymentDate = (*cf)->date();
    for (; cf < leg.end() && (*cf)->date() == paymentDate; ++cf) {
        boost::shared_ptr<Coupon_t<T> > cp = dynamic_pointer_cast<Coupon_t<T> >(*cf);
        if (cp)
            return cp->accrualPeriod();
    }
    return 0;
}

template <class T>
BigInteger CashFlows::accrualDays(const typename Leg_t<T>::Type &leg,
                                  bool includeSettlementDateFlows,
                                  Date settlementDate) {
    typename Leg_t<T>::Type::const_iterator cf =
        nextCashFlow<T>(leg, includeSettlementDateFlows, settlementDate);
    if (cf == leg.end())
        return 0;

    Date paymentDate = (*cf)->date();
    for (; cf < leg.end() && (*cf)->date() == paymentDate; ++cf) {
        boost::shared_ptr<Coupon_t<T> > cp = dynamic_pointer_cast<Coupon_t<T> >(*cf);
        if (cp)
            return cp->accrualDays();
    }
    return 0;
}

template <class T>
Time CashFlows::accruedPeriod(const typename Leg_t<T>::Type &leg,
                              bool includeSettlementDateFlows,
                              Date settlementDate) {
    if (settlementDate == Date())
        settlementDate = Settings::instance().evaluationDate();

    typename Leg_t<T>::Type::const_iterator cf =
        nextCashFlow<T>(leg, includeSettlementDateFlows, settlementDate);
    if (cf == leg.end())
        return 0;

    Date paymentDate = (*cf)->date();
    for (; cf < leg.end() && (*cf)->date() == paymentDate; ++cf) {
        boost::shared_ptr<Coupon_t<T> > cp = dynamic_pointer_cast<Coupon_t<T> >(*cf);
        if (cp)
            return cp->accruedPeriod(settlementDate);
    }
    return 0;
}

template <class T>
BigInteger CashFlows::accruedDays(const typename Leg_t<T>::Type &leg,
                                  bool includeSettlementDateFlows,
                                  Date settlementDate) {
    if (settlementDate == Date())
        settlementDate = Settings::instance().evaluationDate();

    typename Leg_t<T>::Type::const_iterator cf =
        nextCashFlow<T>(leg, includeSettlementDateFlows, settlementDate);
    if (cf == leg.end())
        return 0;

    Date paymentDate = (*cf)->date();
    for (; cf < leg.end() && (*cf)->date() == paymentDate; ++cf) {
        boost::shared_ptr<Coupon_t<T> > cp = dynamic_pointer_cast<Coupon_t<T> >(*cf);
        if (cp)
            return cp->accruedDays(settlementDate);
    }
    return 0;
}

template <class T>
T CashFlows::accruedAmount(const typename Leg_t<T>::Type &leg,
                           bool includeSettlementDateFlows,
                           Date settlementDate) {
    if (settlementDate == Date())
        settlementDate = Settings::instance().evaluationDate();

    typename Leg_t<T>::Type::const_iterator cf =
        nextCashFlow<T>(leg, includeSettlementDateFlows, settlementDate);
    if (cf == leg.end())
        return 0.0;

    Date paymentDate = (*cf)->date();
    T result = 0.0;
    for (; cf < leg.end() && (*cf)->date() == paymentDate; ++cf) {
        boost::shared_ptr<Coupon_t<T> > cp = dynamic_pointer_cast<Coupon_t<T> >(*cf);
        if (cp)
            result += cp->accruedAmount(settlementDate);
    }
    return result;
}

// YieldTermStructure utility functions
namespace {

template <class T>
class BPSCalculator : public AcyclicVisitor,
                      public Visitor<CashFlow_t<T> >,
                      public Visitor<Coupon_t<T> > {
  public:
    BPSCalculator(const YieldTermStructure_t<T> &discountCurve)
        : discountCurve_(discountCurve), bps_(0.0), nonSensNPV_(0.0) {}
    void visit(Coupon_t<T> &c) {
        T bps =
            c.nominal() * c.accrualPeriod() * discountCurve_.discount(c.date());
        bps_ += bps;
    }
    void visit(CashFlow_t<T> &cf) {
        nonSensNPV_ += cf.amount() * discountCurve_.discount(cf.date());
    }
    T bps() const { return bps_; }
    T nonSensNPV() const { return nonSensNPV_; }

  private:
    const YieldTermStructure_t<T> &discountCurve_;
    T bps_, nonSensNPV_;
};

const Spread basisPoint_ = 1.0e-4;
} // anonymous namespace ends here

template <class T>
T CashFlows::npv(const typename Leg_t<T>::Type &leg,
                 const YieldTermStructure_t<T> &discountCurve,
                 bool includeSettlementDateFlows, Date settlementDate,
                 Date npvDate) {

    if (leg.empty())
        return 0.0;

    if (settlementDate == Date())
        settlementDate = Settings::instance().evaluationDate();

    if (npvDate == Date())
        npvDate = settlementDate;

    T totalNPV = 0.0;
    for (Size i = 0; i < leg.size(); ++i) {
        if (!leg[i]->hasOccurred(settlementDate, includeSettlementDateFlows) &&
            !leg[i]->tradingExCoupon(settlementDate))
            totalNPV +=
                leg[i]->amount() * discountCurve.discount(leg[i]->date());
    }

    return totalNPV / discountCurve.discount(npvDate);
}

template <class T>
T CashFlows::bps(const typename Leg_t<T>::Type &leg,
                 const YieldTermStructure_t<T> &discountCurve,
                 bool includeSettlementDateFlows, Date settlementDate,
                 Date npvDate) {
    if (leg.empty())
        return 0.0;

    if (settlementDate == Date())
        settlementDate = Settings::instance().evaluationDate();

    if (npvDate == Date())
        npvDate = settlementDate;

    BPSCalculator<T> calc(discountCurve);
    for (Size i = 0; i < leg.size(); ++i) {
        if (!leg[i]->hasOccurred(settlementDate, includeSettlementDateFlows) &&
            !leg[i]->tradingExCoupon(settlementDate))
            leg[i]->accept(calc);
    }
    return basisPoint_ * calc.bps() / discountCurve.discount(npvDate);
}

template <class T>
void CashFlows::npvbps(const typename Leg_t<T>::Type &leg,
                       const YieldTermStructure_t<T> &discountCurve,
                       bool includeSettlementDateFlows, Date settlementDate,
                       Date npvDate, T &npv, T &bps) {

    npv = 0.0;
    if (leg.empty()) {
        bps = 0.0;
        return;
    }

    BPSCalculator<T> calc(discountCurve);
    for (Size i = 0; i < leg.size(); ++i) {
        CashFlow_t<T> &cf = *leg[i];
        if (!cf.hasOccurred(settlementDate, includeSettlementDateFlows) &&
            !cf.tradingExCoupon(settlementDate)) {
            npv += cf.amount() * discountCurve.discount(cf.date());
            cf.accept(calc);
        }
    }
    T d = discountCurve.discount(npvDate);
    npv /= d;
    bps = basisPoint_ * calc.bps() / d;
}

template <class T>
T CashFlows::atmRate(const typename Leg_t<T>::Type &leg,
                     const YieldTermStructure_t<T> &discountCurve,
                     bool includeSettlementDateFlows, Date settlementDate,
                     Date npvDate, T targetNpv) {

    if (settlementDate == Date())
        settlementDate = Settings::instance().evaluationDate();

    if (npvDate == Date())
        npvDate = settlementDate;

    T npv = 0.0;
    BPSCalculator<T> calc(discountCurve);
    for (Size i = 0; i < leg.size(); ++i) {
        CashFlow_t<T> &cf = *leg[i];
        if (!cf.hasOccurred(settlementDate, includeSettlementDateFlows) &&
            !cf.tradingExCoupon(settlementDate)) {
            npv += cf.amount() * discountCurve.discount(cf.date());
            cf.accept(calc);
        }
    }

    if (targetNpv == Null<Real>())
        targetNpv = npv - calc.nonSensNPV();
    else {
        targetNpv *= discountCurve.discount(npvDate);
        targetNpv -= calc.nonSensNPV();
    }

    if (targetNpv == 0.0)
        return 0.0;

    T bps = calc.bps();
    QL_REQUIRE(bps != 0.0, "null bps: impossible atm rate");

    return targetNpv / bps;
}

// IRR utility functions
namespace {

template <class T> Integer sign(T x) {
    static T zero = T();
    if (x == zero)
        return 0;
    else if (x > zero)
        return 1;
    else
        return -1;
}

template <class T>
T simpleDuration(const typename Leg_t<T>::Type &leg, const InterestRate &y,
                 bool includeSettlementDateFlows, Date settlementDate,
                 Date npvDate) {
    if (leg.empty())
        return 0.0;

    if (settlementDate == Date())
        settlementDate = Settings::instance().evaluationDate();

    if (npvDate == Date())
        npvDate = settlementDate;

    T P = 0.0;
    T dPdy = 0.0;
    Time t = 0.0;
    Date lastDate = npvDate;
    Date refStartDate, refEndDate;
    const DayCounter &dc = y.dayCounter();
    for (Size i = 0; i < leg.size(); ++i) {
        if (leg[i]->hasOccurred(settlementDate, includeSettlementDateFlows))
            continue;

        T c = leg[i]->amount();
        if (leg[i]->tradingExCoupon(settlementDate)) {
            c = 0.0;
        }

        Date couponDate = leg[i]->date();
        boost::shared_ptr<Coupon> coupon = boost::dynamic_pointer_cast<Coupon>(leg[i]);
        if (coupon) {
            refStartDate = coupon->referencePeriodStart();
            refEndDate = coupon->referencePeriodEnd();
        } else {
            if (lastDate == npvDate) {
                // we don't have a previous coupon date,
                // so we fake it
                refStartDate = couponDate - 1 * Years;
            } else {
                refStartDate = lastDate;
            }
            refEndDate = couponDate;
        }

        t += dc.yearFraction(lastDate, couponDate, refStartDate, refEndDate);

        T B = y.discountFactor(t);
        P += c * B;
        dPdy += t * c * B;

        lastDate = couponDate;
    }
    if (P == 0.0) // no cashflows
        return 0.0;
    return dPdy / P;
}

template <class T>
T modifiedDuration(const typename Leg_t<T>::Type &leg, const InterestRate &y,
                   bool includeSettlementDateFlows, Date settlementDate,
                   Date npvDate) {
    if (leg.empty())
        return 0.0;

    if (settlementDate == Date())
        settlementDate = Settings::instance().evaluationDate();

    if (npvDate == Date())
        npvDate = settlementDate;

    T P = 0.0;
    Time t = 0.0;
    T dPdy = 0.0;
    T r = y.rate();
    Natural N = y.frequency();
    Date lastDate = npvDate;
    Date refStartDate, refEndDate;
    const DayCounter &dc = y.dayCounter();
    for (Size i = 0; i < leg.size(); ++i) {
        if (leg[i]->hasOccurred(settlementDate, includeSettlementDateFlows))
            continue;

        T c = leg[i]->amount();
        if (leg[i]->tradingExCoupon(settlementDate)) {
            c = 0.0;
        }

        Date couponDate = leg[i]->date();
        boost::shared_ptr<Coupon> coupon = boost::dynamic_pointer_cast<Coupon>(leg[i]);
        if (coupon) {
            refStartDate = coupon->referencePeriodStart();
            refEndDate = coupon->referencePeriodEnd();
        } else {
            if (lastDate == npvDate) {
                // we don't have a previous coupon date,
                // so we fake it
                refStartDate = couponDate - 1 * Years;
            } else {
                refStartDate = lastDate;
            }
            refEndDate = couponDate;
        }

        t += dc.yearFraction(lastDate, couponDate, refStartDate, refEndDate);

        T B = y.discountFactor(t);
        P += c * B;
        switch (y.compounding()) {
        case Simple:
            dPdy -= c * B * B * t;
            break;
        case Compounded:
            dPdy -= c * t * B / (1 + r / N);
            break;
        case Continuous:
            dPdy -= c * B * t;
            break;
        case SimpleThenCompounded:
            if (t <= 1.0 / N)
                dPdy -= c * B * B * t;
            else
                dPdy -= c * t * B / (1 + r / N);
            break;
        default:
            QL_FAIL("unknown compounding convention ("
                    << Integer(y.compounding()) << ")");
        }
        lastDate = couponDate;
    }

    if (P == 0.0) // no cashflows
        return 0.0;
    return -dPdy / P; // reverse derivative sign
}

template <class T>
T macaulayDuration(const typename Leg_t<T>::Type &leg, const InterestRate &y,
                   bool includeSettlementDateFlows, Date settlementDate,
                   Date npvDate) {

    QL_REQUIRE(y.compounding() == Compounded, "compounded rate required");

    return (1.0 + y.rate() / y.frequency()) *
           modifiedDuration<T>(leg, y, includeSettlementDateFlows,
                               settlementDate, npvDate);
}

template <class T> class IrrFinder : public std::unary_function<T, T> {
  public:
    IrrFinder(const typename Leg_t<T>::Type &leg, T npv,
              const DayCounter &dayCounter, Compounding comp, Frequency freq,
              bool includeSettlementDateFlows, Date settlementDate,
              Date npvDate)
        : leg_(leg), npv_(npv), dayCounter_(dayCounter), compounding_(comp),
          frequency_(freq),
          includeSettlementDateFlows_(includeSettlementDateFlows),
          settlementDate_(settlementDate), npvDate_(npvDate) {

        if (settlementDate == Date())
            settlementDate = Settings::instance().evaluationDate();

        if (npvDate == Date())
            npvDate = settlementDate;

        checkSign();
    }
    T operator()(T y) const {
        InterestRate_t<T> yield(y, dayCounter_, compounding_, frequency_);
        T NPV = CashFlows::npv<T>(leg_, yield, includeSettlementDateFlows_,
                                  settlementDate_, npvDate_);
        return npv_ - NPV;
    }
    T derivative(T y) const {
        InterestRate_t<T> yield(y, dayCounter_, compounding_, frequency_);
        return modifiedDuration<T>(leg_, yield, includeSettlementDateFlows_,
                                   settlementDate_, npvDate_);
    }

  private:
    void checkSign() const {
        // depending on the sign of the market price, check that cash
        // flows of the opposite sign have been specified (otherwise
        // IRR is nonsensical.)

        Integer lastSign = sign(-npv_), signChanges = 0;
        for (Size i = 0; i < leg_.size(); ++i) {
            if (!leg_[i]->hasOccurred(settlementDate_,
                                      includeSettlementDateFlows_) &&
                !leg_[i]->tradingExCoupon(settlementDate_)) {
                Integer thisSign = sign(leg_[i]->amount());
                if (lastSign * thisSign < 0) // sign change
                    signChanges++;

                if (thisSign != 0)
                    lastSign = thisSign;
            }
        }
        QL_REQUIRE(signChanges > 0,
                   "the given cash flows cannot result in the given market "
                   "price due to their sign");

        /* The following is commented out due to the lack of a QL_WARN macro
        if (signChanges > 1) {    // Danger of non-unique solution
                                  // Check the aggregate cash flows (Norstrom)
            T aggregateCashFlow = npv;
            signChanges = 0;
            for (Size i = 0; i < leg.size(); ++i) {
                T nextAggregateCashFlow =
                    aggregateCashFlow + leg[i]->amount();

                if (aggregateCashFlow * nextAggregateCashFlow < 0.0)
                    signChanges++;

                aggregateCashFlow = nextAggregateCashFlow;
            }
            if (signChanges > 1)
                QL_WARN( "danger of non-unique solution");
        };
        */
    }
    const typename Leg_t<T>::Type &leg_;
    T npv_;
    DayCounter dayCounter_;
    Compounding compounding_;
    Frequency frequency_;
    bool includeSettlementDateFlows_;
    Date settlementDate_, npvDate_;
};

} // anonymous namespace ends here

template <class T>
T CashFlows::npv(const typename Leg_t<T>::Type &leg, const InterestRate_t<T> &y,
                 bool includeSettlementDateFlows, Date settlementDate,
                 Date npvDate) {

    if (leg.empty())
        return 0.0;

    if (settlementDate == Date())
        settlementDate = Settings::instance().evaluationDate();

    if (npvDate == Date())
        npvDate = settlementDate;

    T npv = 0.0;
    T discount = 1.0;
    Date lastDate = npvDate;
    Date refStartDate, refEndDate;

    for (Size i = 0; i < leg.size(); ++i) {
        if (leg[i]->hasOccurred(settlementDate, includeSettlementDateFlows))
            continue;

        Date couponDate = leg[i]->date();
        T amount = leg[i]->amount();
        if (leg[i]->tradingExCoupon(settlementDate)) {
            amount = 0.0;
        }

        boost::shared_ptr<Coupon> coupon = boost::dynamic_pointer_cast<Coupon>(leg[i]);
        if (coupon) {
            refStartDate = coupon->referencePeriodStart();
            refEndDate = coupon->referencePeriodEnd();
        } else {
            if (lastDate == npvDate) {
                // we don't have a previous coupon date,
                // so we fake it
                refStartDate = couponDate - 1 * Years;
            } else {
                refStartDate = lastDate;
            }
            refEndDate = couponDate;
        }
        T b = y.discountFactor(lastDate, couponDate, refStartDate, refEndDate);
        discount *= b;
        lastDate = couponDate;

        npv += amount * discount;
    }

    return npv;
}

template <class T>
T CashFlows::npv(const typename Leg_t<T>::Type &leg, T yield,
                 const DayCounter &dc, Compounding comp, Frequency freq,
                 bool includeSettlementDateFlows, Date settlementDate,
                 Date npvDate) {
    return npv(leg, InterestRate(yield, dc, comp, freq),
               includeSettlementDateFlows, settlementDate, npvDate);
}

template <class T>
T CashFlows::bps(const typename Leg_t<T>::Type &leg,
                 const InterestRate_t<T> &yield,
                 bool includeSettlementDateFlows, Date settlementDate,
                 Date npvDate) {

    if (leg.empty())
        return 0.0;

    if (settlementDate == Date())
        settlementDate = Settings::instance().evaluationDate();

    if (npvDate == Date())
        npvDate = settlementDate;

    FlatForward_t<T> flatRate(settlementDate, yield.rate(), yield.dayCounter(),
                              yield.compounding(), yield.frequency());
    return bps(leg, flatRate, includeSettlementDateFlows, settlementDate,
               npvDate);
}

template <class T>
T CashFlows::bps(const typename Leg_t<T>::Type &leg, T yield,
                 const DayCounter &dc, Compounding comp, Frequency freq,
                 bool includeSettlementDateFlows, Date settlementDate,
                 Date npvDate) {
    return bps(leg, InterestRate(yield, dc, comp, freq),
               includeSettlementDateFlows, settlementDate, npvDate);
}

template <class T>
T CashFlows::yield(const typename Leg_t<T>::Type &leg, T npv,
                   const DayCounter &dayCounter, Compounding compounding,
                   Frequency frequency, bool includeSettlementDateFlows,
                   Date settlementDate, Date npvDate, T accuracy,
                   Size maxIterations, T guess) {
    // Brent solver;
    NewtonSafe_t<T> solver;
    solver.setMaxEvaluations(maxIterations);
    IrrFinder<T> objFunction(leg, npv, dayCounter, compounding, frequency,
                             includeSettlementDateFlows, settlementDate,
                             npvDate);
    return solver.solve(objFunction, accuracy, guess, guess / 10.0);
}

template <class T>
Time CashFlows::duration(const typename Leg_t<T>::Type &leg,
                         const InterestRate &rate, Duration::Type type,
                         bool includeSettlementDateFlows, Date settlementDate,
                         Date npvDate) {

    if (leg.empty())
        return 0.0;

    if (settlementDate == Date())
        settlementDate = Settings::instance().evaluationDate();

    if (npvDate == Date())
        npvDate = settlementDate;

    switch (type) {
    case Duration::Simple:
        return simpleDuration<T>(leg, rate, includeSettlementDateFlows,
                                 settlementDate, npvDate);
    case Duration::Modified:
        return modifiedDuration<T>(leg, rate, includeSettlementDateFlows,
                                   settlementDate, npvDate);
    case Duration::Macaulay:
        return macaulayDuration<T>(leg, rate, includeSettlementDateFlows,
                                   settlementDate, npvDate);
    default:
        QL_FAIL("unknown duration type");
    }
}

template <class T>
Time CashFlows::duration(const typename Leg_t<T>::Type &leg, T yield,
                         const DayCounter &dc, Compounding comp, Frequency freq,
                         Duration::Type type, bool includeSettlementDateFlows,
                         Date settlementDate, Date npvDate) {
    return duration<T>(leg, InterestRate(yield, dc, comp, freq), type,
                       includeSettlementDateFlows, settlementDate, npvDate);
}

template <class T>
T CashFlows::convexity(const typename Leg_t<T>::Type &leg,
                       const InterestRate_t<T> &y,
                       bool includeSettlementDateFlows, Date settlementDate,
                       Date npvDate) {
    if (leg.empty())
        return 0.0;

    if (settlementDate == Date())
        settlementDate = Settings::instance().evaluationDate();

    if (npvDate == Date())
        npvDate = settlementDate;

    const DayCounter &dc = y.dayCounter();

    T P = 0.0;
    Time t = 0.0;
    T d2Pdy2 = 0.0;
    T r = y.rate();
    Natural N = y.frequency();
    Date lastDate = npvDate;
    Date refStartDate, refEndDate;
    for (Size i = 0; i < leg.size(); ++i) {
        if (leg[i]->hasOccurred(settlementDate, includeSettlementDateFlows))
            continue;

        T c = leg[i]->amount();
        if (leg[i]->tradingExCoupon(settlementDate)) {
            c = 0.0;
        }

        Date couponDate = leg[i]->date();
        boost::shared_ptr<Coupon> coupon = boost::dynamic_pointer_cast<Coupon>(leg[i]);
        if (coupon) {
            refStartDate = coupon->referencePeriodStart();
            refEndDate = coupon->referencePeriodEnd();
        } else {
            if (lastDate == npvDate) {
                // we don't have a previous coupon date,
                // so we fake it
                refStartDate = couponDate - 1 * Years;
            } else {
                refStartDate = lastDate;
            }
            refEndDate = couponDate;
        }

        t += dc.yearFraction(lastDate, couponDate, refStartDate, refEndDate);

        T B = y.discountFactor(t);
        P += c * B;
        switch (y.compounding()) {
        case Simple:
            d2Pdy2 += c * 2.0 * B * B * B * t * t;
            break;
        case Compounded:
            d2Pdy2 += c * B * t * (N * t + 1) / (N * (1 + r / N) * (1 + r / N));
            break;
        case Continuous:
            d2Pdy2 += c * B * t * t;
            break;
        case SimpleThenCompounded:
            if (t <= 1.0 / N)
                d2Pdy2 += c * 2.0 * B * B * B * t * t;
            else
                d2Pdy2 +=
                    c * B * t * (N * t + 1) / (N * (1 + r / N) * (1 + r / N));
            break;
        default:
            QL_FAIL("unknown compounding convention ("
                    << Integer(y.compounding()) << ")");
        }
        lastDate = couponDate;
    }

    if (P == 0.0)
        // no cashflows
        return 0.0;

    return d2Pdy2 / P;
}

template <class T>
T CashFlows::convexity(const typename Leg_t<T>::Type &leg, T yield,
                       const DayCounter &dc, Compounding comp, Frequency freq,
                       bool includeSettlementDateFlows, Date settlementDate,
                       Date npvDate) {
    return convexity(leg, InterestRate(yield, dc, comp, freq),
                     includeSettlementDateFlows, settlementDate, npvDate);
}

template <class T>
T CashFlows::basisPointValue(const typename Leg_t<T>::Type &leg,
                             const InterestRate_t<T> &y,
                             bool includeSettlementDateFlows,
                             Date settlementDate, Date npvDate) {
    if (leg.empty())
        return 0.0;

    if (settlementDate == Date())
        settlementDate = Settings::instance().evaluationDate();

    if (npvDate == Date())
        npvDate = settlementDate;

    T npv = CashFlows::npv(leg, y, includeSettlementDateFlows, settlementDate,
                           npvDate);
    T modifiedDuration = CashFlows::duration<T>(leg, y, Duration::Modified,
                                             includeSettlementDateFlows,
                                             settlementDate, npvDate);
    T convexity = CashFlows::convexity(leg, y, includeSettlementDateFlows,
                                       settlementDate, npvDate);
    T delta = -modifiedDuration * npv;
    T gamma = (convexity / 100.0) * npv;

    T shift = 0.0001;
    delta *= shift;
    gamma *= shift * shift;

    return delta + 0.5 * gamma;
}

template <class T>
T CashFlows::basisPointValue(const typename Leg_t<T>::Type &leg, T yield,
                             const DayCounter &dc, Compounding comp,
                             Frequency freq, bool includeSettlementDateFlows,
                             Date settlementDate, Date npvDate) {
    return basisPointValue(leg, InterestRate(yield, dc, comp, freq),
                           includeSettlementDateFlows, settlementDate, npvDate);
}

template <class T>
T CashFlows::yieldValueBasisPoint(const typename Leg_t<T>::Type &leg,
                                  const InterestRate_t<T> &y,
                                  bool includeSettlementDateFlows,
                                  Date settlementDate, Date npvDate) {
    if (leg.empty())
        return 0.0;

    if (settlementDate == Date())
        settlementDate = Settings::instance().evaluationDate();

    if (npvDate == Date())
        npvDate = settlementDate;

    T npv = CashFlows::npv(leg, y, includeSettlementDateFlows, settlementDate,
                           npvDate);
    T modifiedDuration = CashFlows::duration<T>(leg, y, Duration::Modified,
                                                includeSettlementDateFlows,
                                                settlementDate, npvDate);

    T shift = 0.01;
    return (1.0 / (-npv * modifiedDuration)) * shift;
}

template <class T>
T CashFlows::yieldValueBasisPoint(const typename Leg_t<T>::Type &leg, T yield,
                                  const DayCounter &dc, Compounding comp,
                                  Frequency freq,
                                  bool includeSettlementDateFlows,
                                  Date settlementDate, Date npvDate) {
    return yieldValueBasisPoint(leg, InterestRate(yield, dc, comp, freq),
                                includeSettlementDateFlows, settlementDate,
                                npvDate);
}

// Z-spread utility functions
namespace {

template <class T> class ZSpreadFinder : public std::unary_function<T, T> {
  public:
    ZSpreadFinder(const typename Leg_t<T>::Type &leg,
                  const boost::shared_ptr<YieldTermStructure_t<T> > &discountCurve,
                  T npv, const DayCounter &dc, Compounding comp, Frequency freq,
                  bool includeSettlementDateFlows, Date settlementDate,
                  Date npvDate)
        : leg_(leg), npv_(npv), zSpread_(new SimpleQuote_t<T>(0.0)),
          curve_(Handle<YieldTermStructure_t<T> >(discountCurve),
                 Handle<Quote_t<T> >(zSpread_), comp, freq, dc),
          includeSettlementDateFlows_(includeSettlementDateFlows),
          settlementDate_(settlementDate), npvDate_(npvDate) {

        if (settlementDate == Date())
            settlementDate = Settings::instance().evaluationDate();

        if (npvDate == Date())
            npvDate = settlementDate;

        // if the discount curve allows extrapolation, let's
        // the spreaded curve do too.
        curve_.enableExtrapolation(discountCurve->allowsExtrapolation());
    }
    T operator()(T zSpread) const {
        zSpread_->setValue(zSpread);
        T NPV = CashFlows::npv(leg_, curve_, includeSettlementDateFlows_,
                               settlementDate_, npvDate_);
        return npv_ - NPV;
    }

  private:
    const typename Leg_t<T>::Type &leg_;
    T npv_;
    boost::shared_ptr<SimpleQuote_t<T> > zSpread_;
    ZeroSpreadedTermStructure_t<T> curve_;
    bool includeSettlementDateFlows_;
    Date settlementDate_, npvDate_;
};

} // anonymous namespace ends here

template <class T>
T CashFlows::npv(const typename Leg_t<T>::Type &leg,
                 const boost::shared_ptr<YieldTermStructure_t<T> > &discountCurve,
                 T zSpread, const DayCounter &dc, Compounding comp,
                 Frequency freq, bool includeSettlementDateFlows,
                 Date settlementDate, Date npvDate) {

    if (leg.empty())
        return 0.0;

    if (settlementDate == Date())
        settlementDate = Settings::instance().evaluationDate();

    if (npvDate == Date())
        npvDate = settlementDate;

    Handle<YieldTermStructure> discountCurveHandle(discountCurve);
    Handle<Quote> zSpreadQuoteHandle(
        boost::shared_ptr<Quote>(new SimpleQuote(zSpread)));

    ZeroSpreadedTermStructure spreadedCurve(discountCurveHandle,
                                            zSpreadQuoteHandle, comp, freq, dc);

    spreadedCurve.enableExtrapolation(
        discountCurveHandle->allowsExtrapolation());

    return npv(leg, spreadedCurve, includeSettlementDateFlows, settlementDate,
               npvDate);
}

template <class T>
T CashFlows::zSpread(const typename Leg_t<T>::Type &leg, T npv,
                     const boost::shared_ptr<YieldTermStructure_t<T> > &discount,
                     const DayCounter &dayCounter, Compounding compounding,
                     Frequency frequency, bool includeSettlementDateFlows,
                     Date settlementDate, Date npvDate, T accuracy,
                     Size maxIterations, T guess) {

    if (settlementDate == Date())
        settlementDate = Settings::instance().evaluationDate();

    if (npvDate == Date())
        npvDate = settlementDate;

    Brent_t<T> solver;
    solver.setMaxEvaluations(maxIterations);
    ZSpreadFinder<T> objFunction(leg, discount, npv, dayCounter, compounding,
                                 frequency, includeSettlementDateFlows,
                                 settlementDate, npvDate);
    T step = 0.01;
    return solver.solve(objFunction, accuracy, guess, step);
}
} // namespace QuantLib

#endif

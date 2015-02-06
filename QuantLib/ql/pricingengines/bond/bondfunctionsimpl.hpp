/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
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

/*! \file bondfunctionsimpl.hpp
    \brief bond functions
*/

#ifndef quantlib_bond_functions_impl_hpp
#define quantlib_bond_functions_impl_hpp

#include <ql/cashflows/cashflows.hpp>
#include <ql/instruments/bond.hpp>
#include <ql/interestrate.hpp>

namespace QuantLib {

template <class T> class YieldTermStructure_t;

namespace impl {

template <class T> Date startDate(const Bond_t<T> &bond) {
    return CashFlows::startDate<T>(bond.cashflows());
}
template <class T> Date maturityDate(const Bond_t<T> &bond) {
    return CashFlows::maturityDate<T>(bond.cashflows());
}

template <class T> bool isTradable(const Bond_t<T> &bond, Date settlement) {
    if (settlement == Date())
        settlement = bond.settlementDate();

    return bond.notional(settlement) != 0.0;
}

template <class T>
typename Leg_t<T>::Type::const_reverse_iterator
previousCashFlow(const Bond_t<T> &bond, Date settlement = Date()) {
    if (settlement == Date())
        settlement = bond.settlementDate();

    return CashFlows::previousCashFlow<T>(bond.cashflows(), false, settlement);
}

template <class T>
typename Leg_t<T>::Type::const_iterator nextCashFlow(const Bond_t<T> &bond,
                                                     Date settlement = Date()) {
    if (settlement == Date())
        settlement = bond.settlementDate();

    return CashFlows::nextCashFlow<T>(bond.cashflows(), false, settlement);
}

template <class T>
Date previousCashFlowDate(const Bond_t<T> &bond, Date settlement) {
    if (settlement == Date())
        settlement = bond.settlementDate();

    return CashFlows::previousCashFlowDate<T>(bond.cashflows(), false,
                                              settlement);
}

template <class T>
Date nextCashFlowDate(const Bond_t<T> &bond, Date settlement) {
    if (settlement == Date())
        settlement = bond.settlementDate();

    return CashFlows::nextCashFlowDate<T>(bond.cashflows(), false, settlement);
}

template <class T>
T previousCashFlowAmount(const Bond_t<T> &bond, Date settlement = Date()) {
    if (settlement == Date())
        settlement = bond.settlementDate();

    return CashFlows::previousCashFlowAmount<T>(bond.cashflows(), false,
                                                settlement);
}

template <class T>
T nextCashFlowAmount(const Bond_t<T> &bond, Date settlement = Date()) {
    if (settlement == Date())
        settlement = bond.settlementDate();

    return CashFlows::nextCashFlowAmount<T>(bond.cashflows(), false,
                                            settlement);
}

template <class T>
T previousCouponRate(const Bond_t<T> &bond, Date settlement) {
    if (settlement == Date())
        settlement = bond.settlementDate();

    return CashFlows::previousCouponRate<T>(bond.cashflows(), false,
                                            settlement);
}

template <class T> T nextCouponRate(const Bond_t<T> &bond, Date settlement) {
    if (settlement == Date())
        settlement = bond.settlementDate();

    return CashFlows::nextCouponRate<T>(bond.cashflows(), false, settlement);
}

template <class T>
Date accrualStartDate(const Bond_t<T> &bond, Date settlement = Date()) {
    if (settlement == Date())
        settlement = bond.settlementDate();

    QL_REQUIRE(isTradable<T>(bond, settlement),
               "non tradable at " << settlement << " (maturity being "
                                  << bond.maturityDate() << ")");

    return CashFlows::accrualStartDate<T>(bond.cashflows(), false, settlement);
}

template <class T>
Date accrualEndDate(const Bond_t<T> &bond, Date settlement = Date()) {
    if (settlement == Date())
        settlement = bond.settlementDate();

    QL_REQUIRE(isTradable<T>(bond, settlement),
               "non tradable at " << settlement << " (maturity being "
                                  << bond.maturityDate() << ")");

    return CashFlows::accrualEndDate<T>(bond.cashflows(), false, settlement);
}

template <class T>
Date referencePeriodStart(const Bond_t<T> &bond, Date settlement = Date()) {
    if (settlement == Date())
        settlement = bond.settlementDate();

    QL_REQUIRE(isTradable<T>(bond, settlement),
               "non tradable at " << settlement << " (maturity being "
                                  << bond.maturityDate() << ")");

    return CashFlows::referencePeriodStart<T>(bond.cashflows(), false,
                                              settlement);
}

template <class T>
Date referencePeriodEnd(const Bond_t<T> &bond, Date settlement = Date()) {
    if (settlement == Date())
        settlement = bond.settlementDate();

    QL_REQUIRE(isTradable<T>(bond, settlement),
               "non tradable at " << settlement << " (maturity being "
                                  << bond.maturityDate() << ")");

    return CashFlows::referencePeriodEnd<T>(bond.cashflows(), false,
                                            settlement);
}

template <class T>
Time accrualPeriod(const Bond_t<T> &bond, Date settlement = Date()) {
    if (settlement == Date())
        settlement = bond.settlementDate();

    QL_REQUIRE(isTradable<T>(bond, settlement),
               "non tradable at " << settlement << " (maturity being "
                                  << bond.maturityDate() << ")");

    return CashFlows::accrualPeriod<T>(bond.cashflows(), false, settlement);
}

template <class T>
BigInteger accrualDays(const Bond_t<T> &bond, Date settlement = Date()) {
    if (settlement == Date())
        settlement = bond.settlementDate();

    QL_REQUIRE(isTradable<T>(bond, settlement),
               "non tradable at " << settlement << " (maturity being "
                                  << bond.maturityDate() << ")");

    return CashFlows::accrualDays<T>(bond.cashflows(), false, settlement);
}

template <class T>
Time accruedPeriod(const Bond_t<T> &bond, Date settlement = Date()) {
    if (settlement == Date())
        settlement = bond.settlementDate();

    QL_REQUIRE(isTradable<T>(bond, settlement),
               "non tradable at " << settlement << " (maturity being "
                                  << bond.maturityDate() << ")");

    return CashFlows::accruedPeriod<T>(bond.cashflows(), false, settlement);
}

template <class T>
BigInteger accruedDays(const Bond_t<T> &bond, Date settlement = Date()) {
    if (settlement == Date())
        settlement = bond.settlementDate();

    QL_REQUIRE(isTradable<T>(bond, settlement),
               "non tradable at " << settlement << " (maturity being "
                                  << bond.maturityDate() << ")");

    return CashFlows::accruedDays<T>(bond.cashflows(), false, settlement);
}

template <class T>
T accruedAmount(const Bond_t<T> &bond, Date settlement = Date()) {
    return bond.accruedAmount(settlement);
}

template <class T>
T cleanPrice(const Bond_t<T> &bond,
             const YieldTermStructure_t<T> &discountCurve,
             Date settlement = Date()) {
    if (settlement == Date())
        settlement = bond.settlementDate();

    QL_REQUIRE(isTradable<T>(bond, settlement),
               "non tradable at " << settlement
                                  << " settlement date (maturity being "
                                  << bond.maturityDate() << ")");

    T dirtyPrice =
        CashFlows::npv<T>(bond.cashflows(), discountCurve, false, settlement) *
        100.0 / bond.notional(settlement);
    return dirtyPrice - bond.accruedAmount(settlement);
}

template <class T>
T bps(const Bond_t<T> &bond, const YieldTermStructure_t<T> &discountCurve,
      Date settlement = Date()) {
    if (settlement == Date())
        settlement = bond.settlementDate();

    QL_REQUIRE(isTradable<T>(bond, settlement),
               "non tradable at " << settlement << " (maturity being "
                                  << bond.maturityDate() << ")");

    return CashFlows::bps<T>(bond.cashflows(), discountCurve, false,
                             settlement) *
           100.0 / bond.notional(settlement);
}

template <class T>
T atmRate(const Bond_t<T> &bond, const YieldTermStructure_t<T> &discountCurve,
          Date settlement = Date(), T cleanPrice = Null<Real>()) {
    if (settlement == Date())
        settlement = bond.settlementDate();

    QL_REQUIRE(isTradable<T>(bond, settlement),
               "non tradable at " << settlement << " (maturity being "
                                  << bond.maturityDate() << ")");

    T result0 = Null<Real>();
    T result1 = cleanPrice + bond.accruedAmount(settlement);
    T pred = Null<Real>();
    T dirtyPrice = CondExpEq(cleanPrice, pred, result0, result1);
    T currentNotional = bond.notional(settlement);
    T npv = dirtyPrice / 100.0 * currentNotional;

    return CashFlows::atmRate<T>(bond.cashflows(), discountCurve, false,
                                 settlement, settlement, npv);
}

template <class T>
T dirtyPrice(const Bond_t<T> &bond, const InterestRate_t<T> &yield,
             Date settlement = Date()) {
    if (settlement == Date())
        settlement = bond.settlementDate();

    QL_REQUIRE(isTradable<T>(bond, settlement),
               "non tradable at " << settlement << " (maturity being "
                                  << bond.maturityDate() << ")");

    T dirtyPrice =
        CashFlows::npv<T>(bond.cashflows(), yield, false, settlement) * 100.0 /
        bond.notional(settlement);
    return dirtyPrice;
}

template <class T>
T dirtyPrice(const Bond_t<T> &bond, T yield, const DayCounter &dayCounter,
             Compounding compounding, Frequency frequency,
             Date settlement = Date()) {
    InterestRate_t<T> y(yield, dayCounter, compounding, frequency);
    return dirtyPrice(bond, y, settlement);
}

template <class T>
T cleanPrice(const Bond_t<T> &bond, const InterestRate_t<T> &yield,
             Date settlement = Date()) {
    return dirtyPrice(bond, yield, settlement) - bond.accruedAmount(settlement);
}

template <class T>
T cleanPrice(const Bond_t<T> &bond, T yield, const DayCounter &dayCounter,
             Compounding compounding, Frequency frequency, Date settlement) {
    InterestRate_t<T> y(yield, dayCounter, compounding, frequency);
    return cleanPrice(bond, y, settlement);
}

template <class T>
T bps(const Bond_t<T> &bond, const InterestRate_t<T> &yield,
      Date settlement = Date()) {
    if (settlement == Date())
        settlement = bond.settlementDate();

    QL_REQUIRE(isTradable<T>(bond, settlement),
               "non tradable at " << settlement << " (maturity being "
                                  << bond.maturityDate() << ")");

    return CashFlows::bps<T>(bond.cashflows(), yield, false, settlement) *
           100.0 / bond.notional(settlement);
}

template <class T>
T bps(const Bond_t<T> &bond, T yield, const DayCounter &dayCounter,
      Compounding compounding, Frequency frequency, Date settlement = Date()) {
    InterestRate_t<T> y(yield, dayCounter, compounding, frequency);
    return bps(bond, y, settlement);
}

template <class T>
T yield(const Bond_t<T> &bond, T cleanPrice, const DayCounter &dayCounter,
        Compounding compounding, Frequency frequency, Date settlement,
        Real accuracy, Size maxIterations, Rate guess) {
    if (settlement == Date())
        settlement = bond.settlementDate();

    QL_REQUIRE(isTradable<T>(bond, settlement),
               "non tradable at " << settlement << " (maturity being "
                                  << bond.maturityDate() << ")");

    T dirtyPrice = cleanPrice + bond.accruedAmount(settlement);
    dirtyPrice = dirtyPrice / (100.0 / bond.notional(settlement));

    return CashFlows::yield<T>(bond.cashflows(), dirtyPrice, dayCounter,
                               compounding, frequency, false, settlement,
                               settlement, accuracy, maxIterations, guess);
}

template <class T>
T duration(const Bond_t<T> &bond, const InterestRate_t<T> &yield,
           Duration::Type type = Duration::Modified, Date settlement = Date()) {
    if (settlement == Date())
        settlement = bond.settlementDate();

    QL_REQUIRE(isTradable<T>(bond, settlement),
               "non tradable at " << settlement << " (maturity being "
                                  << bond.maturityDate() << ")");

    return CashFlows::duration<T>(bond.cashflows(), yield, type, false,
                                  settlement);
}

template <class T>
T duration(const Bond_t<T> &bond, T yield, const DayCounter &dayCounter,
           Compounding compounding, Frequency frequency,
           Duration::Type type = Duration::Modified, Date settlement = Date()) {
    InterestRate_t<T> y(yield, dayCounter, compounding, frequency);
    return duration(bond, y, type, settlement);
}

template <class T>
T convexity(const Bond_t<T> &bond, const InterestRate_t<T> &yield,
            Date settlement = Date()) {
    if (settlement == Date())
        settlement = bond.settlementDate();

    QL_REQUIRE(isTradable<T>(bond, settlement),
               "non tradable at " << settlement << " (maturity being "
                                  << bond.maturityDate() << ")");

    return CashFlows::convexity<T>(bond.cashflows(), yield, false, settlement);
}

template <class T>
T convexity(const Bond_t<T> &bond, T yield, const DayCounter &dayCounter,
            Compounding compounding, Frequency frequency,
            Date settlement = Date()) {
    InterestRate_t<T> y(yield, dayCounter, compounding, frequency);
    return convexity(bond, y, settlement);
}

template <class T>
T basisPointValue(const Bond_t<T> &bond, const InterestRate_t<T> &yield,
                  Date settlement = Date()) {
    if (settlement == Date())
        settlement = bond.settlementDate();

    QL_REQUIRE(isTradable<T>(bond, settlement),
               "non tradable at " << settlement << " (maturity being "
                                  << bond.maturityDate() << ")");

    return CashFlows::basisPointValue<T>(bond.cashflows(), yield, false,
                                         settlement);
}

template <class T>
T basisPointValue(const Bond_t<T> &bond, T yield, const DayCounter &dayCounter,
                  Compounding compounding, Frequency frequency,
                  Date settlement = Date()) {
    InterestRate_t<T> y(yield, dayCounter, compounding, frequency);
    return CashFlows::basisPointValue<T>(bond.cashflows(), y, false,
                                         settlement);
}

template <class T>
T yieldValueBasisPoint(const Bond_t<T> &bond, const InterestRate_t<T> &yield,
                       Date settlement = Date()) {
    if (settlement == Date())
        settlement = bond.settlementDate();

    QL_REQUIRE(isTradable<T>(bond, settlement),
               "non tradable at " << settlement << " (maturity being "
                                  << bond.maturityDate() << ")");

    return CashFlows::yieldValueBasisPoint<T>(bond.cashflows(), yield, false,
                                              settlement);
}

template <class T>
T yieldValueBasisPoint(const Bond_t<T> &bond, T yield,
                       const DayCounter &dayCounter, Compounding compounding,
                       Frequency frequency, Date settlement = Date()) {
    InterestRate_t<T> y(yield, dayCounter, compounding, frequency);
    return CashFlows::yieldValueBasisPoint<T>(bond.cashflows(), y, false,
                                              settlement);
}

template <class T>
T cleanPrice(const Bond_t<T> &bond,
             const boost::shared_ptr<YieldTermStructure_t<T> > &d, T zSpread,
             const DayCounter &dc, Compounding comp, Frequency freq,
             Date settlement = Date()) {
    if (settlement == Date())
        settlement = bond.settlementDate();

    QL_REQUIRE(isTradable<T>(bond, settlement),
               "non tradable at " << settlement << " (maturity being "
                                  << bond.maturityDate() << ")");

    T dirtyPrice = CashFlows::npv<T>(bond.cashflows(), d, zSpread, dc, comp,
                                     freq, false, settlement) *
                   100.0 / bond.notional(settlement);
    return dirtyPrice - bond.accruedAmount(settlement);
}

template <class T>
T yieldTermStructurePV01(const Bond_t<T> &bond,
                         const boost::shared_ptr<YieldTermStructure_t<T> > &d,
                         const DayCounter &dc, Compounding comp, Frequency freq,
                         Date settlement = Date()) {
    if (settlement == Date())
        settlement = bond.settlementDate();

    QL_REQUIRE(isTradable<T>(bond, settlement),
               "non tradable at " << settlement << " (maturity being "
                                  << bond.maturityDate() << ")");

    const T basisPoint = 0.0001;

    T zSpread = basisPoint;

    const T dirtyPriceUp = CashFlows::npv<T>(bond.cashflows(), d, zSpread, dc,
                                             comp, freq, false, settlement) *
                           100.0 / bond.notional(settlement);

    zSpread = -basisPoint;

    const T dirtyPriceDown = CashFlows::npv<T>(bond.cashflows(), d, zSpread, dc,
                                               comp, freq, false, settlement) *
                             100.0 / bond.notional(settlement);

    return (dirtyPriceDown - dirtyPriceUp) / 2.0;
}

template <class T>
T zSpread(const Bond_t<T> &bond, T cleanPrice,
          const shared_ptr<YieldTermStructure_t<T> > &d,
          const DayCounter &dayCounter, Compounding compounding,
          Frequency frequency, Date settlement, Real accuracy,
          Size maxIterations, Rate guess) {
    if (settlement == Date())
        settlement = bond.settlementDate();

    QL_REQUIRE(isTradable<T>(bond, settlement),
               "non tradable at " << settlement << " (maturity being "
                                  << bond.maturityDate() << ")");

    T dirtyPrice = cleanPrice + bond.accruedAmount(settlement);
    dirtyPrice = dirtyPrice / (100.0 / bond.notional(settlement));

    return CashFlows::zSpread<T>(bond.cashflows(), d, dirtyPrice, dayCounter,
                                 compounding, frequency, false, settlement,
                                 settlement, accuracy, maxIterations, guess);
}
}
}

#endif

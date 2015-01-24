/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2006, 2007, 2010 Ferdinando Ametrano
 Copyright (C) 2006 Katiuscia Manzoni
 Copyright (C) 2006 StatPro Italia srl
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

/*! \file makevanillaswap.hpp
    \brief Helper class to instantiate standard market swaps.
*/

#ifndef quantlib_makevanillaswap_hpp
#define quantlib_makevanillaswap_hpp

#include <ql/instruments/vanillaswap.hpp>
#include <ql/time/dategenerationrule.hpp>
#include <ql/termstructures/yieldtermstructure.hpp>
#include <ql/pricingengines/swap/discountingswapengine.hpp>
#include <ql/time/daycounters/thirty360.hpp>
#include <ql/indexes/iborindex.hpp>
#include <ql/time/schedule.hpp>
#include <ql/currencies/europe.hpp>

namespace QuantLib {

//! helper class
/*! This class provides a more comfortable way
    to instantiate standard market swap.
*/
template <class T> class MakeVanillaSwap_t {
  public:
    MakeVanillaSwap_t(const Period &swapTenor,
                      const boost::shared_ptr<IborIndex_t<T> > &iborIndex,
                      T fixedRate = Null<Rate>(),
                      const Period &forwardStart = 0 * Days);

    operator VanillaSwap_t<T>() const;
    operator boost::shared_ptr<VanillaSwap_t<T> >() const;

    MakeVanillaSwap_t<T> &receiveFixed(bool flag = true);
    MakeVanillaSwap_t<T> &withType(typename VanillaSwap_t<T>::Type type);
    MakeVanillaSwap_t<T> &withNominal(T n);
    MakeVanillaSwap_t<T> &withEffectiveDate(const Date &);
    MakeVanillaSwap_t<T> &withTerminationDate(const Date &);
    MakeVanillaSwap_t<T> &withRule(DateGeneration::Rule r);

    MakeVanillaSwap_t<T> &withFixedLegTenor(const Period &t);
    MakeVanillaSwap_t<T> &withFixedLegCalendar(const Calendar &cal);
    MakeVanillaSwap_t<T> &withFixedLegConvention(BusinessDayConvention bdc);
    MakeVanillaSwap_t<T> &
    withFixedLegTerminationDateConvention(BusinessDayConvention bdc);
    MakeVanillaSwap_t<T> &withFixedLegRule(DateGeneration::Rule r);
    MakeVanillaSwap_t<T> &withFixedLegEndOfMonth(bool flag = true);
    MakeVanillaSwap_t<T> &withFixedLegFirstDate(const Date &d);
    MakeVanillaSwap_t<T> &withFixedLegNextToLastDate(const Date &d);
    MakeVanillaSwap_t<T> &withFixedLegDayCount(const DayCounter &dc);

    MakeVanillaSwap_t<T> &withFloatingLegTenor(const Period &t);
    MakeVanillaSwap_t<T> &withFloatingLegCalendar(const Calendar &cal);
    MakeVanillaSwap_t<T> &withFloatingLegConvention(BusinessDayConvention bdc);
    MakeVanillaSwap_t<T> &
    withFloatingLegTerminationDateConvention(BusinessDayConvention bdc);
    MakeVanillaSwap_t<T> &withFloatingLegRule(DateGeneration::Rule r);
    MakeVanillaSwap_t<T> &withFloatingLegEndOfMonth(bool flag = true);
    MakeVanillaSwap_t<T> &withFloatingLegFirstDate(const Date &d);
    MakeVanillaSwap_t<T> &withFloatingLegNextToLastDate(const Date &d);
    MakeVanillaSwap_t<T> &withFloatingLegDayCount(const DayCounter &dc);
    MakeVanillaSwap_t<T> &withFloatingLegSpread(T sp);

    MakeVanillaSwap_t<T> &withDiscountingTermStructure(
        const Handle<YieldTermStructure_t<T> > &discountCurve);
    MakeVanillaSwap_t<T> &
    withPricingEngine(const boost::shared_ptr<PricingEngine> &engine);

  private:
    Period swapTenor_;
    boost::shared_ptr<IborIndex_t<T> > iborIndex_;
    T fixedRate_;
    Period forwardStart_;

    Date effectiveDate_, terminationDate_;
    Calendar fixedCalendar_, floatCalendar_;

    typename VanillaSwap_t<T>::Type type_;
    T nominal_;
    Period fixedTenor_, floatTenor_;
    BusinessDayConvention fixedConvention_, fixedTerminationDateConvention_;
    BusinessDayConvention floatConvention_, floatTerminationDateConvention_;
    DateGeneration::Rule fixedRule_, floatRule_;
    bool fixedEndOfMonth_, floatEndOfMonth_;
    Date fixedFirstDate_, fixedNextToLastDate_;
    Date floatFirstDate_, floatNextToLastDate_;
    T floatSpread_;
    DayCounter fixedDayCount_, floatDayCount_;

    boost::shared_ptr<PricingEngine> engine_;
};

typedef MakeVanillaSwap_t<Real> MakeVanillaSwap;

// implementation

template <class T>
MakeVanillaSwap_t<T>::MakeVanillaSwap_t(
    const Period &swapTenor, const boost::shared_ptr<IborIndex_t<T> > &index,
    T fixedRate, const Period &forwardStart)
    : swapTenor_(swapTenor), iborIndex_(index), fixedRate_(fixedRate),
      forwardStart_(forwardStart), effectiveDate_(Date()),
      fixedCalendar_(index->fixingCalendar()),
      floatCalendar_(index->fixingCalendar()), type_(VanillaSwap_t<T>::Payer),
      nominal_(1.0), floatTenor_(index->tenor()),
      fixedConvention_(ModifiedFollowing),
      fixedTerminationDateConvention_(ModifiedFollowing),
      floatConvention_(index->businessDayConvention()),
      floatTerminationDateConvention_(index->businessDayConvention()),
      fixedRule_(DateGeneration::Backward),
      floatRule_(DateGeneration::Backward), fixedEndOfMonth_(false),
      floatEndOfMonth_(false), fixedFirstDate_(Date()),
      fixedNextToLastDate_(Date()), floatFirstDate_(Date()),
      floatNextToLastDate_(Date()), floatSpread_(0.0),
      floatDayCount_(index->dayCounter()),
      engine_(new DiscountingSwapEngine_t<T>(
          iborIndex_->forwardingTermStructure(), false)) {}

template <class T> MakeVanillaSwap_t<T>::operator VanillaSwap_t<T>() const {
    boost::shared_ptr<VanillaSwap> swap = *this;
    return *swap;
}

template <class T>
MakeVanillaSwap_t<T>::operator boost::shared_ptr<VanillaSwap_t<T> >() const {

    Date startDate;
    if (effectiveDate_ != Date())
        startDate = effectiveDate_;
    else {
        Date refDate = Settings::instance().evaluationDate();
        // if the evaluation date is not a business day
        // then move to the next business day
        refDate = floatCalendar_.adjust(refDate);
        Natural fixingDays = iborIndex_->fixingDays();
        Date spotDate = floatCalendar_.advance(refDate, fixingDays * Days);
        startDate = spotDate + forwardStart_;
        if (forwardStart_.length() < 0)
            startDate = floatCalendar_.adjust(startDate, Preceding);
        else
            startDate = floatCalendar_.adjust(startDate, Following);
    }

    Date endDate;
    if (terminationDate_ != Date())
        endDate = terminationDate_;
    else
        endDate = startDate + swapTenor_;

    Period fixedTenor;
    if (fixedTenor_ != Period())
        fixedTenor = fixedTenor_;
    else {
        const Currency &curr = iborIndex_->currency();
        if (curr == EURCurrency())
            fixedTenor = Period(1, Years);
        else if (curr == GBPCurrency())
            fixedTenor = Period(6, Months);
        else
            fixedTenor = Period(1, Years);
    }

    Schedule fixedSchedule(startDate, endDate, fixedTenor, fixedCalendar_,
                           fixedConvention_, fixedTerminationDateConvention_,
                           fixedRule_, fixedEndOfMonth_, fixedFirstDate_,
                           fixedNextToLastDate_);

    Schedule floatSchedule(startDate, endDate, floatTenor_, floatCalendar_,
                           floatConvention_, floatTerminationDateConvention_,
                           floatRule_, floatEndOfMonth_, floatFirstDate_,
                           floatNextToLastDate_);

    DayCounter fixedDayCount;
    if (fixedDayCount_ != DayCounter())
        fixedDayCount = fixedDayCount_;
    else {
        const Currency &curr = iborIndex_->currency();
        if (curr == EURCurrency())
            fixedDayCount = Thirty360(Thirty360::BondBasis);
        else if (curr == GBPCurrency())
            fixedDayCount = Actual365Fixed();
        else
            fixedDayCount = Thirty360(Thirty360::BondBasis);
    }

    T usedFixedRate = fixedRate_;
    if (fixedRate_ == Null<Rate>()) {
        QL_REQUIRE(!iborIndex_->forwardingTermStructure().empty(),
                   "null term structure set to this instance of "
                       << iborIndex_->name());
        VanillaSwap_t<T> temp(type_, nominal_, fixedSchedule,
                              0.0, // fixed rate
                              fixedDayCount, floatSchedule, iborIndex_,
                              floatSpread_, floatDayCount_);
        temp.setPricingEngine(engine_);
        usedFixedRate = temp.fairRate();
    }

    boost::shared_ptr<VanillaSwap_t<T> > swap(new VanillaSwap_t<T>(
        type_, nominal_, fixedSchedule, usedFixedRate, fixedDayCount,
        floatSchedule, iborIndex_, floatSpread_, floatDayCount_));
    swap->setPricingEngine(engine_);
    return swap;
}

template <class T>
MakeVanillaSwap_t<T> &MakeVanillaSwap_t<T>::receiveFixed(bool flag) {
    type_ = flag ? VanillaSwap_t<T>::Receiver : VanillaSwap_t<T>::Payer;
    return *this;
}

template <class T>
MakeVanillaSwap_t<T> &
MakeVanillaSwap_t<T>::withType(typename VanillaSwap_t<T>::Type type) {
    type_ = type;
    return *this;
}

template <class T>
MakeVanillaSwap_t<T> &MakeVanillaSwap_t<T>::withNominal(T n) {
    nominal_ = n;
    return *this;
}

template <class T>
MakeVanillaSwap_t<T> &
MakeVanillaSwap_t<T>::withEffectiveDate(const Date &effectiveDate) {
    effectiveDate_ = effectiveDate;
    return *this;
}

template <class T>
MakeVanillaSwap_t<T> &
MakeVanillaSwap_t<T>::withTerminationDate(const Date &terminationDate) {
    terminationDate_ = terminationDate;
    swapTenor_ = Period();
    return *this;
}

template <class T>
MakeVanillaSwap_t<T> &MakeVanillaSwap_t<T>::withRule(DateGeneration::Rule r) {
    fixedRule_ = r;
    floatRule_ = r;
    return *this;
}

template <class T>
MakeVanillaSwap_t<T> &MakeVanillaSwap_t<T>::withDiscountingTermStructure(
    const Handle<YieldTermStructure_t<T> > &disc) {
    bool includeSettlementDateFlows = false;
    engine_ = boost::shared_ptr<PricingEngine>(
        new DiscountingSwapEngine_t<T>(disc, includeSettlementDateFlows));
    return *this;
}

template <class T>
MakeVanillaSwap_t<T> &MakeVanillaSwap_t<T>::withPricingEngine(
    const boost::shared_ptr<PricingEngine> &engine) {
    engine_ = engine;
    return *this;
}

template <class T>
MakeVanillaSwap_t<T> &MakeVanillaSwap_t<T>::withFixedLegTenor(const Period &t) {
    fixedTenor_ = t;
    return *this;
}

template <class T>
MakeVanillaSwap_t<T> &
MakeVanillaSwap_t<T>::withFixedLegCalendar(const Calendar &cal) {
    fixedCalendar_ = cal;
    return *this;
}

template <class T>
MakeVanillaSwap_t<T> &
MakeVanillaSwap_t<T>::withFixedLegConvention(BusinessDayConvention bdc) {
    fixedConvention_ = bdc;
    return *this;
}

template <class T>
MakeVanillaSwap_t<T> &
MakeVanillaSwap_t<T>::withFixedLegTerminationDateConvention(
    BusinessDayConvention bdc) {
    fixedTerminationDateConvention_ = bdc;
    return *this;
}

template <class T>
MakeVanillaSwap_t<T> &
MakeVanillaSwap_t<T>::withFixedLegRule(DateGeneration::Rule r) {
    fixedRule_ = r;
    return *this;
}

template <class T>
MakeVanillaSwap_t<T> &MakeVanillaSwap_t<T>::withFixedLegEndOfMonth(bool flag) {
    fixedEndOfMonth_ = flag;
    return *this;
}

template <class T>
MakeVanillaSwap_t<T> &
MakeVanillaSwap_t<T>::withFixedLegFirstDate(const Date &d) {
    fixedFirstDate_ = d;
    return *this;
}

template <class T>
MakeVanillaSwap_t<T> &
MakeVanillaSwap_t<T>::withFixedLegNextToLastDate(const Date &d) {
    fixedNextToLastDate_ = d;
    return *this;
}

template <class T>
MakeVanillaSwap_t<T> &
MakeVanillaSwap_t<T>::withFixedLegDayCount(const DayCounter &dc) {
    fixedDayCount_ = dc;
    return *this;
}

template <class T>
MakeVanillaSwap_t<T> &
MakeVanillaSwap_t<T>::withFloatingLegTenor(const Period &t) {
    floatTenor_ = t;
    return *this;
}

template <class T>
MakeVanillaSwap_t<T> &
MakeVanillaSwap_t<T>::withFloatingLegCalendar(const Calendar &cal) {
    floatCalendar_ = cal;
    return *this;
}

template <class T>
MakeVanillaSwap_t<T> &
MakeVanillaSwap_t<T>::withFloatingLegConvention(BusinessDayConvention bdc) {
    floatConvention_ = bdc;
    return *this;
}

template <class T>
MakeVanillaSwap_t<T> &
MakeVanillaSwap_t<T>::withFloatingLegTerminationDateConvention(
    BusinessDayConvention bdc) {
    floatTerminationDateConvention_ = bdc;
    return *this;
}

template <class T>
MakeVanillaSwap_t<T> &
MakeVanillaSwap_t<T>::withFloatingLegRule(DateGeneration::Rule r) {
    floatRule_ = r;
    return *this;
}

template <class T>
MakeVanillaSwap_t<T> &
MakeVanillaSwap_t<T>::withFloatingLegEndOfMonth(bool flag) {
    floatEndOfMonth_ = flag;
    return *this;
}

template <class T>
MakeVanillaSwap_t<T> &
MakeVanillaSwap_t<T>::withFloatingLegFirstDate(const Date &d) {
    floatFirstDate_ = d;
    return *this;
}

template <class T>
MakeVanillaSwap_t<T> &
MakeVanillaSwap_t<T>::withFloatingLegNextToLastDate(const Date &d) {
    floatNextToLastDate_ = d;
    return *this;
}

template <class T>
MakeVanillaSwap_t<T> &
MakeVanillaSwap_t<T>::withFloatingLegDayCount(const DayCounter &dc) {
    floatDayCount_ = dc;
    return *this;
}

template <class T>
MakeVanillaSwap_t<T> &MakeVanillaSwap_t<T>::withFloatingLegSpread(T sp) {
    floatSpread_ = sp;
    return *this;
}
}

#endif

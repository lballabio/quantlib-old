/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl
 Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008 StatPro Italia srl
 Copyright (C) 2007, 2008, 2009 Ferdinando Ametrano
 Copyright (C) 2007, 2009 Roland Lichters
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

/*! \file ratehelpers.hpp
    \brief deposit, FRA, futures, and swap rate helpers
*/

#ifndef quantlib_ratehelpers_hpp
#define quantlib_ratehelpers_hpp

#include <ql/termstructures/bootstraphelper.hpp>
#include <ql/instruments/vanillaswap.hpp>
#include <ql/instruments/bmaswap.hpp>
#include <ql/time/calendar.hpp>
#include <ql/time/daycounter.hpp>

#include <ql/termstructures/yield/ratehelpers.hpp>
#include <ql/time/imm.hpp>
#include <ql/instruments/makevanillaswap.hpp>
#include <ql/pricingengines/swap/discountingswapengine.hpp>
#include <ql/quote.hpp>
#include <ql/currency.hpp>
#include <ql/indexes/swapindex.hpp>
#ifdef QL_USE_INDEXED_COUPON
#include <ql/cashflows/floatingratecoupon.hpp>
#endif

namespace QuantLib {

using boost::shared_ptr;

class SwapIndex;

template <class T> struct RateHelper_t {
    typedef BootstrapHelper<YieldTermStructure_t<T> > Type;
};

template <class T> struct RelativeDateRateHelper_t {
    typedef RelativeDateBootstrapHelper<YieldTermStructure_t<T> > Type;
};

typedef RateHelper_t<Real>::Type RateHelper;
typedef RelativeDateRateHelper_t<Real>::Type RelativeDateRateHelper;

//! Rate helper for bootstrapping over IborIndex futures prices
template <class T = Real>
class FuturesRateHelper_t : public RateHelper_t<T>::Type {
  public:
    FuturesRateHelper_t(
        const Handle<Quote_t<T> > &price, const Date &immDate,
        Natural lengthInMonths, const Calendar &calendar,
        BusinessDayConvention convention, bool endOfMonth,
        const DayCounter &dayCounter,
        const Handle<Quote_t<T> > &convexityAdjustment = Handle<Quote_t<T> >());
    FuturesRateHelper_t(T price, const Date &immDate, Natural lengthInMonths,
                        const Calendar &calendar,
                        BusinessDayConvention convention, bool endOfMonth,
                        const DayCounter &dayCounter,
                        T convexityAdjustment = 0.0);
    FuturesRateHelper_t(
        const Handle<Quote_t<T> > &price, const Date &immStartDate,
        const Date &endDate, const DayCounter &dayCounter,
        const Handle<Quote_t<T> > &convexityAdjustment = Handle<Quote_t<T> >());
    FuturesRateHelper_t(T price, const Date &immStartDate, const Date &endDate,
                        const DayCounter &dayCounter,
                        T convexityAdjustment = 0.0);
    FuturesRateHelper_t(
        const Handle<Quote_t<T> > &price, const Date &immDate,
        const boost::shared_ptr<IborIndex> &iborIndex,
        const Handle<Quote_t<T> > &convexityAdjustment = Handle<Quote_t<T> >());
    FuturesRateHelper_t(T price, const Date &immDate,
                        const boost::shared_ptr<IborIndex> &iborIndex,
                        T convexityAdjustment = 0.0);
    //! \name RateHelper interface
    //@{
    T impliedQuote() const;
    //@}
    //! \name FuturesRateHelper inspectors
    //@{
    T convexityAdjustment() const;
    //@}
    //! \name Visitability
    //@{
    void accept(AcyclicVisitor &);
    //@}
  private:
    Time yearFraction_;
    Handle<Quote_t<T> > convAdj_;
};

typedef FuturesRateHelper_t<Real> FuturesRateHelper;

//! Rate helper for bootstrapping over deposit rates
template <class T = Real>
class DepositRateHelper_t : public RelativeDateRateHelper_t<T>::Type {
  public:
    DepositRateHelper_t(const Handle<Quote_t<T> > &rate, const Period &tenor,
                        Natural fixingDays, const Calendar &calendar,
                        BusinessDayConvention convention, bool endOfMonth,
                        const DayCounter &dayCounter);
    DepositRateHelper_t(T rate, const Period &tenor, Natural fixingDays,
                        const Calendar &calendar,
                        BusinessDayConvention convention, bool endOfMonth,
                        const DayCounter &dayCounter);
    DepositRateHelper_t(const Handle<Quote_t<T> > &rate,
                        const boost::shared_ptr<IborIndex> &iborIndex);
    DepositRateHelper_t(T rate, const boost::shared_ptr<IborIndex> &iborIndex);
    //! \name RateHelper interface
    //@{
    T impliedQuote() const;
    void setTermStructure(YieldTermStructure_t<T> *);
    //@}
    //! \name Visitability
    //@{
    void accept(AcyclicVisitor &);
    //@}
  private:
    void initializeDates();
    Date fixingDate_;
    boost::shared_ptr<IborIndex> iborIndex_;
    RelinkableHandle<YieldTermStructure_t<T> > termStructureHandle_;
};

typedef DepositRateHelper_t<Real> DepositRateHelper;

//! Rate helper for bootstrapping over %FRA rates
template <class T>
class FraRateHelper_t : public RelativeDateRateHelper_t<T>::Type {
  public:
    FraRateHelper_t(const Handle<Quote_t<T> > &rate, Natural monthsToStart,
                    Natural monthsToEnd, Natural fixingDays,
                    const Calendar &calendar, BusinessDayConvention convention,
                    bool endOfMonth, const DayCounter &dayCounter);
    FraRateHelper_t(T rate, Natural monthsToStart, Natural monthsToEnd,
                    Natural fixingDays, const Calendar &calendar,
                    BusinessDayConvention convention, bool endOfMonth,
                    const DayCounter &dayCounter);
    FraRateHelper_t(const Handle<Quote_t<T> > &rate, Natural monthsToStart,
                    const boost::shared_ptr<IborIndex> &iborIndex);
    FraRateHelper_t(T rate, Natural monthsToStart,
                    const boost::shared_ptr<IborIndex> &iborIndex);
    FraRateHelper_t(const Handle<Quote_t<T> > &rate, Period periodToStart,
                    Natural lengthInMonths, Natural fixingDays,
                    const Calendar &calendar, BusinessDayConvention convention,
                    bool endOfMonth, const DayCounter &dayCounter);
    FraRateHelper_t(T rate, Period periodToStart, Natural lengthInMonths,
                    Natural fixingDays, const Calendar &calendar,
                    BusinessDayConvention convention, bool endOfMonth,
                    const DayCounter &dayCounter);
    FraRateHelper_t(const Handle<Quote_t<T> > &rate, Period periodToStart,
                    const boost::shared_ptr<IborIndex> &iborIndex);
    FraRateHelper_t(T rate, Period periodToStart,
                    const boost::shared_ptr<IborIndex> &iborIndex);
    //! \name RateHelper interface
    //@{
    T impliedQuote() const;
    void setTermStructure(YieldTermStructure_t<T> *);
    //@}
    //! \name Visitability
    //@{
    void accept(AcyclicVisitor &);
    //@}
  private:
    void initializeDates();
    Date fixingDate_;
    Period periodToStart_;
    boost::shared_ptr<IborIndex> iborIndex_;
    RelinkableHandle<YieldTermStructure_t<T> > termStructureHandle_;
};

typedef FraRateHelper_t<Real> FraRateHelper;

//! Rate helper for bootstrapping over swap rates
/*! \todo use input SwapIndex to create the swap */
template <class T = Real>
class SwapRateHelper_t : public RelativeDateRateHelper_t<T>::Type {
  public:
    SwapRateHelper_t(const Handle<Quote_t<T> > &rate,
                     const boost::shared_ptr<SwapIndex> &swapIndex,
                     const Handle<Quote_t<T> > &spread = Handle<Quote_t<T> >(),
                     const Period &fwdStart = 0 * Days,
                     // exogenous discounting curve
                     const Handle<YieldTermStructure_t<T> > &discountingCurve =
                         Handle<YieldTermStructure_t<T> >());
    SwapRateHelper_t(const Handle<Quote_t<T> > &rate, const Period &tenor,
                     const Calendar &calendar,
                     // fixed leg
                     Frequency fixedFrequency,
                     BusinessDayConvention fixedConvention,
                     const DayCounter &fixedDayCount,
                     // floating leg
                     const boost::shared_ptr<IborIndex> &iborIndex,
                     const Handle<Quote_t<T> > &spread = Handle<Quote_t<T> >(),
                     const Period &fwdStart = 0 * Days,
                     // exogenous discounting curve
                     const Handle<YieldTermStructure_t<T> > &discountingCurve =
                         Handle<YieldTermStructure_t<T> >());
    SwapRateHelper_t(T rate, const Period &tenor, const Calendar &calendar,
                     // fixed leg
                     Frequency fixedFrequency,
                     BusinessDayConvention fixedConvention,
                     const DayCounter &fixedDayCount,
                     // floating leg
                     const boost::shared_ptr<IborIndex> &iborIndex,
                     const Handle<Quote_t<T> > &spread = Handle<Quote_t<T> >(),
                     const Period &fwdStart = 0 * Days,
                     // exogenous discounting curve
                     const Handle<YieldTermStructure_t<T> > &discountingCurve =
                         Handle<YieldTermStructure_t<T> >());
    SwapRateHelper_t(T rate, const boost::shared_ptr<SwapIndex> &swapIndex,
                     const Handle<Quote_t<T> > &spread = Handle<Quote_t<T> >(),
                     const Period &fwdStart = 0 * Days,
                     // exogenous discounting curve
                     const Handle<YieldTermStructure_t<T> > &discountingCurve =
                         Handle<YieldTermStructure_t<T> >());
    //! \name RateHelper interface
    //@{
    T impliedQuote() const;
    void setTermStructure(YieldTermStructure_t<T> *);
    //@}
    //! \name SwapRateHelper inspectors
    //@{
    T spread() const;
    boost::shared_ptr<VanillaSwap> swap() const;
    const Period &forwardStart() const;
    //@}
    //! \name Visitability
    //@{
    void accept(AcyclicVisitor &);
    //@}
  protected:
    void initializeDates();
    Period tenor_;
    Calendar calendar_;
    BusinessDayConvention fixedConvention_;
    Frequency fixedFrequency_;
    DayCounter fixedDayCount_;
    boost::shared_ptr<IborIndex> iborIndex_;
    boost::shared_ptr<VanillaSwap> swap_;
    RelinkableHandle<YieldTermStructure_t<T> > termStructureHandle_;
    Handle<Quote_t<T> > spread_;
    Period fwdStart_;
    Handle<YieldTermStructure_t<T> > discountHandle_;
    RelinkableHandle<YieldTermStructure_t<T> > discountRelinkableHandle_;
};

typedef SwapRateHelper_t<Real> SwapRateHelper;

//! Rate helper for bootstrapping over BMA swap rates
template <class T = Real>
class BMASwapRateHelper_t : public RelativeDateRateHelper {
  public:
    BMASwapRateHelper_t(const Handle<Quote_t<T> > &liborFraction,
                        const Period &tenor, // swap maturity
                        Natural settlementDays, const Calendar &calendar,
                        // bma leg
                        const Period &bmaPeriod,
                        BusinessDayConvention bmaConvention,
                        const DayCounter &bmaDayCount,
                        const boost::shared_ptr<BMAIndex> &bmaIndex,
                        // ibor leg
                        const boost::shared_ptr<IborIndex> &index);
    //! \name RateHelper interface
    //@{
    T impliedQuote() const;
    void setTermStructure(YieldTermStructure_t<T> *);
    //@}
    //! \name Visitability
    //@{
    void accept(AcyclicVisitor &);
    //@}
  protected:
    void initializeDates();
    Period tenor_;
    Natural settlementDays_;
    Calendar calendar_;
    Period bmaPeriod_;
    BusinessDayConvention bmaConvention_;
    DayCounter bmaDayCount_;
    boost::shared_ptr<BMAIndex> bmaIndex_;
    boost::shared_ptr<IborIndex> iborIndex_;

    boost::shared_ptr<BMASwap> swap_;
    RelinkableHandle<YieldTermStructure_t<T> > termStructureHandle_;
};

typedef BMASwapRateHelper_t<Real> BMASwapRateHelper;

// inline

template <class T> inline T SwapRateHelper_t<T>::spread() const {
    return spread_.empty() ? 0.0 : spread_->value();
}

template <class T>
inline boost::shared_ptr<VanillaSwap> SwapRateHelper_t<T>::swap() const {
    return swap_;
}

template <class T>
inline const Period &SwapRateHelper_t<T>::forwardStart() const {
    return fwdStart_;
}

namespace {
template <class T> void no_deletion(YieldTermStructure_t<T> *){};
}

// implementation

template <class T>
FuturesRateHelper_t<T>::FuturesRateHelper_t(
    const Handle<Quote_t<T> > &price, const Date &immDate,
    Natural lengthInMonths, const Calendar &calendar,
    BusinessDayConvention convention, bool endOfMonth,
    const DayCounter &dayCounter, const Handle<Quote_t<T> > &convAdj)
    : RateHelper_t<T>::Type(price), convAdj_(convAdj) {
    QL_REQUIRE(IMM::isIMMdate(immDate, false),
               immDate << " is not a valid IMM date");
    this->earliestDate_ = immDate;
    this->latestDate_ = calendar.advance(immDate, lengthInMonths * Months,
                                         convention, endOfMonth);
    yearFraction_ =
        dayCounter.yearFraction(this->earliestDate_, this->latestDate_);

    this->registerWith(convAdj_);
}

template <class T>
FuturesRateHelper_t<T>::FuturesRateHelper_t(
    T price, const Date &immDate, Natural lengthInMonths,
    const Calendar &calendar, BusinessDayConvention convention, bool endOfMonth,
    const DayCounter &dayCounter, T convAdj)
    : RateHelper_t<T>::Type(price),
      convAdj_(Handle<Quote_t<T> >(
          shared_ptr<Quote_t<T> >(new SimpleQuote_t<T>(convAdj)))) {
    QL_REQUIRE(IMM::isIMMdate(immDate, false),
               immDate << "is not a valid IMM date");
    this->earliestDate_ = immDate;
    this->latestDate_ = calendar.advance(immDate, lengthInMonths * Months,
                                         convention, endOfMonth);
    yearFraction_ =
        dayCounter.yearFraction(this->earliestDate_, this->latestDate_);
}

template <class T>
FuturesRateHelper_t<T>::FuturesRateHelper_t(const Handle<Quote_t<T> > &price,
                                            const Date &immDate,
                                            const Date &endDate,
                                            const DayCounter &dayCounter,
                                            const Handle<Quote_t<T> > &convAdj)
    : RateHelper_t<T>::Type(price), convAdj_(convAdj) {
    QL_REQUIRE(IMM::isIMMdate(immDate, false),
               immDate << " is not a valid IMM date");
    this->earliestDate_ = immDate;

    if (endDate == Date()) {
        this->latestDate_ = IMM::nextDate(immDate, false);
        this->latestDate_ = IMM::nextDate(this->latestDate_, false);
        this->latestDate_ = IMM::nextDate(this->latestDate_, false);
    } else {
        QL_REQUIRE(endDate > immDate,
                   "end date (" << endDate
                                << ") must be greater than IMM start date ("
                                << immDate << ")");
        this->latestDate_ = endDate;
    }

    yearFraction_ =
        dayCounter.yearFraction(this->earliestDate_, this->latestDate_);

    this->registerWith(convAdj_);
}

template <class T>
FuturesRateHelper_t<T>::FuturesRateHelper_t(T price, const Date &immDate,
                                            const Date &endDate,
                                            const DayCounter &dayCounter,
                                            T convAdj)
    : RateHelper_t<T>::Type(price),
      convAdj_(Handle<Quote_t<T> >(
          shared_ptr<Quote_t<T> >(new SimpleQuote_t<T>(convAdj)))) {
    QL_REQUIRE(IMM::isIMMdate(immDate, false),
               immDate << "is not a valid IMM date");
    this->earliestDate_ = immDate;

    if (endDate == Date()) {
        this->latestDate_ = IMM::nextDate(immDate, false);
        this->latestDate_ = IMM::nextDate(this->latestDate_, false);
        this->latestDate_ = IMM::nextDate(this->latestDate_, false);
    } else {
        QL_REQUIRE(endDate > immDate,
                   "end date (" << endDate
                                << ") must be greater than IMM start date ("
                                << immDate << ")");
        this->latestDate_ = endDate;
    }

    yearFraction_ =
        dayCounter.yearFraction(this->earliestDate_, this->latestDate_);
}

template <class T>
FuturesRateHelper_t<T>::FuturesRateHelper_t(const Handle<Quote_t<T> > &price,
                                            const Date &immDate,
                                            const shared_ptr<IborIndex> &i,
                                            const Handle<Quote_t<T> > &convAdj)
    : RateHelper_t<T>::Type(price), convAdj_(convAdj) {
    QL_REQUIRE(IMM::isIMMdate(immDate, false),
               immDate << "is not a valid IMM date");
    this->earliestDate_ = immDate;
    const Calendar &cal = i->fixingCalendar();
    this->latestDate_ =
        cal.advance(immDate, i->tenor(), i->businessDayConvention());
    yearFraction_ =
        i->dayCounter().yearFraction(this->earliestDate_, this->latestDate_);

    this->registerWith(convAdj);
}

template <class T>
FuturesRateHelper_t<T>::FuturesRateHelper_t(T price, const Date &immDate,
                                            const shared_ptr<IborIndex> &i,
                                            T convAdj)
    : RateHelper_t<T>::Type(price),
      convAdj_(Handle<Quote_t<T> >(
          shared_ptr<Quote_t<T> >(new SimpleQuote_t<T>(convAdj)))) {
    QL_REQUIRE(IMM::isIMMdate(immDate, false),
               immDate << "is not a valid IMM date");
    this->earliestDate_ = immDate;
    const Calendar &cal = i->fixingCalendar();
    this->latestDate_ =
        cal.advance(immDate, i->tenor(), i->businessDayConvention());
    yearFraction_ =
        i->dayCounter().yearFraction(this->earliestDate_, this->latestDate_);
}

template <class T> T FuturesRateHelper_t<T>::impliedQuote() const {
    QL_REQUIRE(this->termStructure_ != 0, "term structure not set");
    T forwardRate = (this->termStructure_->discount(this->earliestDate_) /
                         this->termStructure_->discount(this->latestDate_) -
                     1.0) /
                    yearFraction_;
    T convAdj = convAdj_.empty() ? 0.0 : convAdj_->value();
    QL_ENSURE(convAdj >= 0.0, "Negative (" << convAdj
                                           << ") futures convexity adjustment");
    T futureRate = forwardRate + convAdj;
    return 100.0 * (1.0 - futureRate);
}

template <class T> T FuturesRateHelper_t<T>::convexityAdjustment() const {
    return convAdj_.empty() ? 0.0 : convAdj_->value();
}

template <class T> void FuturesRateHelper_t<T>::accept(AcyclicVisitor &v) {
    Visitor<FuturesRateHelper_t<T> > *v1 =
        dynamic_cast<Visitor<FuturesRateHelper_t<T> > *>(&v);
    if (v1 != 0)
        v1->visit(*this);
    else
        this->accept(v);
}

template <class T>
DepositRateHelper_t<T>::DepositRateHelper_t(
    const Handle<Quote_t<T> > &rate, const Period &tenor, Natural fixingDays,
    const Calendar &calendar, BusinessDayConvention convention, bool endOfMonth,
    const DayCounter &dayCounter)
    : RelativeDateRateHelper_t<T>::Type(rate) {
    iborIndex_ = shared_ptr<IborIndex>(
        new IborIndex("no-fix", // never take fixing into account
                      tenor, fixingDays, Currency(), calendar, convention,
                      endOfMonth, dayCounter, termStructureHandle_));
    initializeDates();
}

template <class T>
DepositRateHelper_t<T>::DepositRateHelper_t(T rate, const Period &tenor,
                                            Natural fixingDays,
                                            const Calendar &calendar,
                                            BusinessDayConvention convention,
                                            bool endOfMonth,
                                            const DayCounter &dayCounter)
    : RelativeDateRateHelper_t<T>::Type(rate) {
    iborIndex_ = shared_ptr<IborIndex>(
        new IborIndex("no-fix", // never take fixing into account
                      tenor, fixingDays, Currency(), calendar, convention,
                      endOfMonth, dayCounter, termStructureHandle_));
    initializeDates();
}

template <class T>
DepositRateHelper_t<T>::DepositRateHelper_t(const Handle<Quote_t<T> > &rate,
                                            const shared_ptr<IborIndex> &i)
    : RelativeDateRateHelper_t<T>::Type(rate) {
    // do not use clone, as we do not want to take fixing into account
    iborIndex_ = shared_ptr<IborIndex>(
        new IborIndex("no-fix", // never take fixing into account
                      i->tenor(), i->fixingDays(), Currency(),
                      i->fixingCalendar(), i->businessDayConvention(),
                      i->endOfMonth(), i->dayCounter(), termStructureHandle_));
    initializeDates();
}

template <class T>
DepositRateHelper_t<T>::DepositRateHelper_t(T rate,
                                            const shared_ptr<IborIndex> &i)
    : RelativeDateRateHelper_t<T>::Type(rate) {
    // do not use clone, as we do not want to take fixing into account
    iborIndex_ = shared_ptr<IborIndex>(
        new IborIndex("no-fix", // never take fixing into account
                      i->tenor(), i->fixingDays(), Currency(),
                      i->fixingCalendar(), i->businessDayConvention(),
                      i->endOfMonth(), i->dayCounter(), termStructureHandle_));
    initializeDates();
}

template <class T> T DepositRateHelper_t<T>::impliedQuote() const {
    QL_REQUIRE(this->termStructure_ != 0, "term structure not set");
    return iborIndex_->fixing(fixingDate_, true);
}

template <class T>
void DepositRateHelper_t<T>::setTermStructure(YieldTermStructure_t<T> *t) {
    // no need to register---the index is not lazy
    termStructureHandle_.linkTo(
        shared_ptr<YieldTermStructure_t<T> >(t, no_deletion<T>), false);
    this->setTermStructure(t);
}

template <class T> void DepositRateHelper_t<T>::initializeDates() {
    // if the evaluation date is not a business day
    // then move to the next business day
    Date referenceDate =
        iborIndex_->fixingCalendar().adjust(this->evaluationDate_);
    this->earliestDate_ = iborIndex_->fixingCalendar().advance(
        referenceDate, iborIndex_->fixingDays() * Days);
    this->latestDate_ = iborIndex_->maturityDate(this->earliestDate_);
    fixingDate_ = iborIndex_->fixingDate(this->earliestDate_);
}

template <class T> void DepositRateHelper_t<T>::accept(AcyclicVisitor &v) {
    Visitor<DepositRateHelper> *v1 =
        dynamic_cast<Visitor<DepositRateHelper> *>(&v);
    if (v1 != 0)
        v1->visit(*this);
    else
        this->accept(v);
}

template <class T>
FraRateHelper_t<T>::FraRateHelper_t(const Handle<Quote_t<T> > &rate,
                                    Natural monthsToStart, Natural monthsToEnd,
                                    Natural fixingDays,
                                    const Calendar &calendar,
                                    BusinessDayConvention convention,
                                    bool endOfMonth,
                                    const DayCounter &dayCounter)
    : RelativeDateRateHelper_t<T>::Type(rate),
      periodToStart_(monthsToStart * Months) {
    QL_REQUIRE(monthsToEnd > monthsToStart,
               "monthsToEnd (" << monthsToEnd
                               << ") must be grater than monthsToStart ("
                               << monthsToStart << ")");
    // no way to take fixing into account,
    // even if we would like to for FRA over today
    iborIndex_ = shared_ptr<IborIndex>(new IborIndex(
        "no-fix", // correct family name would be needed
        (monthsToEnd - monthsToStart) * Months, fixingDays, Currency(),
        calendar, convention, endOfMonth, dayCounter, termStructureHandle_));
    initializeDates();
}

template <class T>
FraRateHelper_t<T>::FraRateHelper_t(T rate, Natural monthsToStart,
                                    Natural monthsToEnd, Natural fixingDays,
                                    const Calendar &calendar,
                                    BusinessDayConvention convention,
                                    bool endOfMonth,
                                    const DayCounter &dayCounter)
    : RelativeDateRateHelper_t<T>::Type(rate),
      periodToStart_(monthsToStart * Months) {
    QL_REQUIRE(monthsToEnd > monthsToStart,
               "monthsToEnd (" << monthsToEnd
                               << ") must be grater than monthsToStart ("
                               << monthsToStart << ")");
    // no way to take fixing into account,
    // even if we would like to for FRA over today
    iborIndex_ = shared_ptr<IborIndex>(new IborIndex(
        "no-fix", // correct family name would be needed
        (monthsToEnd - monthsToStart) * Months, fixingDays, Currency(),
        calendar, convention, endOfMonth, dayCounter, termStructureHandle_));
    initializeDates();
}

template <class T>
FraRateHelper_t<T>::FraRateHelper_t(const Handle<Quote_t<T> > &rate,
                                    Natural monthsToStart,
                                    const shared_ptr<IborIndex> &i)
    : RelativeDateRateHelper_t<T>::Type(rate),
      periodToStart_(monthsToStart * Months) {
    // take fixing into account
    iborIndex_ = i->clone(termStructureHandle_);
    // We want to be notified of changes of fixings, but we don't
    // want notifications from termStructureHandle_ (they would
    // interfere with bootstrapping.)
    iborIndex_->unregisterWith(termStructureHandle_);
    this->registerWith(iborIndex_);
    initializeDates();
}

template <class T>
FraRateHelper_t<T>::FraRateHelper_t(T rate, Natural monthsToStart,
                                    const shared_ptr<IborIndex> &i)
    : RelativeDateRateHelper_t<T>::Type(rate),
      periodToStart_(monthsToStart * Months) {
    // take fixing into account
    iborIndex_ = i->clone(termStructureHandle_);
    // see above
    iborIndex_->unregisterWith(termStructureHandle_);
    this->registerWith(iborIndex_);
    initializeDates();
}

template <class T>
FraRateHelper_t<T>::FraRateHelper_t(const Handle<Quote_t<T> > &rate,
                                    Period periodToStart,
                                    Natural lengthInMonths, Natural fixingDays,
                                    const Calendar &calendar,
                                    BusinessDayConvention convention,
                                    bool endOfMonth,
                                    const DayCounter &dayCounter)
    : RelativeDateRateHelper_t<T>::Type(rate), periodToStart_(periodToStart) {
    // no way to take fixing into account,
    // even if we would like to for FRA over today
    iborIndex_ = shared_ptr<IborIndex>(new IborIndex(
        "no-fix", // correct family name would be needed
        lengthInMonths * Months, fixingDays, Currency(), calendar, convention,
        endOfMonth, dayCounter, termStructureHandle_));
    initializeDates();
}

template <class T>
FraRateHelper_t<T>::FraRateHelper_t(T rate, Period periodToStart,
                                    Natural lengthInMonths, Natural fixingDays,
                                    const Calendar &calendar,
                                    BusinessDayConvention convention,
                                    bool endOfMonth,
                                    const DayCounter &dayCounter)
    : RelativeDateRateHelper_t<T>::Type(rate), periodToStart_(periodToStart) {
    // no way to take fixing into account,
    // even if we would like to for FRA over today
    iborIndex_ = shared_ptr<IborIndex>(new IborIndex(
        "no-fix", // correct family name would be needed
        lengthInMonths * Months, fixingDays, Currency(), calendar, convention,
        endOfMonth, dayCounter, termStructureHandle_));
    initializeDates();
}

template <class T>
FraRateHelper_t<T>::FraRateHelper_t(const Handle<Quote_t<T> > &rate,
                                    Period periodToStart,
                                    const shared_ptr<IborIndex> &i)
    : RelativeDateRateHelper_t<T>::Type(rate), periodToStart_(periodToStart) {
    // take fixing into account
    iborIndex_ = i->clone(termStructureHandle_);
    // see above
    iborIndex_->unregisterWith(termStructureHandle_);
    this->registerWith(iborIndex_);
    initializeDates();
}

template <class T>
FraRateHelper_t<T>::FraRateHelper_t(T rate, Period periodToStart,
                                    const shared_ptr<IborIndex> &i)
    : RelativeDateRateHelper_t<T>::Type(rate), periodToStart_(periodToStart) {
    // take fixing into account
    iborIndex_ = i->clone(termStructureHandle_);
    // see above
    iborIndex_->unregisterWith(termStructureHandle_);
    this->registerWith(iborIndex_);
    initializeDates();
}

template <class T> T FraRateHelper_t<T>::impliedQuote() const {
    QL_REQUIRE(this->termStructure_ != 0, "term structure not set");
    return iborIndex_->fixing(fixingDate_, true);
}

template <class T>
void FraRateHelper_t<T>::setTermStructure(YieldTermStructure_t<T> *t) {
    // no need to register---the index is not lazy
    this->termStructureHandle_.linkTo(
        shared_ptr<YieldTermStructure_t<T> >(t, no_deletion<T>), false);
    this->setTermStructure(t);
}

template <class T> void FraRateHelper_t<T>::initializeDates() {
    // if the evaluation date is not a business day
    // then move to the next business day
    Date referenceDate =
        iborIndex_->fixingCalendar().adjust(this->evaluationDate_);
    Date spotDate = iborIndex_->fixingCalendar().advance(
        referenceDate, iborIndex_->fixingDays() * Days);
    this->earliestDate_ = iborIndex_->fixingCalendar().advance(
        spotDate, periodToStart_, iborIndex_->businessDayConvention(),
        iborIndex_->endOfMonth());
    this->latestDate_ = iborIndex_->maturityDate(this->earliestDate_);
    fixingDate_ = iborIndex_->fixingDate(this->earliestDate_);
}

template <class T> void FraRateHelper_t<T>::accept(AcyclicVisitor &v) {
    Visitor<FraRateHelper_t<T> > *v1 =
        dynamic_cast<Visitor<FraRateHelper_t<T> > *>(&v);
    if (v1 != 0)
        v1->visit(*this);
    else
        this->accept(v);
}

template <class T>
SwapRateHelper_t<T>::SwapRateHelper_t(
    const Handle<Quote_t<T> > &rate, const shared_ptr<SwapIndex> &swapIndex,
    const Handle<Quote_t<T> > &spread, const Period &fwdStart,
    const Handle<YieldTermStructure_t<T> > &discount)
    : RelativeDateRateHelper_t<T>::Type(rate), tenor_(swapIndex->tenor()),
      calendar_(swapIndex->fixingCalendar()),
      fixedConvention_(swapIndex->fixedLegConvention()),
      fixedFrequency_(swapIndex->fixedLegTenor().frequency()),
      fixedDayCount_(swapIndex->dayCounter()), spread_(spread),
      fwdStart_(fwdStart), discountHandle_(discount) {
    // take fixing into account
    iborIndex_ = swapIndex->iborIndex()->clone(termStructureHandle_);
    // We want to be notified of changes of fixings, but we don't
    // want notifications from termStructureHandle_ (they would
    // interfere with bootstrapping.)
    iborIndex_->unregisterWith(termStructureHandle_);

    this->registerWith(iborIndex_);
    this->registerWith(spread_);
    this->registerWith(discountHandle_);
    initializeDates();
}

template <class T>
SwapRateHelper_t<T>::SwapRateHelper_t(
    const Handle<Quote_t<T> > &rate, const Period &tenor,
    const Calendar &calendar, Frequency fixedFrequency,
    BusinessDayConvention fixedConvention, const DayCounter &fixedDayCount,
    const shared_ptr<IborIndex> &iborIndex, const Handle<Quote_t<T> > &spread,
    const Period &fwdStart, const Handle<YieldTermStructure_t<T> > &discount)
    : RelativeDateRateHelper_t<T>::Type(rate), tenor_(tenor),
      calendar_(calendar), fixedConvention_(fixedConvention),
      fixedFrequency_(fixedFrequency), fixedDayCount_(fixedDayCount),
      spread_(spread), fwdStart_(fwdStart), discountHandle_(discount) {
    // take fixing into account
    iborIndex_ = iborIndex->clone(termStructureHandle_);
    // We want to be notified of changes of fixings, but we don't
    // want notifications from termStructureHandle_ (they would
    // interfere with bootstrapping.)
    iborIndex_->unregisterWith(termStructureHandle_);

    this->registerWith(iborIndex_);
    this->registerWith(spread_);
    this->registerWith(discountHandle_);
    initializeDates();
}

template <class T>
SwapRateHelper_t<T>::SwapRateHelper_t(
    T rate, const Period &tenor, const Calendar &calendar,
    Frequency fixedFrequency, BusinessDayConvention fixedConvention,
    const DayCounter &fixedDayCount, const shared_ptr<IborIndex> &iborIndex,
    const Handle<Quote_t<T> > &spread, const Period &fwdStart,
    const Handle<YieldTermStructure_t<T> > &discount)
    : RelativeDateRateHelper_t<T>::Type(rate), tenor_(tenor),
      calendar_(calendar), fixedConvention_(fixedConvention),
      fixedFrequency_(fixedFrequency), fixedDayCount_(fixedDayCount),
      spread_(spread), fwdStart_(fwdStart), discountHandle_(discount) {
    // take fixing into account
    iborIndex_ = iborIndex->clone(termStructureHandle_);
    // We want to be notified of changes of fixings, but we don't
    // want notifications from termStructureHandle_ (they would
    // interfere with bootstrapping.)
    iborIndex_->unregisterWith(termStructureHandle_);

    this->registerWith(iborIndex_);
    this->registerWith(spread_);
    this->registerWith(discountHandle_);
    initializeDates();
}

template <class T>
SwapRateHelper_t<T>::SwapRateHelper_t(
    T rate, const shared_ptr<SwapIndex> &swapIndex,
    const Handle<Quote_t<T> > &spread, const Period &fwdStart,
    const Handle<YieldTermStructure_t<T> > &discount)
    : RelativeDateRateHelper_t<T>::Type(rate), tenor_(swapIndex->tenor()),
      calendar_(swapIndex->fixingCalendar()),
      fixedConvention_(swapIndex->fixedLegConvention()),
      fixedFrequency_(swapIndex->fixedLegTenor().frequency()),
      fixedDayCount_(swapIndex->dayCounter()), spread_(spread),
      fwdStart_(fwdStart), discountHandle_(discount) {
    // take fixing into account
    iborIndex_ = swapIndex->iborIndex()->clone(termStructureHandle_);
    // We want to be notified of changes of fixings, but we don't
    // want notifications from termStructureHandle_ (they would
    // interfere with bootstrapping.)
    iborIndex_->unregisterWith(termStructureHandle_);

    this->registerWith(iborIndex_);
    this->registerWith(spread_);
    this->registerWith(discountHandle_);
    initializeDates();
}

template <class T> void SwapRateHelper_t<T>::initializeDates() {

    // 1. do not pass the spread here, as it might be a Quote
    //    i.e. it can dinamically change
    // 2. input discount curve Handle might be empty now but it could
    //    be assigned a curve later; use a RelinkableHandle here
    swap_ = MakeVanillaSwap(tenor_, iborIndex_, 0.0, fwdStart_)
                .withDiscountingTermStructure(discountRelinkableHandle_)
                .withFixedLegDayCount(fixedDayCount_)
                .withFixedLegTenor(Period(fixedFrequency_))
                .withFixedLegConvention(fixedConvention_)
                .withFixedLegTerminationDateConvention(fixedConvention_)
                .withFixedLegCalendar(calendar_)
                .withFloatingLegCalendar(calendar_);

    this->earliestDate_ = swap_->startDate();

    // Usually...
    this->latestDate_ = swap_->maturityDate();
// ...but due to adjustments, the last floating coupon might
// need a later date for fixing
#ifdef QL_USE_INDEXED_COUPON
    shared_ptr<FloatingRateCoupon> lastFloating =
        boost::dynamic_pointer_cast<FloatingRateCoupon>(
            swap_->floatingLeg().back());
    Date fixingValueDate = iborIndex_->valueDate(lastFloating->fixingDate());
    Date endValueDate = iborIndex_->maturityDate(fixingValueDate);
    this->latestDate_ = std::max(this->latestDate_, endValueDate);
#endif
}

template <class T>
void SwapRateHelper_t<T>::setTermStructure(YieldTermStructure_t<T> *t) {
    // do not set the relinkable handle as an observer -
    // force recalculation when needed
    bool observer = false;

    shared_ptr<YieldTermStructure_t<T> > temp(t, no_deletion<T>);
    termStructureHandle_.linkTo(temp, observer);

    if (discountHandle_.empty())
        discountRelinkableHandle_.linkTo(temp, observer);
    else
        discountRelinkableHandle_.linkTo(*discountHandle_, observer);

    this->setTermStructure(t);
}

template <class T> T SwapRateHelper_t<T>::impliedQuote() const {
    QL_REQUIRE(this->termStructure_ != 0, "term structure not set");
    // we didn't register as observers - force calculation
    swap_->recalculate();
    // weak implementation... to be improved
    static const Spread basisPoint = 1.0e-4;
    T floatingLegNPV = swap_->floatingLegNPV();
    T spread = spread_.empty() ? 0.0 : spread_->value();
    T spreadNPV = swap_->floatingLegBPS() / basisPoint * spread;
    T totNPV = -(floatingLegNPV + spreadNPV);
    T result = totNPV / (swap_->fixedLegBPS() / basisPoint);
    return result;
}

template <class T> void SwapRateHelper_t<T>::accept(AcyclicVisitor &v) {
    Visitor<SwapRateHelper_t> *v1 =
        dynamic_cast<Visitor<SwapRateHelper_t> *>(&v);
    if (v1 != 0)
        v1->visit(*this);
    else
        this->accept(v);
}

template <class T>
BMASwapRateHelper_t<T>::BMASwapRateHelper_t(
    const Handle<Quote_t<T> > &liborFraction, const Period &tenor,
    Natural settlementDays, const Calendar &calendar,
    // bma leg
    const Period &bmaPeriod, BusinessDayConvention bmaConvention,
    const DayCounter &bmaDayCount, const shared_ptr<BMAIndex> &bmaIndex,
    // libor leg
    const shared_ptr<IborIndex> &iborIndex)
    : RelativeDateRateHelper(liborFraction), tenor_(tenor),
      settlementDays_(settlementDays), calendar_(calendar),
      bmaPeriod_(bmaPeriod), bmaConvention_(bmaConvention),
      bmaDayCount_(bmaDayCount), bmaIndex_(bmaIndex), iborIndex_(iborIndex) {
    this->registerWith(iborIndex_);
    this->registerWith(bmaIndex_);
    initializeDates();
}

template <class T> void BMASwapRateHelper_t<T>::initializeDates() {
    this->earliestDate_ = calendar_.advance(this->evaluationDate_,
                                            settlementDays_ * Days, Following);

    Date maturity = this->earliestDate_ + tenor_;

    // dummy BMA index with curve/swap arguments
    shared_ptr<BMAIndex> clonedIndex(new BMAIndex(termStructureHandle_));

    Schedule bmaSchedule = MakeSchedule()
                               .from(this->earliestDate_)
                               .to(maturity)
                               .withTenor(bmaPeriod_)
                               .withCalendar(bmaIndex_->fixingCalendar())
                               .withConvention(bmaConvention_)
                               .backwards();

    Schedule liborSchedule =
        MakeSchedule()
            .from(this->earliestDate_)
            .to(maturity)
            .withTenor(iborIndex_->tenor())
            .withCalendar(iborIndex_->fixingCalendar())
            .withConvention(iborIndex_->businessDayConvention())
            .endOfMonth(iborIndex_->endOfMonth())
            .backwards();

    swap_ = shared_ptr<BMASwap>(
        new BMASwap(BMASwap::Payer, 100.0, liborSchedule,
                    0.75, // arbitrary
                    0.0, iborIndex_, iborIndex_->dayCounter(), bmaSchedule,
                    clonedIndex, bmaDayCount_));
    swap_->setPricingEngine(shared_ptr<PricingEngine>(
        new DiscountingSwapEngine(iborIndex_->forwardingTermStructure())));

    Date d = calendar_.adjust(swap_->maturityDate(), Following);
    Weekday w = d.weekday();
    Date nextWednesday = (w >= 4) ? d + (11 - w) * Days : d + (4 - w) * Days;
    this->latestDate_ = clonedIndex->valueDate(
        clonedIndex->fixingCalendar().adjust(nextWednesday));
}

template <class T>
void BMASwapRateHelper_t<T>::setTermStructure(YieldTermStructure_t<T> *t) {
    // do not set the relinkable handle as an observer -
    // force recalculation when needed
    termStructureHandle_.linkTo(
        shared_ptr<YieldTermStructure_t<T> >(t, no_deletion<T>), false);
    this->setTermStructure(t);
}

template <class T> T BMASwapRateHelper_t<T>::impliedQuote() const {
    QL_REQUIRE(this->termStructure_ != 0, "term structure not set");
    // we didn't register as observers - force calculation
    swap_->recalculate();
    return swap_->fairLiborFraction();
}

template <class T> void BMASwapRateHelper_t<T>::accept(AcyclicVisitor &v) {
    Visitor<BMASwapRateHelper_t<T> > *v1 =
        dynamic_cast<Visitor<BMASwapRateHelper_t<T> > *>(&v);
    if (v1 != 0)
        v1->visit(*this);
    else
        this->accept(v);
}

} // namespace QuantLib

#endif

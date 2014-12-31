/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl
 Copyright (C) 2003, 2004 StatPro Italia srl
 Copyright (C) 2003 Nicolas Di Césaré
 Copyright (C) 2006, 2007 Cristina Duminuco
 Copyright (C) 2006 Ferdinando Ametrano
 Copyright (C) 2007 Giorgio Facchinetti
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

/*! \file floatingratecoupon.hpp
    \brief Coupon paying a variable index-based rate
*/

#ifndef quantlib_floating_rate_coupon_hpp
#define quantlib_floating_rate_coupon_hpp

#include <ql/cashflows/coupon.hpp>
#include <ql/patterns/visitor.hpp>
#include <ql/time/daycounter.hpp>
#include <ql/handle.hpp>
#include <ql/termstructures/yieldtermstructure.hpp>
#include <ql/indexes/interestrateindex.hpp>
#include <ql/cashflows/couponpricerbase.hpp>

namespace QuantLib {

template <class T> class InterestRateIndex_t;
template <class T> class FloatingRateCouponPricer_t;

using std::min;

//! base floating-rate coupon class
template <class T = Real>
class FloatingRateCoupon_t : public Coupon_t<T>, public Observer {
  public:
    FloatingRateCoupon_t(
        const Date &paymentDate, T nominal, const Date &startDate,
        const Date &endDate, Natural fixingDays,
        const boost::shared_ptr<InterestRateIndex_t<T> > &index,
        T gearing = 1.0, T spread = 0.0, const Date &refPeriodStart = Date(),
        const Date &refPeriodEnd = Date(),
        const DayCounter &dayCounter = DayCounter(), bool isInArrears = false);

    //! \name CashFlow interface
    //@{
    T amount() const {
        return rate() * this->accrualPeriod() * this->nominal();
    }
    //@}

    //! \name Coupon interface
    //@{
    T rate() const;
    T price(const Handle<YieldTermStructure_t<T> > &discountingCurve) const;
    DayCounter dayCounter() const { return dayCounter_; }
    T accruedAmount(const Date &) const;
    //@}

    //! \name Inspectors
    //@{
    //! floating index
    const boost::shared_ptr<InterestRateIndex_t<T> > &index() const;
    //! fixing days
    Natural fixingDays() const { return fixingDays_; }
    //! fixing date
    virtual Date fixingDate() const;
    //! index gearing, i.e. multiplicative coefficient for the index
    T gearing() const { return gearing_; }
    //! spread paid over the fixing of the underlying index
    T spread() const { return spread_; }
    //! fixing of the underlying index
    virtual T indexFixing() const;
    //! convexity adjustment
    virtual T convexityAdjustment() const;
    //! convexity-adjusted fixing
    virtual T adjustedFixing() const;
    //! whether or not the coupon fixes in arrears
    bool isInArrears() const { return isInArrears_; }
    //@}

    //! \name Observer interface
    //@{
    void update() { this->notifyObservers(); }
    //@}

    //! \name Visitability
    //@{
    virtual void accept(AcyclicVisitor &);
    //@}

    void setPricer(const boost::shared_ptr<FloatingRateCouponPricer_t<T> > &);
    boost::shared_ptr<FloatingRateCouponPricer_t<T> > pricer() const;

  protected:
    //! convexity adjustment for the given index fixing
    T convexityAdjustmentImpl(T fixing) const;
    boost::shared_ptr<InterestRateIndex_t<T> > index_;
    DayCounter dayCounter_;
    Natural fixingDays_;
    T gearing_;
    T spread_;
    bool isInArrears_;
    boost::shared_ptr<FloatingRateCouponPricer_t<T> > pricer_;
};

typedef FloatingRateCoupon_t<Real> FloatingRateCoupon;

// inline definitions

template <class T>
inline const boost::shared_ptr<InterestRateIndex_t<T> > &
FloatingRateCoupon_t<T>::index() const {
    return index_;
}

template <class T>
inline T FloatingRateCoupon_t<T>::convexityAdjustment() const {
    return convexityAdjustmentImpl(indexFixing());
}

template <class T> inline T FloatingRateCoupon_t<T>::adjustedFixing() const {
    return (rate() - spread()) / gearing();
}

template <class T>
inline boost::shared_ptr<FloatingRateCouponPricer_t<T> >
FloatingRateCoupon_t<T>::pricer() const {
    return pricer_;
}

template <class T>
inline T FloatingRateCoupon_t<T>::convexityAdjustmentImpl(T fixing) const {
    return (gearing() == 0.0 ? 0.0 : adjustedFixing() - fixing);
}

template <class T>
inline void FloatingRateCoupon_t<T>::accept(AcyclicVisitor &v) {
    Visitor<FloatingRateCoupon_t<T> > *v1 =
        dynamic_cast<Visitor<FloatingRateCoupon_t<T> > *>(&v);
    if (v1 != 0)
        v1->visit(*this);
    else
        Coupon_t<T>::accept(v);
}

// implementation

template <class T>
FloatingRateCoupon_t<T>::FloatingRateCoupon_t(
    const Date &paymentDate, T nominal, const Date &startDate,
    const Date &endDate, Natural fixingDays,
    const boost::shared_ptr<InterestRateIndex_t<T> > &index, T gearing,
    T spread, const Date &refPeriodStart, const Date &refPeriodEnd,
    const DayCounter &dayCounter, bool isInArrears)
    : Coupon_t<T>(paymentDate, nominal, startDate, endDate, refPeriodStart,
                  refPeriodEnd),
      index_(index), dayCounter_(dayCounter),
      fixingDays_(fixingDays == Null<Natural>() ? index->fixingDays()
                                                : fixingDays),
      gearing_(gearing), spread_(spread), isInArrears_(isInArrears) {
    QL_REQUIRE(gearing_ != 0, "Null gearing not allowed");

    if (dayCounter_.empty())
        dayCounter_ = index_->dayCounter();

    registerWith(index_);
    registerWith(Settings::instance().evaluationDate());
}

template <class T>
void FloatingRateCoupon_t<T>::setPricer(
    const boost::shared_ptr<FloatingRateCouponPricer_t<T> > &pricer) {
    if (this->pricer_)
        unregisterWith(this->pricer_);
    this->pricer_ = pricer;
    if (this->pricer_)
        registerWith(this->pricer_);
    update();
}

template <class T>
T FloatingRateCoupon_t<T>::accruedAmount(const Date &d) const {
    if (d <= this->accrualStartDate_ || d > this->paymentDate_) {
        return 0.0;
    } else {
        return this->nominal() * this->rate() *
               this->dayCounter().yearFraction(
                   this->accrualStartDate_, min(d, this->accrualEndDate_),
                   this->refPeriodStart_, this->refPeriodEnd_);
    }
}

template <class T> Date FloatingRateCoupon_t<T>::fixingDate() const {
    // if isInArrears_ fix at the end of period
    Date refDate =
        isInArrears_ ? this->accrualEndDate_ : this->accrualStartDate_;
    return this->index_->fixingCalendar().advance(
        refDate, -static_cast<Integer>(this->fixingDays_), Days, Preceding);
}

template <class T> T FloatingRateCoupon_t<T>::rate() const {
    QL_REQUIRE(pricer_, "pricer not set");
    pricer_->initialize(*this);
    return pricer_->swapletRate();
}

template <class T>
T FloatingRateCoupon_t<T>::price(
    const Handle<YieldTermStructure_t<T> > &discountingCurve) const {
    return amount() * discountingCurve->discount(this->date());
}

template <class T> T FloatingRateCoupon_t<T>::indexFixing() const {
    return index_->fixing(fixingDate());
}
}

#endif

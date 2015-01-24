/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2006, 2007 StatPro Italia srl
 Copyright (C) 2006 Cristina Duminuco
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

/*! \file capflooredcoupon.hpp
    \brief Floating rate coupon with additional cap/floor
*/

#ifndef quantlib_capped_floored_coupon_base_hpp
#define quantlib_capped_floored_coupon_base_hpp

#include <ql/cashflows/iborcouponbase.hpp>
#include <ql/utilities/null.hpp>

namespace QuantLib {

class Date;

//! Capped and/or floored floating-rate coupon
/*! The payoff \f$ P \f$ of a capped floating-rate coupon is:
    \f[ P = N \times T \times \min(a L + b, C). \f]
    The payoff of a floored floating-rate coupon is:
    \f[ P = N \times T \times \max(a L + b, F). \f]
    The payoff of a collared floating-rate coupon is:
    \f[ P = N \times T \times \min(\max(a L + b, F), C). \f]

    where \f$ N \f$ is the notional, \f$ T \f$ is the accrual
    time, \f$ L \f$ is the floating rate, \f$ a \f$ is its
    gearing, \f$ b \f$ is the spread, and \f$ C \f$ and \f$ F \f$
    the strikes.

    They can be decomposed in the following manner.
    Decomposition of a capped floating rate coupon:
    \f[
    R = \min(a L + b, C) = (a L + b) + \min(C - b - \xi |a| L, 0)
    \f]
    where \f$ \xi = sgn(a) \f$. Then:
    \f[
    R = (a L + b) + |a| \min(\frac{C - b}{|a|} - \xi L, 0)
    \f]
*/
template <class T>
class CappedFlooredCoupon_t : public FloatingRateCoupon_t<T> {
  public:
    CappedFlooredCoupon_t(
        const boost::shared_ptr<FloatingRateCoupon_t<T> > &underlying,
        T cap = Null<Rate>(), T floor = Null<Rate>());
    //! \name Coupon interface
    //@{
    T rate() const;
    T convexityAdjustment() const;
    //@}
    //! cap
    T cap() const;
    //! floor
    T floor() const;
    //! effective cap of fixing
    T effectiveCap() const;
    //! effective floor of fixing
    T effectiveFloor() const;
    //@}
    //! \name Observer interface
    //@{
    void update();
    //@}
    //! \name Visitability
    //@{
    virtual void accept(AcyclicVisitor &);

    bool isCapped() const { return isCapped_; }
    bool isFloored() const { return isFloored_; }

    void
    setPricer(const boost::shared_ptr<FloatingRateCouponPricer_t<T> > &pricer);

  protected:
    // data
    boost::shared_ptr<FloatingRateCoupon_t<T> > underlying_;
    bool isCapped_, isFloored_;
    T cap_, floor_;
};

typedef CappedFlooredCoupon_t<Real> CappedFlooredCoupon;

template <class T>
class CappedFlooredIborCoupon_t : public CappedFlooredCoupon_t<T> {
  public:
    CappedFlooredIborCoupon_t(
        const Date &paymentDate, T nominal, const Date &startDate,
        const Date &endDate, Natural fixingDays,
        const boost::shared_ptr<IborIndex_t<T> > &index, T gearing = 1.0,
        T spread = 0.0, T cap = Null<Rate>(), T floor = Null<Rate>(),
        const Date &refPeriodStart = Date(), const Date &refPeriodEnd = Date(),
        const DayCounter &dayCounter = DayCounter(), bool isInArrears = false)
        : CappedFlooredCoupon_t<T>(
              boost::shared_ptr<FloatingRateCoupon_t<T> >(new IborCoupon_t<T>(
                  paymentDate, nominal, startDate, endDate, fixingDays, index,
                  gearing, spread, refPeriodStart, refPeriodEnd, dayCounter,
                  isInArrears)),
              cap, floor) {}

    virtual void accept(AcyclicVisitor &v) {
        Visitor<CappedFlooredIborCoupon_t<T> > *v1 =
            dynamic_cast<Visitor<CappedFlooredIborCoupon_t<T> > *>(&v);
        if (v1 != 0)
            v1->visit(*this);
        else
            CappedFlooredCoupon_t<T>::accept(v);
    }
};

typedef CappedFlooredIborCoupon_t<Real> CappedFlooredIborCoupon;

// implementation

template <class T>
CappedFlooredCoupon_t<T>::CappedFlooredCoupon_t(
    const boost::shared_ptr<FloatingRateCoupon_t<T> > &underlying, T cap,
    T floor)
    : FloatingRateCoupon_t<T>(
          underlying->date(), underlying->nominal(),
          underlying->accrualStartDate(), underlying->accrualEndDate(),
          underlying->fixingDays(), underlying->index(), underlying->gearing(),
          underlying->spread(), underlying->referencePeriodStart(),
          underlying->referencePeriodEnd(), underlying->dayCounter(),
          underlying->isInArrears()),
      underlying_(underlying), isCapped_(false), isFloored_(false) {

    if (this->gearing_ > 0) {
        if (cap != Null<Rate>()) {
            isCapped_ = true;
            cap_ = cap;
        }
        if (floor != Null<Rate>()) {
            floor_ = floor;
            isFloored_ = true;
        }
    } else {
        if (cap != Null<Rate>()) {
            floor_ = cap;
            isFloored_ = true;
        }
        if (floor != Null<Rate>()) {
            isCapped_ = true;
            cap_ = floor;
        }
    }

    if (isCapped_ && isFloored_) {
        QL_REQUIRE(cap >= floor, "cap level (" << cap
                                               << ") less than floor level ("
                                               << floor << ")");
    }

    this->registerWith(underlying);
}

template <class T>
void CappedFlooredCoupon_t<T>::setPricer(
    const boost::shared_ptr<FloatingRateCouponPricer_t<T> > &pricer) {
    FloatingRateCoupon_t<T>::setPricer(pricer);
    underlying_->setPricer(pricer);
}

template <class T> T CappedFlooredCoupon_t<T>::rate() const {
    QL_REQUIRE(underlying_->pricer(), "pricer not set");
    T swapletRate = underlying_->rate();
    T floorletRate = 0.;
    if (isFloored_)
        floorletRate = underlying_->pricer()->floorletRate(effectiveFloor());
    T capletRate = 0.;
    if (isCapped_)
        capletRate = underlying_->pricer()->capletRate(effectiveCap());
    return swapletRate + floorletRate - capletRate;
}

template <class T> T CappedFlooredCoupon_t<T>::convexityAdjustment() const {
    return underlying_->convexityAdjustment();
}

template <class T> T CappedFlooredCoupon_t<T>::cap() const {
    if ((this->gearing_ > 0) && isCapped_)
        return cap_;
    if ((this->gearing_ < 0) && isFloored_)
        return floor_;
    return Null<Rate>();
}

template <class T> T CappedFlooredCoupon_t<T>::floor() const {
    if ((this->gearing_ > 0) && isFloored_)
        return floor_;
    if ((this->gearing_ < 0) && isCapped_)
        return cap_;
    return Null<T>();
}

template <class T> T CappedFlooredCoupon_t<T>::effectiveCap() const {
    if (isCapped_)
        return (cap_ - this->spread()) / this->gearing();
    else
        return Null<T>();
}

template <class T> T CappedFlooredCoupon_t<T>::effectiveFloor() const {
    if (isFloored_)
        return (floor_ - this->spread()) / this->gearing();
    else
        return Null<T>();
}

template <class T> void CappedFlooredCoupon_t<T>::update() {
    this->notifyObservers();
}

template <class T> void CappedFlooredCoupon_t<T>::accept(AcyclicVisitor &v) {
    typedef FloatingRateCoupon_t<T> super;
    Visitor<CappedFlooredCoupon_t<T> > *v1 =

        dynamic_cast<Visitor<CappedFlooredCoupon_t<T> > *>(&v);
    if (v1 != 0)
        v1->visit(*this);
    else
        super::accept(v);
}
} // namesapce QuantLib

#endif

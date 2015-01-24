/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2007, 2011 Ferdinando Ametrano
 Copyright (C) 2007 Giorgio Facchinetti
 Copyright (C) 2007 Cristina Duminuco
 Copyright (C) 2007 StatPro Italia srl
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

/*! \file iborcoupon.hpp
    \brief Coupon paying a Libor-type index
*/

#ifndef quantlib_ibor_coupon_hpp
#define quantlib_ibor_coupon_hpp

#include <ql/cashflows/iborcouponbase.hpp>
#include <ql/cashflows/couponpricerbase2.hpp>
#include <ql/cashflows/capflooredcouponbase.hpp>

namespace QuantLib {

using boost::shared_ptr;

//! helper class building a sequence of capped/floored ibor-rate coupons
template <class T = Real> class IborLeg_t {
  public:
    IborLeg_t(const Schedule &schedule,
              const boost::shared_ptr<IborIndex_t<T> > &index);
    IborLeg_t &withNotionals(T notional);
    IborLeg_t &withNotionals(const std::vector<T> &notionals);
    IborLeg_t &withPaymentDayCounter(const DayCounter &);
    IborLeg_t &withPaymentAdjustment(BusinessDayConvention);
    IborLeg_t &withFixingDays(Natural fixingDays);
    IborLeg_t &withFixingDays(const std::vector<Natural> &fixingDays);
    IborLeg_t &withGearings(T gearing);
    IborLeg_t &withGearings(const std::vector<T> &gearings);
    IborLeg_t &withSpreads(T spread);
    IborLeg_t &withSpreads(const std::vector<T> &spreads);
    IborLeg_t &withCaps(T cap);
    IborLeg_t &withCaps(const std::vector<T> &caps);
    IborLeg_t &withFloors(T floor);
    IborLeg_t &withFloors(const std::vector<T> &floors);
    IborLeg_t &inArrears(bool flag = true);
    IborLeg_t &withZeroPayments(bool flag = true);
    operator typename Leg_t<T>::Type() const;

  private:
    Schedule schedule_;
    boost::shared_ptr<IborIndex_t<T> > index_;
    std::vector<T> notionals_;
    DayCounter paymentDayCounter_;
    BusinessDayConvention paymentAdjustment_;
    std::vector<Natural> fixingDays_;
    std::vector<T> gearings_;
    std::vector<T> spreads_;
    std::vector<T> caps_, floors_;
    bool inArrears_, zeroPayments_;
};

typedef IborLeg_t<Real> IborLeg;

// implementation

template <class T>
IborLeg_t<T>::IborLeg_t(const Schedule &schedule,
                        const shared_ptr<IborIndex_t<T> > &index)
    : schedule_(schedule), index_(index), paymentAdjustment_(Following),
      inArrears_(false), zeroPayments_(false) {}

template <class T> IborLeg_t<T> &IborLeg_t<T>::withNotionals(T notional) {
    notionals_ = std::vector<T>(1, notional);
    return *this;
}

template <class T>
IborLeg_t<T> &IborLeg_t<T>::withNotionals(const std::vector<T> &notionals) {
    notionals_ = notionals;
    return *this;
}

template <class T>
IborLeg_t<T> &
IborLeg_t<T>::withPaymentDayCounter(const DayCounter &dayCounter) {
    paymentDayCounter_ = dayCounter;
    return *this;
}

template <class T>
IborLeg_t<T> &
IborLeg_t<T>::withPaymentAdjustment(BusinessDayConvention convention) {
    paymentAdjustment_ = convention;
    return *this;
}

template <class T>
IborLeg_t<T> &IborLeg_t<T>::withFixingDays(Natural fixingDays) {
    fixingDays_ = std::vector<Natural>(1, fixingDays);
    return *this;
}

template <class T>
IborLeg_t<T> &
IborLeg_t<T>::withFixingDays(const std::vector<Natural> &fixingDays) {
    fixingDays_ = fixingDays;
    return *this;
}

template <class T> IborLeg_t<T> &IborLeg_t<T>::withGearings(T gearing) {
    gearings_ = std::vector<Real>(1, gearing);
    return *this;
}

template <class T>
IborLeg_t<T> &IborLeg_t<T>::withGearings(const std::vector<T> &gearings) {
    gearings_ = gearings;
    return *this;
}

template <class T> IborLeg_t<T> &IborLeg_t<T>::withSpreads(T spread) {
    spreads_ = std::vector<T>(1, spread);
    return *this;
}

template <class T>
IborLeg_t<T> &IborLeg_t<T>::withSpreads(const std::vector<T> &spreads) {
    spreads_ = spreads;
    return *this;
}

template <class T> IborLeg_t<T> &IborLeg_t<T>::withCaps(T cap) {
    caps_ = std::vector<T>(1, cap);
    return *this;
}

template <class T>
IborLeg_t<T> &IborLeg_t<T>::withCaps(const std::vector<T> &caps) {
    caps_ = caps;
    return *this;
}

template <class T> IborLeg_t<T> &IborLeg_t<T>::withFloors(T floor) {
    floors_ = std::vector<T>(1, floor);
    return *this;
}

template <class T>
IborLeg_t<T> &IborLeg_t<T>::withFloors(const std::vector<T> &floors) {
    floors_ = floors;
    return *this;
}

template <class T> IborLeg_t<T> &IborLeg_t<T>::inArrears(bool flag) {
    inArrears_ = flag;
    return *this;
}

template <class T> IborLeg_t<T> &IborLeg_t<T>::withZeroPayments(bool flag) {
    zeroPayments_ = flag;
    return *this;
}

template <class T> IborLeg_t<T>::operator typename Leg_t<T>::Type() const {

    typename Leg_t<T>::Type leg =
        FloatingLeg<IborIndex_t, IborCoupon_t, CappedFlooredIborCoupon_t, T>(
            schedule_, notionals_, index_, paymentDayCounter_,
            paymentAdjustment_, fixingDays_, gearings_, spreads_, caps_,
            floors_, inArrears_, zeroPayments_);

    if (caps_.empty() && floors_.empty() && !inArrears_) {
        shared_ptr<IborCouponPricer_t<T> > pricer(
            new BlackIborCouponPricer_t<T>);
        for (typename Leg_t<T>::Type::iterator i = leg.begin(); i != leg.end();
             ++i)
            boost::dynamic_pointer_cast<IborCoupon_t<T> >(*i)
                ->setPricer(pricer);
    }

    return leg;
}

} // namespace QuantLib

#endif

/*
 Copyright (C) 2006 Giorgio Facchinetti
 Copyright (C) 2006 Mario Pucci
 Copyright (C) 2006, 2007 StatPro Italia srl
 Copyright (C) 2015 Peter Caspers

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it
 under the terms of the QuantLib license.  You should have received a
 copy of the license along with this program; if not, please email
 <quantlib-dev@lists.sf.net>. The license is also available online at
 <http://quantlib.org/license.shtml>.


 This program is distributed in the hope that it will be useful, but
 WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 or FITNESS FOR A PARTICULAR PURPOSE. See the license for more details. */

/*! \file cmscoupon.hpp
    \brief CMS coupon
*/

#ifndef quantlib_cms_coupon_hpp
#define quantlib_cms_coupon_hpp

#include <ql/cashflows/cmscouponbase.hpp>
#include <ql/cashflows/capflooredcoupon.hpp>

namespace QuantLib {

//! helper class building a sequence of capped/floored cms-rate coupons
template <class T> class CmsLeg_t {
  public:
    CmsLeg_t(const Schedule &schedule,
             const boost::shared_ptr<SwapIndex_t<T> > &swapIndex);
    CmsLeg_t &withNotionals(T notional);
    CmsLeg_t &withNotionals(const std::vector<T> &notionals);
    CmsLeg_t &withPaymentDayCounter(const DayCounter &);
    CmsLeg_t &withPaymentAdjustment(BusinessDayConvention);
    CmsLeg_t &withFixingDays(Natural fixingDays);
    CmsLeg_t &withFixingDays(const std::vector<Natural> &fixingDays);
    CmsLeg_t &withGearings(T gearing);
    CmsLeg_t &withGearings(const std::vector<T> &gearings);
    CmsLeg_t &withSpreads(T spread);
    CmsLeg_t &withSpreads(const std::vector<T> &spreads);
    CmsLeg_t &withCaps(T cap);
    CmsLeg_t &withCaps(const std::vector<T> &caps);
    CmsLeg_t &withFloors(T floor);
    CmsLeg_t &withFloors(const std::vector<T> &floors);
    CmsLeg_t &inArrears(bool flag = true);
    CmsLeg_t &withZeroPayments(bool flag = true);
    operator Leg() const;

  private:
    Schedule schedule_;
    boost::shared_ptr<SwapIndex_t<T> > swapIndex_;
    std::vector<T> notionals_;
    DayCounter paymentDayCounter_;
    BusinessDayConvention paymentAdjustment_;
    std::vector<Natural> fixingDays_;
    std::vector<T> gearings_;
    std::vector<T> spreads_;
    std::vector<T> caps_, floors_;
    bool inArrears_, zeroPayments_;
};

typedef CmsLeg_t<Real> CmsLeg;

// implementation

template <class T>
CmsLeg_t<T>::CmsLeg_t(const Schedule &schedule,
                      const boost::shared_ptr<SwapIndex_t<T> > &swapIndex)
    : schedule_(schedule), swapIndex_(swapIndex), paymentAdjustment_(Following),
      inArrears_(false), zeroPayments_(false) {}

template <class T> CmsLeg_t<T> &CmsLeg_t<T>::withNotionals(T notional) {
    notionals_ = std::vector<T>(1, notional);
    return *this;
}

template <class T>
CmsLeg_t<T> &CmsLeg_t<T>::withNotionals(const std::vector<T> &notionals) {
    notionals_ = notionals;
    return *this;
}

template <class T>
CmsLeg_t<T> &CmsLeg_t<T>::withPaymentDayCounter(const DayCounter &dayCounter) {
    paymentDayCounter_ = dayCounter;
    return *this;
}

template <class T>
CmsLeg_t<T> &
CmsLeg_t<T>::withPaymentAdjustment(BusinessDayConvention convention) {
    paymentAdjustment_ = convention;
    return *this;
}

template <class T>
CmsLeg_t<T> &CmsLeg_t<T>::withFixingDays(Natural fixingDays) {
    fixingDays_ = std::vector<Natural>(1, fixingDays);
    return *this;
}

template <class T>
CmsLeg_t<T> &
CmsLeg_t<T>::withFixingDays(const std::vector<Natural> &fixingDays) {
    fixingDays_ = fixingDays;
    return *this;
}

template <class T> CmsLeg_t<T> &CmsLeg_t<T>::withGearings(T gearing) {
    gearings_ = std::vector<T>(1, gearing);
    return *this;
}

template <class T>
CmsLeg_t<T> &CmsLeg_t<T>::withGearings(const std::vector<T> &gearings) {
    gearings_ = gearings;
    return *this;
}

template <class T> CmsLeg_t<T> &CmsLeg_t<T>::withSpreads(T spread) {
    spreads_ = std::vector<T>(1, spread);
    return *this;
}

template <class T>
CmsLeg_t<T> &CmsLeg_t<T>::withSpreads(const std::vector<T> &spreads) {
    spreads_ = spreads;
    return *this;
}

template <class T> CmsLeg_t<T> &CmsLeg_t<T>::withCaps(T cap) {
    caps_ = std::vector<T>(1, cap);
    return *this;
}

template <class T>
CmsLeg_t<T> &CmsLeg_t<T>::withCaps(const std::vector<T> &caps) {
    caps_ = caps;
    return *this;
}

template <class T> CmsLeg_t<T> &CmsLeg_t<T>::withFloors(T floor) {
    floors_ = std::vector<T>(1, floor);
    return *this;
}

template <class T>
CmsLeg_t<T> &CmsLeg_t<T>::withFloors(const std::vector<T> &floors) {
    floors_ = floors;
    return *this;
}

template <class T> CmsLeg_t<T> &CmsLeg_t<T>::inArrears(bool flag) {
    inArrears_ = flag;
    return *this;
}

template <class T> CmsLeg_t<T> &CmsLeg_t<T>::withZeroPayments(bool flag) {
    zeroPayments_ = flag;
    return *this;
}

template <class T> CmsLeg_t<T>::operator Leg() const {
    return FloatingLeg<SwapIndex_t, CmsCoupon_t, CappedFlooredCmsCoupon_t>(
        schedule_, notionals_, swapIndex_, paymentDayCounter_,
        paymentAdjustment_, fixingDays_, gearings_, spreads_, caps_, floors_,
        inArrears_, zeroPayments_);
}
}

#endif

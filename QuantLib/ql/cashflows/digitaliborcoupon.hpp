/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2007 Cristina Duminuco
 Copyright (C) 2007 Giorgio Facchinetti
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

/*! \file digitaliborcoupon.hpp
    \brief Ibor-rate coupon with digital call/put option
*/

#ifndef quantlib_digital_ibor_coupon_hpp
#define quantlib_digital_ibor_coupon_hpp

#include <ql/cashflows/digitalcoupon.hpp>
#include <ql/cashflows/iborcouponbase.hpp>
#include <ql/time/schedule.hpp>

namespace QuantLib {

//! Ibor rate coupon with digital digital call/put option
template <class T> class DigitalIborCoupon_t : public DigitalCoupon_t<T> {
  public:
    DigitalIborCoupon_t(
        const boost::shared_ptr<IborCoupon_t<T> > &underlying,
        T callStrike = Null<Rate>(),
        Position::Type callPosition = Position::Long,
        bool isCallATMIncluded = false, T callDigitalPayoff = Null<Rate>(),
        T putStrike = Null<Rate>(), Position::Type putPosition = Position::Long,
        bool isPutATMIncluded = false, T putDigitalPayoff = Null<Rate>(),
        const boost::shared_ptr<DigitalReplication> &replication =
            boost::shared_ptr<DigitalReplication>());

    //! \name Visitability
    //@{
    virtual void accept(AcyclicVisitor &);
    //@}
};

//! helper class building a sequence of digital ibor-rate coupons
template <class T> class DigitalIborLeg_t {
  public:
    DigitalIborLeg_t(const Schedule &schedule,
                     const boost::shared_ptr<IborIndex_t<T> > &index);
    DigitalIborLeg_t &withNotionals(T notional);
    DigitalIborLeg_t &withNotionals(const std::vector<T> &notionals);
    DigitalIborLeg_t &withPaymentDayCounter(const DayCounter &);
    DigitalIborLeg_t &withPaymentAdjustment(BusinessDayConvention);
    DigitalIborLeg_t &withFixingDays(Natural fixingDays);
    DigitalIborLeg_t &withFixingDays(const std::vector<Natural> &fixingDays);
    DigitalIborLeg_t &withGearings(T gearing);
    DigitalIborLeg_t &withGearings(const std::vector<T> &gearings);
    DigitalIborLeg_t &withSpreads(T spread);
    DigitalIborLeg_t &withSpreads(const std::vector<T> &spreads);
    DigitalIborLeg_t &inArrears(bool flag = true);
    DigitalIborLeg_t &withCallStrikes(T strike);
    DigitalIborLeg_t &withCallStrikes(const std::vector<T> &strikes);
    DigitalIborLeg_t &withLongCallOption(Position::Type);
    DigitalIborLeg_t &withCallATM(bool flag = true);
    DigitalIborLeg_t &withCallPayoffs(T payoff);
    DigitalIborLeg_t &withCallPayoffs(const std::vector<T> &payoffs);
    DigitalIborLeg_t &withPutStrikes(T strike);
    DigitalIborLeg_t &withPutStrikes(const std::vector<T> &strikes);
    DigitalIborLeg_t &withLongPutOption(Position::Type);
    DigitalIborLeg_t &withPutATM(bool flag = true);
    DigitalIborLeg_t &withPutPayoffs(T payoff);
    DigitalIborLeg_t &withPutPayoffs(const std::vector<T> &payoffs);
    DigitalIborLeg_t &
    withReplication(const boost::shared_ptr<DigitalReplication> &replication =
                        boost::shared_ptr<DigitalReplication>());
    operator Leg() const;

  private:
    Schedule schedule_;
    boost::shared_ptr<IborIndex_t<T> > index_;
    std::vector<T> notionals_;
    DayCounter paymentDayCounter_;
    BusinessDayConvention paymentAdjustment_;
    std::vector<Natural> fixingDays_;
    std::vector<T> gearings_;
    std::vector<T> spreads_;
    bool inArrears_;
    std::vector<T> callStrikes_, callPayoffs_;
    Position::Type longCallOption_;
    bool callATM_;
    std::vector<T> putStrikes_, putPayoffs_;
    Position::Type longPutOption_;
    bool putATM_;
    boost::shared_ptr<DigitalReplication> replication_;
};

// implementation

template <class T>
DigitalIborCoupon_t<T>::DigitalIborCoupon_t(
    const boost::shared_ptr<IborCoupon_t<T> > &underlying, T callStrike,
    Position::Type callPosition, bool isCallATMIncluded, T callDigitalPayoff,
    T putStrike, Position::Type putPosition, bool isPutATMIncluded,
    T putDigitalPayoff,
    const boost::shared_ptr<DigitalReplication> &replication)
    : DigitalCoupon_t<T>(underlying, callStrike, callPosition,
                         isCallATMIncluded, callDigitalPayoff, putStrike,
                         putPosition, isPutATMIncluded, putDigitalPayoff,
                         replication) {}

template <class T> void DigitalIborCoupon_t<T>::accept(AcyclicVisitor &v) {
    typedef DigitalCoupon_t<T> super;
    Visitor<DigitalIborCoupon_t<T> > *v1 =
        dynamic_cast<Visitor<DigitalIborCoupon_t<T> > *>(&v);
    if (v1 != 0)
        v1->visit(*this);
    else
        super::accept(v);
}

template <class T>
DigitalIborLeg_t<T>::DigitalIborLeg_t(
    const Schedule &schedule, const boost::shared_ptr<IborIndex_t<T> > &index)
    : schedule_(schedule), index_(index), paymentAdjustment_(Following),
      inArrears_(false), longCallOption_(Position::Long), callATM_(false),
      longPutOption_(Position::Long), putATM_(false) {}

template <class T>
DigitalIborLeg_t<T> &DigitalIborLeg_t<T>::withNotionals(T notional) {
    notionals_ = std::vector<T>(1, notional);
    return *this;
}

template <class T>
DigitalIborLeg_t<T> &
DigitalIborLeg_t<T>::withNotionals(const std::vector<T> &notionals) {
    notionals_ = notionals;
    return *this;
}

template <class T>
DigitalIborLeg_t<T> &
DigitalIborLeg_t<T>::withPaymentDayCounter(const DayCounter &dayCounter) {
    paymentDayCounter_ = dayCounter;
    return *this;
}

template <class T>
DigitalIborLeg_t<T> &
DigitalIborLeg_t<T>::withPaymentAdjustment(BusinessDayConvention convention) {
    paymentAdjustment_ = convention;
    return *this;
}

template <class T>
DigitalIborLeg_t<T> &DigitalIborLeg_t<T>::withFixingDays(Natural fixingDays) {
    fixingDays_ = std::vector<Natural>(1, fixingDays);
    return *this;
}

template <class T>
DigitalIborLeg_t<T> &
DigitalIborLeg_t<T>::withFixingDays(const std::vector<Natural> &fixingDays) {
    fixingDays_ = fixingDays;
    return *this;
}

template <class T>
DigitalIborLeg_t<T> &DigitalIborLeg_t<T>::withGearings(T gearing) {
    gearings_ = std::vector<T>(1, gearing);
    return *this;
}

template <class T>
DigitalIborLeg_t<T> &
DigitalIborLeg_t<T>::withGearings(const std::vector<T> &gearings) {
    gearings_ = gearings;
    return *this;
}

template <class T>
DigitalIborLeg_t<T> &DigitalIborLeg_t<T>::withSpreads(T spread) {
    spreads_ = std::vector<T>(1, spread);
    return *this;
}

template <class T>
DigitalIborLeg_t<T> &
DigitalIborLeg_t<T>::withSpreads(const std::vector<T> &spreads) {
    spreads_ = spreads;
    return *this;
}

template <class T>
DigitalIborLeg_t<T> &DigitalIborLeg_t<T>::inArrears(bool flag) {
    inArrears_ = flag;
    return *this;
}

template <class T>
DigitalIborLeg_t<T> &DigitalIborLeg_t<T>::withCallStrikes(T strike) {
    callStrikes_ = std::vector<T>(1, strike);
    return *this;
}

template <class T>
DigitalIborLeg_t<T> &
DigitalIborLeg_t<T>::withCallStrikes(const std::vector<T> &strikes) {
    callStrikes_ = strikes;
    return *this;
}

template <class T>
DigitalIborLeg_t<T> &
DigitalIborLeg_t<T>::withLongCallOption(Position::Type type) {
    longCallOption_ = type;
    return *this;
}

template <class T>
DigitalIborLeg_t<T> &DigitalIborLeg_t<T>::withCallATM(bool flag) {
    callATM_ = flag;
    return *this;
}

template <class T>
DigitalIborLeg_t<T> &DigitalIborLeg_t<T>::withCallPayoffs(T payoff) {
    callPayoffs_ = std::vector<T>(1, payoff);
    return *this;
}

template <class T>
DigitalIborLeg_t<T> &
DigitalIborLeg_t<T>::withCallPayoffs(const std::vector<T> &payoffs) {
    callPayoffs_ = payoffs;
    return *this;
}

template <class T>
DigitalIborLeg_t<T> &DigitalIborLeg_t<T>::withPutStrikes(T strike) {
    putStrikes_ = std::vector<T>(1, strike);
    return *this;
}

template <class T>
DigitalIborLeg_t<T> &
DigitalIborLeg_t<T>::withPutStrikes(const std::vector<T> &strikes) {
    putStrikes_ = strikes;
    return *this;
}

template <class T>
DigitalIborLeg_t<T> &
DigitalIborLeg_t<T>::withLongPutOption(Position::Type type) {
    longPutOption_ = type;
    return *this;
}

template <class T>
DigitalIborLeg_t<T> &DigitalIborLeg_t<T>::withPutATM(bool flag) {
    putATM_ = flag;
    return *this;
}

template <class T>
DigitalIborLeg_t<T> &DigitalIborLeg_t<T>::withPutPayoffs(T payoff) {
    putPayoffs_ = std::vector<T>(1, payoff);
    return *this;
}

template <class T>
DigitalIborLeg_t<T> &
DigitalIborLeg_t<T>::withPutPayoffs(const std::vector<T> &payoffs) {
    putPayoffs_ = payoffs;
    return *this;
}

template <class T>
DigitalIborLeg_t<T> &DigitalIborLeg_t<T>::withReplication(
    const boost::shared_ptr<DigitalReplication> &replication) {
    replication_ = replication;
    return *this;
}

template <class T> DigitalIborLeg_t<T>::operator Leg() const {
    return FloatingDigitalLeg<IborIndex_t, IborCoupon_t, DigitalIborCoupon_t>(
        schedule_, notionals_, index_, paymentDayCounter_, paymentAdjustment_,
        fixingDays_, gearings_, spreads_, inArrears_, callStrikes_,
        longCallOption_, callATM_, callPayoffs_, putStrikes_, longPutOption_,
        putATM_, putPayoffs_, replication_);
}
}

#endif

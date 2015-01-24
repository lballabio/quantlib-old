/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl
 Copyright (C) 2003, 2004, 2007 StatPro Italia srl
 Copyright (C) 2007 Piter Dias
 Copyright (C) 2010 Ferdinando Ametrano
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

/*! \file fixedratecoupon.hpp
    \brief Coupon paying a fixed annual rate
*/

#ifndef quantlib_fixed_rate_coupon_hpp
#define quantlib_fixed_rate_coupon_hpp

#include <ql/cashflows/coupon.hpp>
#include <ql/patterns/visitor.hpp>
#include <ql/interestrate.hpp>
#include <ql/time/daycounter.hpp>
#include <ql/time/schedule.hpp>

using boost::shared_ptr;
using std::vector;

namespace QuantLib {

//! %Coupon paying a fixed interest rate
template <class T> class FixedRateCoupon_t : public Coupon_t<T> {
  public:
    //! \name constructors
    //@{
    FixedRateCoupon_t(const Date &paymentDate, T nominal, T rate,
                      const DayCounter &dayCounter,
                      const Date &accrualStartDate, const Date &accrualEndDate,
                      const Date &refPeriodStart = Date(),
                      const Date &refPeriodEnd = Date(),
                      const Date &exCouponDate = Date());
    FixedRateCoupon_t(const Date &paymentDate, T nominal,
                      const InterestRate_t<T> &interestRate,
                      const Date &accrualStartDate, const Date &accrualEndDate,
                      const Date &refPeriodStart = Date(),
                      const Date &refPeriodEnd = Date(),
                      const Date &exCouponDate = Date());
    //@}
    //! \name CashFlow interface
    //@{
    T amount() const;
    //@}
    //! \name Coupon interface
    //@{
    T rate() const { return rate_; }
    InterestRate_t<T> interestRate() const { return rate_; }
    DayCounter dayCounter() const { return rate_.dayCounter(); }
    T accruedAmount(const Date &) const;
    //@}
    //! \name Visitability
    //@{
    virtual void accept(AcyclicVisitor &);
    //@}
  private:
    InterestRate_t<T> rate_;
};

typedef FixedRateCoupon_t<Real> FixedRateCoupon;

//! helper class building a sequence of fixed rate coupons
template <class T> class FixedRateLeg_t {
  public:
    FixedRateLeg_t(const Schedule &schedule);
    FixedRateLeg_t<T> &withNotionals(T);
    FixedRateLeg_t<T> &withNotionals(const std::vector<T> &);
    FixedRateLeg_t<T> &withCouponRates(T, const DayCounter &paymentDayCounter,
                                       Compounding comp = Simple,
                                       Frequency freq = Annual);
    FixedRateLeg_t<T> &withCouponRates(const std::vector<T> &,
                                       const DayCounter &paymentDayCounter,
                                       Compounding comp = Simple,
                                       Frequency freq = Annual);
    FixedRateLeg_t<T> &withCouponRates(const InterestRate_t<T> &);
    FixedRateLeg_t<T> &withCouponRates(const std::vector<InterestRate_t<T> > &);
    FixedRateLeg_t<T> &withPaymentAdjustment(BusinessDayConvention);
    FixedRateLeg_t<T> &withFirstPeriodDayCounter(const DayCounter &);
    FixedRateLeg_t<T> &withPaymentCalendar(const Calendar &);
    FixedRateLeg_t<T> &withExCouponPeriod(const Period &, const Calendar &,
                                          BusinessDayConvention,
                                          bool endOfMonth = false);
    operator typename Leg_t<T>::Type() const;

  private:
    Schedule schedule_;
    Calendar calendar_;
    std::vector<T> notionals_;
    std::vector<InterestRate_t<T> > couponRates_;
    DayCounter firstPeriodDC_;
    BusinessDayConvention paymentAdjustment_;
    Period exCouponPeriod_;
    Calendar exCouponCalendar_;
    BusinessDayConvention exCouponAdjustment_;
    bool exCouponEndOfMonth_;
};

typedef FixedRateLeg_t<Real> FixedRateLeg;

// inline

template <class T> inline void FixedRateCoupon_t<T>::accept(AcyclicVisitor &v) {
    Visitor<FixedRateCoupon_t<T> > *v1 =
        dynamic_cast<Visitor<FixedRateCoupon_t<T> > *>(&v);
    if (v1 != 0)
        v1->visit(*this);
    else
        Coupon_t<T>::accept(v);
}

typedef FixedRateCoupon_t<Real> FixedRateCoupon;

// implementation

template <class T>
FixedRateCoupon_t<T>::FixedRateCoupon_t(const Date &paymentDate, T nominal,
                                        T rate, const DayCounter &dayCounter,
                                        const Date &accrualStartDate,
                                        const Date &accrualEndDate,
                                        const Date &refPeriodStart,
                                        const Date &refPeriodEnd,
                                        const Date &exCouponDate)
    : Coupon_t<T>(paymentDate, nominal, accrualStartDate, accrualEndDate,
                  refPeriodStart, refPeriodEnd, exCouponDate),
      rate_(InterestRate_t<T>(rate, dayCounter, Simple, Annual)) {}

template <class T>
FixedRateCoupon_t<T>::FixedRateCoupon_t(const Date &paymentDate, T nominal,
                                        const InterestRate_t<T> &interestRate,
                                        const Date &accrualStartDate,
                                        const Date &accrualEndDate,
                                        const Date &refPeriodStart,
                                        const Date &refPeriodEnd,
                                        const Date &exCouponDate)
    : Coupon_t<T>(paymentDate, nominal, accrualStartDate, accrualEndDate,
                  refPeriodStart, refPeriodEnd, exCouponDate),
      rate_(interestRate) {}

template <class T> T FixedRateCoupon_t<T>::amount() const {
    return this->nominal() *
           (rate_.compoundFactor(this->accrualStartDate_, this->accrualEndDate_,
                                 this->refPeriodStart_, this->refPeriodEnd_) -
            1.0);
}

template <class T> T FixedRateCoupon_t<T>::accruedAmount(const Date &d) const {
    if (d <= this->accrualStartDate_ || d > this->paymentDate_) {
        return 0.0;
    } else if (this->tradingExCoupon(d)) {
        return -this->nominal() *
               (rate_.compoundFactor(d, this->accrualEndDate_,
                                     this->refPeriodStart_,
                                     this->refPeriodEnd_) -
                1.0);
    } else {
        return this->nominal() *
               (rate_.compoundFactor(
                    this->accrualStartDate_, std::min(d, this->accrualEndDate_),
                    this->refPeriodStart_, this->refPeriodEnd_) -
                1.0);
    }
}

template <class T>
FixedRateLeg_t<T>::FixedRateLeg_t(const Schedule &schedule)
    : schedule_(schedule), calendar_(schedule.calendar()),
      paymentAdjustment_(Following) {}

template <class T>
FixedRateLeg_t<T> &FixedRateLeg_t<T>::withNotionals(T notional) {
    notionals_ = vector<T>(1, notional);
    return *this;
}

template <class T>
FixedRateLeg_t<T> &
FixedRateLeg_t<T>::withNotionals(const vector<T> &notionals) {
    notionals_ = notionals;
    return *this;
}

template <class T>
FixedRateLeg_t<T> &
FixedRateLeg_t<T>::withCouponRates(T rate, const DayCounter &dc,
                                   Compounding comp, Frequency freq) {
    couponRates_.resize(1);
    couponRates_[0] = InterestRate_t<T>(rate, dc, comp, freq);
    return *this;
}

template <class T>
FixedRateLeg_t<T> &
FixedRateLeg_t<T>::withCouponRates(const InterestRate_t<T> &i) {
    couponRates_.resize(1);
    couponRates_[0] = i;
    return *this;
}

template <class T>
FixedRateLeg_t<T> &
FixedRateLeg_t<T>::withCouponRates(const vector<T> &rates, const DayCounter &dc,
                                   Compounding comp, Frequency freq) {
    couponRates_.resize(rates.size());
    for (Size i = 0; i < rates.size(); ++i)
        couponRates_[i] = InterestRate_t<T>(rates[i], dc, comp, freq);
    return *this;
}

template <class T>
FixedRateLeg_t<T> &FixedRateLeg_t<T>::withCouponRates(
    const vector<InterestRate_t<T> > &interestRates) {
    couponRates_ = interestRates;
    return *this;
}

template <class T>
FixedRateLeg_t<T> &
FixedRateLeg_t<T>::withPaymentAdjustment(BusinessDayConvention convention) {
    paymentAdjustment_ = convention;
    return *this;
}

template <class T>
FixedRateLeg_t<T> &
FixedRateLeg_t<T>::withFirstPeriodDayCounter(const DayCounter &dayCounter) {
    firstPeriodDC_ = dayCounter;
    return *this;
}

template <class T>
FixedRateLeg_t<T> &FixedRateLeg_t<T>::withPaymentCalendar(const Calendar &cal) {
    calendar_ = cal;
    return *this;
}

template <class T>
FixedRateLeg_t<T> &
FixedRateLeg_t<T>::withExCouponPeriod(const Period &period, const Calendar &cal,
                                      BusinessDayConvention convention,
                                      bool endOfMonth) {
    exCouponPeriod_ = period;
    exCouponCalendar_ = cal;
    exCouponAdjustment_ = convention;
    exCouponEndOfMonth_ = endOfMonth;
    return *this;
}

template <class T> FixedRateLeg_t<T>::operator typename Leg_t<T>::Type() const {

    QL_REQUIRE(!couponRates_.empty(), "no coupon rates given");
    QL_REQUIRE(!notionals_.empty(), "no notional given");

    typename Leg_t<T>::Type leg;
    leg.reserve(schedule_.size() - 1);

    Calendar schCalendar = schedule_.calendar();

    // first period might be short or long
    Date start = schedule_.date(0), end = schedule_.date(1);
    Date paymentDate = calendar_.adjust(end, paymentAdjustment_);
    Date exCouponDate;
    InterestRate_t<T> rate = couponRates_[0];
    T nominal = notionals_[0];

    if (exCouponPeriod_ != Period()) {
        exCouponDate =
            exCouponCalendar_.advance(paymentDate, -exCouponPeriod_,
                                      exCouponAdjustment_, exCouponEndOfMonth_);
    }

    if (schedule_.isRegular(1)) {
        QL_REQUIRE(firstPeriodDC_.empty() ||
                       firstPeriodDC_ == rate.dayCounter(),
                   "regular first coupon "
                   "does not allow a first-period day count");
        shared_ptr<CashFlow_t<T> > temp(new FixedRateCoupon_t<T>(
            paymentDate, nominal, rate, start, end, start, end, exCouponDate));
        leg.push_back(temp);
    } else {
        Date ref = end - schedule_.tenor();
        ref = schCalendar.adjust(ref, schedule_.businessDayConvention());
        InterestRate_t<T> r(rate.rate(),
                            firstPeriodDC_.empty() ? rate.dayCounter()
                                                   : firstPeriodDC_,
                            rate.compounding(), rate.frequency());
        leg.push_back(shared_ptr<CashFlow_t<T> >(new FixedRateCoupon_t<T>(
            paymentDate, nominal, r, start, end, ref, end, exCouponDate)));
    }
    // regular periods
    for (Size i = 2; i < schedule_.size() - 1; ++i) {
        start = end;
        end = schedule_.date(i);
        paymentDate = calendar_.adjust(end, paymentAdjustment_);
        if (exCouponPeriod_ != Period()) {
            exCouponDate = exCouponCalendar_.advance(
                paymentDate, -exCouponPeriod_, exCouponAdjustment_,
                exCouponEndOfMonth_);
        }
        if ((i - 1) < couponRates_.size())
            rate = couponRates_[i - 1];
        else
            rate = couponRates_.back();
        if ((i - 1) < notionals_.size())
            nominal = notionals_[i - 1];
        else
            nominal = notionals_.back();
        leg.push_back(shared_ptr<CashFlow_t<T> >(new FixedRateCoupon_t<T>(
            paymentDate, nominal, rate, start, end, start, end, exCouponDate)));
    }
    if (schedule_.size() > 2) {
        // last period might be short or long
        Size N = schedule_.size();
        start = end;
        end = schedule_.date(N - 1);
        paymentDate = calendar_.adjust(end, paymentAdjustment_);
        if (exCouponPeriod_ != Period()) {
            exCouponDate = exCouponCalendar_.advance(
                paymentDate, -exCouponPeriod_, exCouponAdjustment_,
                exCouponEndOfMonth_);
        }
        if ((N - 2) < couponRates_.size())
            rate = couponRates_[N - 2];
        else
            rate = couponRates_.back();
        if ((N - 2) < notionals_.size())
            nominal = notionals_[N - 2];
        else
            nominal = notionals_.back();
        if (schedule_.isRegular(N - 1)) {
            leg.push_back(shared_ptr<CashFlow_t<T> >(
                new FixedRateCoupon_t<T>(paymentDate, nominal, rate, start, end,
                                         start, end, exCouponDate)));
        } else {
            Date ref = start + schedule_.tenor();
            ref = schCalendar.adjust(ref, schedule_.businessDayConvention());
            leg.push_back(shared_ptr<CashFlow_t<T> >(
                new FixedRateCoupon_t<T>(paymentDate, nominal, rate, start, end,
                                         start, ref, exCouponDate)));
        }
    }
    return leg;
}
}

#endif

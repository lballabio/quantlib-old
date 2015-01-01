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

#ifndef quantlib_ibor_coupon_base_hpp
#define quantlib_ibor_coupon_base_hpp

#include <ql/cashflows/cashflowvectors.hpp>
#include <ql/cashflows/floatingratecoupon.hpp>
#include <ql/indexes/iborindex.hpp>
#include <ql/time/schedule.hpp>

namespace QuantLib {

using boost::shared_ptr;

//! %Coupon paying a Libor-type index
template <class T = Real> class IborCoupon_t : public FloatingRateCoupon_t<T> {
  public:
    IborCoupon_t(const Date &paymentDate, T nominal, const Date &startDate,
                 const Date &endDate, Natural fixingDays,
                 const boost::shared_ptr<IborIndex_t<T> > &index,
                 T gearing = 1.0, T spread = 0.0,
                 const Date &refPeriodStart = Date(),
                 const Date &refPeriodEnd = Date(),
                 const DayCounter &dayCounter = DayCounter(),
                 bool isInArrears = false);
    //! \name Inspectors
    //@{
    const boost::shared_ptr<IborIndex> &iborIndex() const { return iborIndex_; }
    //@}
    //! \name FloatingRateCoupon interface
    //@{
    //! Implemented in order to manage the case of par coupon
    T indexFixing() const;
    //@}
    //! \name Visitability
    //@{
    virtual void accept(AcyclicVisitor &);
    //@}
  private:
    boost::shared_ptr<IborIndex_t<T> > iborIndex_;
    Date fixingDate_, fixingValueDate_, fixingEndDate_;
    Time spanningTime_;
};

typedef IborCoupon_t<Real> IborCoupon;

// implementation

template <class T>
IborCoupon_t<T>::IborCoupon_t(const Date &paymentDate, T nominal,
                              const Date &startDate, const Date &endDate,
                              Natural fixingDays,
                              const shared_ptr<IborIndex_t<T> > &iborIndex,
                              T gearing, T spread, const Date &refPeriodStart,
                              const Date &refPeriodEnd,
                              const DayCounter &dayCounter, bool isInArrears)
    : FloatingRateCoupon_t<T>(paymentDate, nominal, startDate, endDate,
                              fixingDays, iborIndex, gearing, spread,
                              refPeriodStart, refPeriodEnd, dayCounter,
                              isInArrears),
      iborIndex_(iborIndex) {

    fixingDate_ = this->fixingDate();

    const Calendar &fixingCalendar = this->index_->fixingCalendar();
    Natural indexFixingDays = this->index_->fixingDays();

    fixingValueDate_ =
        fixingCalendar.advance(fixingDate_, indexFixingDays, Days);

#ifdef QL_USE_INDEXED_COUPON
    fixingEndDate_ = this->index_->maturityDate(fixingValueDate_);
#else
    if (this->isInArrears_)
        fixingEndDate_ = this->index_->maturityDate(fixingValueDate_);
    else { // par coupon approximation
        Date nextFixingDate = fixingCalendar.advance(
            this->accrualEndDate_, -static_cast<Integer>(this->fixingDays_),
            Days);
        fixingEndDate_ =
            fixingCalendar.advance(nextFixingDate, indexFixingDays, Days);
    }
#endif

    const DayCounter &dc = this->index_->dayCounter();
    spanningTime_ = dc.yearFraction(fixingValueDate_, fixingEndDate_);
    QL_REQUIRE(spanningTime_ > 0.0,
               "\n cannot calculate forward rate between "
                   << fixingValueDate_ << " and " << fixingEndDate_
                   << ":\n non positive time (" << spanningTime_ << ") using "
                   << dc.name() << " daycounter");
}

template <class T> T IborCoupon_t<T>::indexFixing() const {

    /* instead of just returning index_->fixing(fixingValueDate_)
       its logic is duplicated here using a specialized iborIndex
       forecastFixing overload which
       1) allows to save date/time recalculations, and
       2) takes into account par coupon needs
    */
    Date today = Settings::instance().evaluationDate();

    if (fixingDate_ > today)
        return iborIndex_->forecastFixing(fixingValueDate_, fixingEndDate_,
                                          spanningTime_);

    if (fixingDate_ < today ||
        Settings::instance().enforcesTodaysHistoricFixings()) {
        // do not catch exceptions
        T result = this->index_->pastFixing(fixingDate_);
        QL_REQUIRE(result != Null<Real>(), "Missing " << this->index_->name()
                                                      << " fixing for "
                                                      << fixingDate_);
        return result;
    }

    try {
        T result = this->index_->pastFixing(fixingDate_);
        if (result != Null<Real>())
            return result;
        else
            ; // fall through and forecast
    } catch (Error &) {
        ; // fall through and forecast
    }
    return iborIndex_->forecastFixing(fixingValueDate_, fixingEndDate_,
                                      spanningTime_);
}

template <class T> void IborCoupon_t<T>::accept(AcyclicVisitor &v) {
    Visitor<IborCoupon_t<T> > *v1 =
        dynamic_cast<Visitor<IborCoupon_t<T> > *>(&v);
    if (v1 != 0)
        v1->visit(*this);
    else
        FloatingRateCoupon_t<T>::accept(v);
}

} // namespace QuantLib

#endif

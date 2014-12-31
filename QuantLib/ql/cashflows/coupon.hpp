/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl
 Copyright (C) 2003, 2004, 2007 StatPro Italia srl
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

/*! \file coupon.hpp
    \brief Coupon accruing over a fixed period
*/

#ifndef quantlib_coupon_hpp
#define quantlib_coupon_hpp

#include <ql/cashflow.hpp>
#include <ql/patterns/visitor.hpp>
#include <ql/time/daycounter.hpp>

namespace QuantLib {

class DayCounter;

//! %coupon accruing over a fixed period
/*! This class implements part of the CashFlow interface but it is
    still abstract and provides derived classes with methods for
    accrual period calculations.
*/
template <class T> class Coupon_t : public CashFlow_t<T> {
  public:
    /*! \warning the coupon does not adjust the payment date which
                 must already be a business day.
    */
    Coupon_t(const Date &paymentDate, Real nominal, const Date &accrualStartDate,
           const Date &accrualEndDate, const Date &refPeriodStart = Date(),
           const Date &refPeriodEnd = Date(),
           const Date &exCouponDate = Date());
    //! \name Event interface
    //@{
    Date date() const { return paymentDate_; }
    //@}
    //! \name CashFlow interface
    //@{
    Date exCouponDate() const { return exCouponDate_; }
    //@}
    //! \name Inspectors
    //@{
    T nominal() const;
    //! start of the accrual period
    const Date &accrualStartDate() const;
    //! end of the accrual period
    const Date &accrualEndDate() const;
    //! start date of the reference period
    const Date &referencePeriodStart() const;
    //! end date of the reference period
    const Date &referencePeriodEnd() const;
    //! accrual period as fraction of year
    Time accrualPeriod() const;
    //! accrual period in days
    BigInteger accrualDays() const;
    //! accrued rate
    virtual T rate() const = 0;
    //! day counter for accrual calculation
    virtual DayCounter dayCounter() const = 0;
    //! accrued period as fraction of year at the given date
    Time accruedPeriod(const Date &) const;
    //! accrued days at the given date
    BigInteger accruedDays(const Date &) const;
    //! accrued amount at the given date
    virtual T accruedAmount(const Date &) const = 0;
    //@}
    //! \name Visitability
    //@{
    virtual void accept(AcyclicVisitor &);
    //@}
  protected:
    Date paymentDate_;
    T nominal_;
    Date accrualStartDate_, accrualEndDate_, refPeriodStart_, refPeriodEnd_;
    Date exCouponDate_;
};

typedef Coupon_t<Real> Coupon;

// inline definitions

template <class T> inline T Coupon_t<T>::nominal() const { return nominal_; }

template <class T> inline const Date &Coupon_t<T>::accrualStartDate() const {
    return accrualStartDate_;
}

template <class T> inline const Date &Coupon_t<T>::accrualEndDate() const {
    return accrualEndDate_;
}

template <class T>
inline const Date &Coupon_t<T>::referencePeriodStart() const {
    return refPeriodStart_;
}

template <class T> inline const Date &Coupon_t<T>::referencePeriodEnd() const {
    return refPeriodEnd_;
}

// implementation

template <class T>
Coupon_t<T>::Coupon_t(const Date &paymentDate, Real nominal,
                      const Date &accrualStartDate, const Date &accrualEndDate,
                      const Date &refPeriodStart, const Date &refPeriodEnd,
                      const Date &exCouponDate)
    : paymentDate_(paymentDate), nominal_(nominal),
      accrualStartDate_(accrualStartDate), accrualEndDate_(accrualEndDate),
      refPeriodStart_(refPeriodStart), refPeriodEnd_(refPeriodEnd),
      exCouponDate_(exCouponDate) {
    if (refPeriodStart_ == Date())
        refPeriodStart_ = accrualStartDate_;
    if (refPeriodEnd_ == Date())
        refPeriodEnd_ = accrualEndDate_;
}

template <class T> Time Coupon_t<T>::accrualPeriod() const {
    return dayCounter().yearFraction(accrualStartDate_, accrualEndDate_,
                                     refPeriodStart_, refPeriodEnd_);
}

template <class T> BigInteger Coupon_t<T>::accrualDays() const {
    return dayCounter().dayCount(accrualStartDate_, accrualEndDate_);
}

template <class T> Time Coupon_t<T>::accruedPeriod(const Date &d) const {
    if (d <= accrualStartDate_ || d > paymentDate_) {
        return 0.0;
    } else {
        return dayCounter().yearFraction(accrualStartDate_,
                                         std::min(d, accrualEndDate_),
                                         refPeriodStart_, refPeriodEnd_);
    }
}

template <class T> BigInteger Coupon_t<T>::accruedDays(const Date &d) const {
    if (d <= accrualStartDate_ || d > paymentDate_) {
        return 0;
    } else {
        return dayCounter().dayCount(accrualStartDate_,
                                     std::min(d, accrualEndDate_));
    }
}

template <class T> void Coupon_t<T>::accept(AcyclicVisitor &v) {
    Visitor<Coupon> *v1 = dynamic_cast<Visitor<Coupon> *>(&v);
    if (v1 != 0)
        v1->visit(*this);
    else
        CashFlow::accept(v);
}
}

#endif

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

#ifndef quantlib_cms_coupon_base_hpp
#define quantlib_cms_coupon_base_hpp

#include <ql/cashflows/floatingratecoupon.hpp>
#include <ql/time/schedule.hpp>
#include <ql/cashflows/cashflowvectors.hpp>
#include <ql/indexes/swapindex.hpp>

namespace QuantLib {

//! CMS coupon class
/*! \warning This class does not perform any date adjustment,
             i.e., the start and end date passed upon construction
             should be already rolled to a business day.
*/
template <class T> class CmsCoupon_t : public FloatingRateCoupon_t<T> {
  public:
    CmsCoupon_t(const Date &paymentDate, T nominal, const Date &startDate,
                const Date &endDate, Natural fixingDays,
                const boost::shared_ptr<SwapIndex_t<T> > &index,
                T gearing = 1.0, T spread = 0.0,
                const Date &refPeriodStart = Date(),
                const Date &refPeriodEnd = Date(),
                const DayCounter &dayCounter = DayCounter(),
                bool isInArrears = false);
    //! \name Inspectors
    //@{
    const boost::shared_ptr<SwapIndex_t<T> > &swapIndex() const {
        return swapIndex_;
    }
    //@}
    //! \name Visitability
    //@{
    virtual void accept(AcyclicVisitor &);
    //@}
  private:
    boost::shared_ptr<SwapIndex_t<T> > swapIndex_;
};

typedef CmsCoupon_t<Real> CmsCoupon;

// implementation

template <class T>
CmsCoupon_t<T>::CmsCoupon_t(const Date &paymentDate, T nominal,
                            const Date &startDate, const Date &endDate,
                            Natural fixingDays,
                            const boost::shared_ptr<SwapIndex_t<T> > &swapIndex,
                            T gearing, T spread, const Date &refPeriodStart,
                            const Date &refPeriodEnd,
                            const DayCounter &dayCounter, bool isInArrears)
    : FloatingRateCoupon_t<T>(paymentDate, nominal, startDate, endDate,
                              fixingDays, swapIndex, gearing, spread,
                              refPeriodStart, refPeriodEnd, dayCounter,
                              isInArrears),
      swapIndex_(swapIndex) {}

template <class T> void CmsCoupon_t<T>::accept(AcyclicVisitor &v) {
    Visitor<CmsCoupon_t<T> > *v1 = dynamic_cast<Visitor<CmsCoupon_t<T> > *>(&v);
    if (v1 != 0)
        v1->visit(*this);
    else
        FloatingRateCoupon_t<T>::accept(v);
}
}

#endif

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

#ifndef quantlib_capped_floored_coupon_hpp
#define quantlib_capped_floored_coupon_hpp

#include <ql/cashflows/capflooredcouponbase.hpp>
#include <ql/cashflows/cmscouponbase.hpp>

namespace QuantLib {

class Date;

template <class T>
class CappedFlooredCmsCoupon_t : public CappedFlooredCoupon_t<T> {
  public:
    CappedFlooredCmsCoupon_t(
        const Date &paymentDate, T nominal, const Date &startDate,
        const Date &endDate, Natural fixingDays,
        const boost::shared_ptr<SwapIndex_t<T> > &index, T gearing = 1.0,
        T spread = 0.0, const T cap = Null<Rate>(),
        const T floor = Null<Rate>(), const Date &refPeriodStart = Date(),
        const Date &refPeriodEnd = Date(),
        const DayCounter &dayCounter = DayCounter(), bool isInArrears = false)
        : CappedFlooredCoupon_t<T>(
              boost::shared_ptr<FloatingRateCoupon_t<T> >(new CmsCoupon_t<T>(
                  paymentDate, nominal, startDate, endDate, fixingDays, index,
                  gearing, spread, refPeriodStart, refPeriodEnd, dayCounter,
                  isInArrears)),
              cap, floor) {}

    virtual void accept(AcyclicVisitor &v) {
        Visitor<CappedFlooredCmsCoupon_t<T> > *v1 =
            dynamic_cast<Visitor<CappedFlooredCmsCoupon_t<T> > *>(&v);
        if (v1 != 0)
            v1->visit(*this);
        else
            CappedFlooredCoupon_t<T>::accept(v);
    }
};

typedef CappedFlooredCmsCoupon_t<Real> CappedFlooredCmsCoupon;

} // namesapce QuantLib

#endif

/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2007 Giorgio Facchinetti
 Copyright (C) 2007 Cristina Duminuco
 Copyright (C) 2011 Ferdinando Ametrano
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

/*! \file couponpricer.hpp
    \brief Coupon pricers
*/

#ifndef quantlib_coupon_pricer_base_hpp
#define quantlib_coupon_pricer_base_hpp

#include <ql/patterns/observable.hpp>

namespace QuantLib {

template <class T> class FloatingRateCoupon_t;

//! generic pricer for floating-rate coupons
template <class T>
class FloatingRateCouponPricer_t : public virtual Observer,
                                   public virtual Observable {
  public:
    virtual ~FloatingRateCouponPricer_t() {}
    //! \name required interface
    //@{
    virtual T swapletPrice() const = 0;
    virtual T swapletRate() const = 0;
    virtual T capletPrice(T effectiveCap) const = 0;
    virtual T capletRate(T effectiveCap) const = 0;
    virtual T floorletPrice(T effectiveFloor) const = 0;
    virtual T floorletRate(T effectiveFloor) const = 0;
    virtual void initialize(const FloatingRateCoupon_t<T> &coupon) = 0;
    //@}
    //! \name Observer interface
    //@{
    void update() { notifyObservers(); }
    //@}
};

typedef FloatingRateCouponPricer_t<Real> FloatingRateCouponPricer;

}

#endif

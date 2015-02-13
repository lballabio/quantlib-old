/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2007 Ferdinando Ametrano
 Copyright (C) 2005 StatPro Italia srl

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

/*! \file zerocouponbond.hpp
    \brief zero-coupon bond
*/

#ifndef quantlib_zero_coupon_bond_hpp
#define quantlib_zero_coupon_bond_hpp

#include <ql/instruments/bond.hpp>
#include <ql/cashflows/simplecashflow.hpp>

namespace QuantLib {

    //! zero-coupon bond
    /*! \ingroup instruments

        \test calculations are tested by checking results against
              cached values.
    */
	template<class T = Real>
    class ZeroCouponBond_t : public Bond_t<T> {
      public:
        ZeroCouponBond_t(Natural settlementDays,
                       const Calendar& calendar,
                       T faceAmount,
                       const Date& maturityDate,
                       BusinessDayConvention paymentConvention = Following,
                       T redemption = 100.0,
                       const Date& issueDate = Date());
    };

	typedef ZeroCouponBond_t<Real> ZeroCouponBond;

    // Implementation

	template<class T>
	ZeroCouponBond_t<T>::ZeroCouponBond_t(Natural settlementDays,
		const Calendar& calendar,
		T faceAmount,
		const Date& maturityDate,
		BusinessDayConvention paymentConvention,
		T redemption,
		const Date& issueDate)
		: Bond_t<T>(settlementDays, calendar, issueDate) {

			this->maturityDate_ = maturityDate;
			Date redemptionDate = this->calendar_.adjust(maturityDate,
				paymentConvention);
			this->setSingleRedemption(faceAmount, redemption, redemptionDate);
	}

}

#endif

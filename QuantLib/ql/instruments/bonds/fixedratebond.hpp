/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2004 Jeff Yu
 Copyright (C) 2004 M-Dimension Consulting Inc.
 Copyright (C) 2005 StatPro Italia srl
 Copyright (C) 2007, 2008, 2010 Ferdinando Ametrano
 Copyright (C) 2009 Piter Dias
 COpyright (C) 2013 Cheng Li, DataYes

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

/*! \file fixedratebond.hpp
    \brief fixed-rate bond
*/

#ifndef quantlib_fixed_rate_bond_hpp
#define quantlib_fixed_rate_bond_hpp

#include <ql/instruments/bond.hpp>
#include <ql/time/dategenerationrule.hpp>
#include <ql/time/daycounter.hpp>
#include <ql/time/daycounters/actual360.hpp>
#include <ql/time/calendars/china.hpp>
#include <ql/interestrate.hpp>
#include <ql/cashflows/cashflowvectors.hpp>
#include <ql/cashflows/simplecashflow.hpp>
#include <ql/time/schedule.hpp>
#include <ql/pricingengines/bond/bondfunctions.hpp>
#include <ql/math/rounding.hpp>

namespace QuantLib {

    class Schedule;

    //! fixed-rate bond
    /*! \ingroup instruments

        \test calculations are tested by checking results against
              cached values.
    */
	template<class T = Real>
    class FixedRateBond_t : public Bond_t<T> {
      public:
        //! simple annual compounding coupon rates
        FixedRateBond_t(Natural settlementDays,
                      T faceAmount,
                      const Schedule& schedule,
                      const std::vector<T>& coupons,
                      const DayCounter& accrualDayCounter,
                      BusinessDayConvention paymentConvention = Following,
                      T redemption = 100.0,
                      const Date& issueDate = Date(),
					  const Calendar& paymentCalendar = Calendar(),
					  const Period& exCouponPeriod = Period(),
					  const Calendar& exCouponCalendar = Calendar(),
					  const BusinessDayConvention exCouponConvention = Unadjusted,
					  bool exCouponEndOfMonth = false);

        /*! simple annual compounding coupon rates
            with internal schedule calculation */
        FixedRateBond_t(Natural settlementDays,
                      const Calendar& couponCalendar,
                      T faceAmount,
                      const Date& startDate,
                      const Date& maturityDate,
                      const Period& tenor,
                      const std::vector<T>& coupons,
                      const DayCounter& accrualDayCounter,
                      BusinessDayConvention accrualConvention = Following,
                      BusinessDayConvention paymentConvention = Following,
                      T redemption = 100.0,
                      const Date& issueDate = Date(),
                      const Date& stubDate = Date(),
                      DateGeneration::Rule rule = DateGeneration::Backward,
                      bool endOfMonth = false,
                      const Calendar& paymentCalendar = Calendar(),
					  const Period& exCouponPeriod = Period(),
					  const Calendar& exCouponCalendar = Calendar(),
					  const BusinessDayConvention exCouponConvention = Unadjusted,
					  bool exCouponEndOfMonth = false);
        //! generic compounding and frequency InterestRate coupons 
        FixedRateBond_t(Natural settlementDays,
                      T faceAmount,
                      const Schedule& schedule,
                      const std::vector<InterestRate_t<T> >& coupons,
                      BusinessDayConvention paymentConvention = Following,
                      T redemption = 100.0,
                      const Date& issueDate = Date(),
                      const Calendar& paymentCalendar = Calendar(),
					  const Period& exCouponPeriod = Period(),
					  const Calendar& exCouponCalendar = Calendar(),
					  const BusinessDayConvention exCouponConvention = Unadjusted,
					  bool exCouponEndOfMonth = false);
        Frequency frequency() const { return frequency_; }
        const DayCounter& dayCounter() const { return dayCounter_; }

      protected:
        Frequency frequency_;
        DayCounter dayCounter_;
    };

	typedef FixedRateBond_t<Real> FixedRateBond;

	template<class T>
	FixedRateBond_t<T>::FixedRateBond_t(Natural settlementDays,
		T faceAmount,
		const Schedule& schedule,
		const std::vector<T>& coupons,
		const DayCounter& accrualDayCounter,
		BusinessDayConvention paymentConvention,
		T redemption,
		const Date& issueDate,
		const Calendar& paymentCalendar,
		const Period& exCouponPeriod,
		const Calendar& exCouponCalendar,
		const BusinessDayConvention exCouponConvention,
		bool exCouponEndOfMonth)

		: Bond_t<T>(settlementDays,
		paymentCalendar==Calendar() ? schedule.calendar() : paymentCalendar,
		issueDate),
		frequency_(schedule.tenor().frequency()),
		dayCounter_(accrualDayCounter) {

			this->maturityDate_ = schedule.endDate();

			this->cashflows_ = FixedRateLeg_t<T>(schedule)
				.withNotionals(faceAmount)
				.withCouponRates(coupons, accrualDayCounter)
				.withPaymentCalendar(this->calendar_)
				.withPaymentAdjustment(paymentConvention)
				.withExCouponPeriod(exCouponPeriod,
				exCouponCalendar,
				exCouponConvention,
				exCouponEndOfMonth);


			this->addRedemptionsToCashflows(std::vector<T>(1, redemption));

			QL_ENSURE(!this->cashflows().empty(), "bond with no cashflows!");
			QL_ENSURE(this->redemptions_.size() == 1, "multiple redemptions created");
	}

	template<class T>
	FixedRateBond_t<T>::FixedRateBond_t(Natural settlementDays,
		const Calendar& calendar,
		T faceAmount,
		const Date& startDate,
		const Date& maturityDate,
		const Period& tenor,
		const std::vector<T>& coupons,
		const DayCounter& accrualDayCounter,
		BusinessDayConvention accrualConvention,
		BusinessDayConvention paymentConvention,
		T redemption,
		const Date& issueDate,
		const Date& stubDate,
		DateGeneration::Rule rule,
		bool endOfMonth,
		const Calendar& paymentCalendar,
		const Period& exCouponPeriod,
		const Calendar& exCouponCalendar,
		const BusinessDayConvention exCouponConvention,
		bool exCouponEndOfMonth)

		: Bond_t<T>(settlementDays,
		paymentCalendar==Calendar() ? calendar : paymentCalendar,
		issueDate),
		frequency_(tenor.frequency()), dayCounter_(accrualDayCounter) {

			this->maturityDate_ = maturityDate;

			Date firstDate, nextToLastDate;
			switch (rule) {
			case DateGeneration::Backward:
				firstDate = Date();
				nextToLastDate = stubDate;
				break;
			case DateGeneration::Forward:
				firstDate = stubDate;
				nextToLastDate = Date();
				break;
			case DateGeneration::Zero:
			case DateGeneration::ThirdWednesday:
			case DateGeneration::Twentieth:
			case DateGeneration::TwentiethIMM:
				QL_FAIL("stub date (" << stubDate << ") not allowed with " <<
					rule << " DateGeneration::Rule");
			default:
				QL_FAIL("unknown DateGeneration::Rule (" << Integer(rule) << ")");
			}

			Schedule schedule(startDate, this->maturityDate_, tenor,
				calendar, accrualConvention, accrualConvention,
				rule, endOfMonth,
				firstDate, nextToLastDate);

			this->cashflows_ = FixedRateLeg_t<T>(schedule)
				.withNotionals(faceAmount)
				.withCouponRates(coupons, accrualDayCounter)
				.withPaymentCalendar(this->calendar_)
				.withPaymentAdjustment(paymentConvention)
				.withExCouponPeriod(exCouponPeriod,
				exCouponCalendar,
				exCouponConvention,
				exCouponEndOfMonth);


			this->addRedemptionsToCashflows(std::vector<T>(1, redemption));

			QL_ENSURE(!this->cashflows().empty(), "bond with no cashflows!");
			QL_ENSURE(this->redemptions_.size() == 1, "multiple redemptions created");
	}

	template<class T>
	FixedRateBond_t<T>::FixedRateBond_t(Natural settlementDays,
		T faceAmount,
		const Schedule& schedule,
		const std::vector<InterestRate_t<T> >& coupons,
		BusinessDayConvention paymentConvention,
		T redemption,
		const Date& issueDate,
		const Calendar& paymentCalendar,
		const Period& exCouponPeriod,
		const Calendar& exCouponCalendar,
		const BusinessDayConvention exCouponConvention,
		bool exCouponEndOfMonth)

		: Bond(settlementDays,
		paymentCalendar==Calendar() ? schedule.calendar() : paymentCalendar,
		issueDate),
		frequency_(schedule.tenor().frequency()),
		dayCounter_(coupons[0].dayCounter()) {

			this->maturityDate_ = schedule.endDate();

			this->cashflows_ = FixedRateLeg_t<T>(schedule)
				.withNotionals(faceAmount)
				.withCouponRates(coupons)
				.withPaymentCalendar(this->calendar_)
				.withPaymentAdjustment(paymentConvention)
				.withExCouponPeriod(exCouponPeriod,
				exCouponCalendar,
				exCouponConvention,
				exCouponEndOfMonth);


			this->addRedemptionsToCashflows(std::vector<T>(1, redemption));

			QL_ENSURE(!this->cashflows().empty(), "bond with no cashflows!");
			QL_ENSURE(this->redemptions_.size() == 1, "multiple redemptions created");
	}
}

#endif

/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2007 Ferdinando Ametrano
 Copyright (C) 2007 Chiara Fornarola
 Copyright (C) 2015 Cheng Li

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

/*! \file floatingratebond.hpp
    \brief floating-rate bond
*/

#ifndef quantlib_floating_rate_bond_hpp
#define quantlib_floating_rate_bond_hpp

#include <ql/instruments/bond.hpp>
#include <ql/time/dategenerationrule.hpp>
#include <ql/cashflows/iborcoupon.hpp>
#include <ql/cashflows/simplecashflow.hpp>
#include <ql/time/schedule.hpp>
#include <ql/indexes/iborindex.hpp>

namespace QuantLib {

    //! floating-rate bond (possibly capped and/or floored)
    /*! \ingroup instruments

        \test calculations are tested by checking results against
              cached values.
    */
	template<class T = Real>
    class FloatingRateBond_t : public Bond_t<T> {
      public:
        FloatingRateBond_t(Natural settlementDays,
                         T faceAmount,
                         const Schedule& schedule,
                         const boost::shared_ptr<IborIndex_t<T> >& iborIndex,
                         const DayCounter& accrualDayCounter,
                         BusinessDayConvention paymentConvention
                                             = Following,
                         Natural fixingDays = Null<Natural>(),
                         const std::vector<T>& gearings
                                             = std::vector<T>(1, 1.0),
                         const std::vector<T>& spreads
                                             = std::vector<T>(1, 0.0),
                         const std::vector<T>& caps
                                             = std::vector<T>(),
                         const std::vector<T>& floors
                                            = std::vector<T>(),
                         bool inArrears = false,
                         T redemption = 100.0,
                         const Date& issueDate = Date());
        FloatingRateBond_t(Natural settlementDays,
                         T faceAmount,
                         const Date& startDate,
                         const Date& maturityDate,
                         Frequency couponFrequency,
                         const Calendar& calendar,
                         const boost::shared_ptr<IborIndex_t<T> >& iborIndex,
                         const DayCounter& accrualDayCounter,
                         BusinessDayConvention accrualConvention = Following,
                         BusinessDayConvention paymentConvention = Following,
                         Natural fixingDays = Null<Natural>(),
                         const std::vector<T>& gearings
                                             = std::vector<T>(1, 1.0),
                         const std::vector<T>& spreads
                                             = std::vector<T>(1, 0.0),
                         const std::vector<T>& caps = std::vector<T>(),
                         const std::vector<T>& floors = std::vector<T>(),
                         bool inArrears = false,
                         T redemption = 100.0,
                         const Date& issueDate = Date(),
                         const Date& stubDate = Date(),
                         DateGeneration::Rule rule = DateGeneration::Backward,
                         bool endOfMonth = false);
    };

	typedef FloatingRateBond_t<Real> FloatingRateBond;

	// Implementation

	template<class T>
	FloatingRateBond_t<T>::FloatingRateBond_t(
		Natural settlementDays,
		T faceAmount,
		const Schedule& schedule,
		const boost::shared_ptr<IborIndex_t<T> >& iborIndex,
		const DayCounter& paymentDayCounter,
		BusinessDayConvention paymentConvention,
		Natural fixingDays,
		const std::vector<T>& gearings,
		const std::vector<T>& spreads,
		const std::vector<T>& caps,
		const std::vector<T>& floors,
		bool inArrears,
		T redemption,
		const Date& issueDate)
		: Bond_t<T>(settlementDays, schedule.calendar(), issueDate) {

			this->maturityDate_ = schedule.endDate();

			this->cashflows_ = IborLeg_t<T>(schedule, iborIndex)
				.withNotionals(faceAmount)
				.withPaymentDayCounter(paymentDayCounter)
				.withPaymentAdjustment(paymentConvention)
				.withFixingDays(fixingDays)
				.withGearings(gearings)
				.withSpreads(spreads)
				.withCaps(caps)
				.withFloors(floors)
				.inArrears(inArrears);

			this->addRedemptionsToCashflows(std::vector<T>(1, redemption));

			QL_ENSURE(!this->cashflows().empty(), "bond with no cashflows!");
			QL_ENSURE(this->redemptions_.size() == 1, "multiple redemptions created");

			this->registerWith(iborIndex);
	}

	template<class T>
	FloatingRateBond_t<T>::FloatingRateBond_t(
		Natural settlementDays,
		T faceAmount,
		const Date& startDate,
		const Date& maturityDate,
		Frequency couponFrequency,
		const Calendar& calendar,
		const boost::shared_ptr<IborIndex_t<T> >& iborIndex,
		const DayCounter& accrualDayCounter,
		BusinessDayConvention accrualConvention,
		BusinessDayConvention paymentConvention,
		Natural fixingDays,
		const std::vector<T>& gearings,
		const std::vector<T>& spreads,
		const std::vector<T>& caps,
		const std::vector<T>& floors,
		bool inArrears,
		T redemption,
		const Date& issueDate,
		const Date& stubDate,
		DateGeneration::Rule rule,
		bool endOfMonth)
		: Bond_t<T>(settlementDays, calendar, issueDate) {

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

			Schedule schedule(startDate, this->maturityDate_, Period(couponFrequency),
				this->calendar_, accrualConvention, accrualConvention,
				rule, endOfMonth,
				firstDate, nextToLastDate);

			this->cashflows_ = IborLeg_t<T>(schedule, iborIndex)
				.withNotionals(faceAmount)
				.withPaymentDayCounter(accrualDayCounter)
				.withPaymentAdjustment(paymentConvention)
				.withFixingDays(fixingDays)
				.withGearings(gearings)
				.withSpreads(spreads)
				.withCaps(caps)
				.withFloors(floors)
				.inArrears(inArrears);

			this->addRedemptionsToCashflows(std::vector<T>(1, redemption));

			QL_ENSURE(!this->cashflows().empty(), "bond with no cashflows!");
			QL_ENSURE(this->redemptions_.size() == 1, "multiple redemptions created");

			this->registerWith(iborIndex);
	}

	// Plain vanilla floating rate bond. Without any special terms (e.g.cap/floor)
	template<class T = Real>
	class PlainFloatingRateBond_t :
		public FloatingRateBond_t<T> {
	public:
		PlainFloatingRateBond_t(Natural settlementDays,
							  T faceAmount,
			                  const Date& startDate,
			                  const Date& maturityDate,
			                  const Calendar& calendar,
			                  const boost::shared_ptr<IborIndex_t<T> >& iborIndex,
			                  const DayCounter& accrualDayCounter,
			                  BusinessDayConvention accrualConvention = Following,
			                  BusinessDayConvention paymentConvention = Following);
							 
	};

	typedef PlainFloatingRateBond_t<Real> PlainFloatingRateBond;

	// Implementation

	template<class T>
	PlainFloatingRateBond_t<T>::PlainFloatingRateBond_t(Natural settlementDays,
		T faceAmount,
		const Date& startDate,
		const Date& maturityDate,
		const Calendar& calendar,
		const boost::shared_ptr<IborIndex_t<T> >& iborIndex,
		const DayCounter& accrualDayCounter,
		BusinessDayConvention accrualConvention,
		BusinessDayConvention paymentConvention)
		: FloatingRateBond_t<T>(settlementDays,
		faceAmount,
		startDate,
		maturityDate,
		iborIndex->tenor().frequency(),
		calendar,
		iborIndex,
		accrualDayCounter,
		accrualConvention,
		paymentConvention) {}

}

#endif

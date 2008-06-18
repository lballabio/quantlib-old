/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2006, 2007 Chiara Fornarola
 Copyright (C) 2006, 2007 Ferdinando Ametrano
 Copyright (C) 2005, 2006 Eric Ehlers
 Copyright (C) 2005 Plamen Neykov
 Copyright (C) 2005 Walter Penschke

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

#ifndef qla_bonds_hpp
#define qla_bonds_hpp

#include <qlo/baseinstruments.hpp>
#include <qlo/leg.hpp>

#include <ql/time/businessdayconvention.hpp>
#include <ql/time/dategenerationrule.hpp>
#include <ql/types.hpp>

#include <string>

namespace QuantLib {
    class Currency;
    class FloatingRateCouponPricer;
    class SwapIndex;
    class IborIndex;
    class Schedule;
    class Calendar;
    class Date;
    class DayCounter;
    class YieldTermStructure;
}

namespace QuantLibAddin {

    class Bond : public Instrument {
      public:
        std::vector<std::vector<ObjectHandler::property_t> > flowAnalysis();
        std::string description();
        //QuantLib::Currency currency();
        std::string currency();
        void setCouponPricer(const boost::shared_ptr<QuantLib::FloatingRateCouponPricer>& pricer);
        void setCouponPricers(const std::vector<boost::shared_ptr<QuantLib::FloatingRateCouponPricer> >& pricers);
      protected:
        OH_OBJ_CTOR(Bond, Instrument);
    };

    class GenericBond : public Bond {
      public:
        GenericBond(const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
                    const std::string& des,
                    const QuantLib::Currency& cur,
                    QuantLib::Natural settlementDays,
                    const QuantLib::Calendar& calendar,
                    QuantLib::Real faceAmount,
                    const QuantLib::Date& maturityDate,
                    const QuantLib::Date& issueDate,
                    const boost::shared_ptr<Leg>& leg,
                    bool permanent);
    };

    class ZeroCouponBond : public Bond {
      public:
        ZeroCouponBond(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const std::string& des,
            const QuantLib::Currency& cur,
            QuantLib::Natural settlementDays,
            const QuantLib::Calendar& calendar,
            QuantLib::Real faceAmount,
            const QuantLib::Date& maturityDate,
            QuantLib::BusinessDayConvention paymentConvention,
            QuantLib::Real redemption,
            const QuantLib::Date& issueDate,
            bool permanent);
    };

    class FixedRateBond : public Bond {
      public:
        FixedRateBond(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const std::string& des,
            const QuantLib::Currency& cur,
            QuantLib::Natural settlementDays,
            QuantLib::Real faceAmount,
            const boost::shared_ptr<QuantLib::Schedule>& schedule,
            const std::vector<QuantLib::Rate>& coupons,
            const QuantLib::DayCounter& accrualDayCounter,
            QuantLib::BusinessDayConvention paymentConvention,
            QuantLib::Real redemption,
            const QuantLib::Date& issueDate,
            bool permanent);
        FixedRateBond(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const std::string& des,
            const QuantLib::Currency& cur,
            QuantLib::Natural settlementDays,
            const QuantLib::Calendar& calendar,
            QuantLib::Real faceAmount,
            const QuantLib::Date& startDate,
            const QuantLib::Date& maturityDate,
            const QuantLib::Period& tenor,
            const std::vector<QuantLib::Rate>& coupons,
            const QuantLib::DayCounter& accrualDayCounter,
            QuantLib::BusinessDayConvention accrualConvention,
            QuantLib::BusinessDayConvention paymentConvention,
            QuantLib::Real redemption,
            const QuantLib::Date& issueDate,
            const QuantLib::Date& stubDate,
            QuantLib::DateGeneration::Rule rule,
            bool permanent);
    };

    class FloatingRateBond : public Bond {
      public:
        FloatingRateBond(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const std::string& des,
            const QuantLib::Currency& cur,
            QuantLib::Natural settlementDays,
            QuantLib::BusinessDayConvention paymentConvention,
            QuantLib::Real faceAmount,
            const boost::shared_ptr<QuantLib::Schedule>& schedule,
            QuantLib::Natural fixingDays,
            bool inArrears,
            const QuantLib::DayCounter& paymentDayCounter,
            const std::vector<QuantLib::Rate>& floors,
            const std::vector<QuantLib::Real>& gearings,
            const boost::shared_ptr<QuantLib::IborIndex>& index,
            const std::vector<QuantLib::Spread>& spreads,
            const std::vector<QuantLib::Rate>& caps,
            QuantLib::Real redemption,
            const QuantLib::Date& issueDate,
            bool permanent);
        // add constructor without schedule
    };

    class CmsRateBond : public Bond {
      public:
        CmsRateBond(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const std::string& des,
            const QuantLib::Currency& cur,
            QuantLib::Natural settlementDays,
            QuantLib::BusinessDayConvention paymentConvention,
            QuantLib::Real faceAmount,
            const boost::shared_ptr<QuantLib::Schedule>& schedule,
            QuantLib::Natural fixingDays,
            bool inArrears,
            const QuantLib::DayCounter& paymentDayCounter,
            const std::vector<QuantLib::Rate>& floors,
            const std::vector<QuantLib::Real>& gearings,
            const boost::shared_ptr<QuantLib::SwapIndex>& index,
            const std::vector<QuantLib::Spread>& spreads,
            const std::vector<QuantLib::Rate>& caps,
            QuantLib::Real redemption,
            const QuantLib::Date& issueDate,
            bool permanent);
        // add constructor without schedule
    };

}

#endif

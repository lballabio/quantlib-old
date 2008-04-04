/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2005, 2006, 2007 Eric Ehlers
 Copyright (C) 2005 Aurelien Chanudet
 Copyright (C) 2005 Plamen Neykov
 Copyright (C) 2006, 2007, 2008 Ferdinando Ametrano
 Copyright (C) 2007 Marco Bianchetti

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

#ifndef qla_ratehelpers_hpp
#define qla_ratehelpers_hpp

#include <oh/libraryobject.hpp>

#include <ql/types.hpp>
#include <ql/time/businessdayconvention.hpp>
#include <ql/time/frequency.hpp>

namespace QuantLib {
    class YieldTermStructure;

    template<class TS>
    class BootstrapHelper;

    typedef BootstrapHelper<YieldTermStructure> RateHelper;

    class Quote;
    class Period;
    class Calendar;
    class DayCounter;
    class IborIndex;
    class SwapIndex;
    class Schedule;
    class Date;
    class FixedRateBond;
    template <class T>
    class Handle;
}

namespace QuantLibAddin {

    class RateHelper : public ObjectHandler::LibraryObject<QuantLib::RateHelper> {
      public:
        enum DepoInclusionCriteria {AllDepos,
                                    DeposBeforeFirstFuturesStartDate,
                                    DeposBeforeFirstFuturesStartDatePlusOne,
                                    DeposBeforeFirstFuturesExpiryDate
        };
      protected:
        OH_LIB_CTOR(RateHelper, QuantLib::RateHelper);
    };

    class DepositRateHelper : public RateHelper {
      public:
        DepositRateHelper(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const QuantLib::Handle<QuantLib::Quote>& rate,
            const boost::shared_ptr<QuantLib::IborIndex>& iborIndex,
            bool permanent);
        DepositRateHelper(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const QuantLib::Handle<QuantLib::Quote>& quote,
            const QuantLib::Period& p,
            QuantLib::Natural settlementDays,
            const QuantLib::Calendar& calendar,
            QuantLib::BusinessDayConvention convention,
            bool endOfMonth,
            const QuantLib::DayCounter& dayCounter,
            bool permanent);
    };

    class FuturesRateHelper : public RateHelper {
      public:
        FuturesRateHelper(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const QuantLib::Handle<QuantLib::Quote>& price,
            const QuantLib::Date& immDate,
            const boost::shared_ptr<QuantLib::IborIndex>& iborIndex,
            const QuantLib::Handle<QuantLib::Quote>& convAdj,
            bool permanent);
        FuturesRateHelper(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const QuantLib::Handle<QuantLib::Quote>& price,
            const QuantLib::Date& immDate,
            QuantLib::Size months,
            const QuantLib::Calendar& calendar,
            QuantLib::BusinessDayConvention convention,
            bool endOfMonth,
            const QuantLib::DayCounter& dayCounter,
            const QuantLib::Handle<QuantLib::Quote>& convAdj,
            bool permanent);
    };

    class SwapRateHelper : public RateHelper {
      public:
        SwapRateHelper(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const QuantLib::Handle<QuantLib::Quote>& quote,
            const boost::shared_ptr<QuantLib::SwapIndex>& swapIndex,
            const QuantLib::Handle<QuantLib::Quote>& spread,
            const QuantLib::Period& forwardStart,
            bool permanent);
        SwapRateHelper(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const QuantLib::Handle<QuantLib::Quote>& quote,
            const QuantLib::Period& p,
            const QuantLib::Calendar& calendar,
            const QuantLib::Frequency& fixedFrequency,
            QuantLib::BusinessDayConvention fixedConvention,
            const QuantLib::DayCounter& fixedDayCounter,
            const boost::shared_ptr<QuantLib::IborIndex>& iborIndex,
            const QuantLib::Handle<QuantLib::Quote>& spread,
            const QuantLib::Period& forwardStart,
            bool permanent);
     };

    class FraRateHelper : public RateHelper {
      public:
        FraRateHelper(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const QuantLib::Handle<QuantLib::Quote>& rate,
            QuantLib::Natural monthsToStart,
            const boost::shared_ptr<QuantLib::IborIndex>& iborIndex,
            bool permanent);
        FraRateHelper(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const QuantLib::Handle<QuantLib::Quote>& rate,
            QuantLib::Natural monthsToStart,
            QuantLib::Natural monthsToEnd,
            QuantLib::Natural fixingDays,
            const QuantLib::Calendar& calendar,
            QuantLib::BusinessDayConvention convention,
            bool endOfMonth,
            const QuantLib::DayCounter& dayCounter,
            bool permanent);
    };

    class FixedRateBondHelper : public RateHelper {
      public:
        FixedRateBondHelper(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const QuantLib::Handle<QuantLib::Quote>& cleanPrice,
            const boost::shared_ptr<QuantLib::FixedRateBond>& fixedBond,
            bool permanent);
        FixedRateBondHelper(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const QuantLib::Handle<QuantLib::Quote>& cleanPrice,
            QuantLib::Natural settlementDays,
            QuantLib::Real faceAmount,
            const boost::shared_ptr<QuantLib::Schedule>& schedule,
            const std::vector<QuantLib::Rate>& coupons,
            const QuantLib::DayCounter& accrualDayCounter,
            QuantLib::BusinessDayConvention paymentConvention,
            QuantLib::Real redemption,
            const QuantLib::Date& issueDate,
            bool permanent);
    };


    // Processes the set of curve bootstrapping instruments
    // and selects a subset according to the given rules and parameters
    std::vector<std::string> rateHelperSelection(
        const std::vector<boost::shared_ptr<QuantLibAddin::RateHelper> >& qlarhs,
        const std::vector<QuantLib::Natural>& priority,
        QuantLib::Natural nImmFutures,
        QuantLib::Natural nSerialFutures,
        QuantLib::Natural frontFuturesRollingDays,
        RateHelper::DepoInclusionCriteria depoInclusionCriteria,
        QuantLib::Natural minDistanceInDays);
}

#endif


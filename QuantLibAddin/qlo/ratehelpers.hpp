/*
 Copyright (C) 2005, 2006 Eric Ehlers
 Copyright (C) 2006 Ferdinando Ametrano
 Copyright (C) 2005 Plamen Neykov
 Copyright (C) 2005 Aurelien Chanudet

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it under the
 terms of the QuantLib license.  You should have received a copy of the
 license along with this program; if not, please email quantlib-dev@lists.sf.net
 The license is also available online at http://quantlib.org/html/license.html

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
*/

#ifndef qla_ratehelpers_hpp
#define qla_ratehelpers_hpp

#include <ql/indexes/iborindex.hpp>
#include <ql/termstructures/yieldcurves/ratehelpers.hpp>
#include <oh/objhandler.hpp>

namespace QuantLibAddin {

    class RateHelper : public ObjHandler::LibraryObject<QuantLib::RateHelper> {
      public:
         enum DepoInclusionCriteria {
                        AllDepos,
                        DeposBeforeFirstFuturesStartDate,
                        DeposBeforeFirstFuturesStartDatePlusOne,
                        DeposBeforeFirstFuturesExpiryDate
         };
      protected:
        QuantLib::Handle<QuantLib::Quote> quoteHandle_;
    };

    class DepositRateHelper : public RateHelper {
      public:
        DepositRateHelper(
            const QuantLib::Handle<QuantLib::Quote>& quote,
            const QuantLib::Period& p,
            const QuantLib::Natural fixingDays,
            const QuantLib::Calendar& calendar,
            QuantLib::BusinessDayConvention convention,
            const QuantLib::DayCounter& dayCounter);
    };

    class FuturesRateHelper : public RateHelper {
      public:
        FuturesRateHelper(
            const QuantLib::Handle<QuantLib::Quote>& price,
            const std::string& immDateID,
            const QuantLib::Integer months,
            const QuantLib::Calendar& calendar,
            QuantLib::BusinessDayConvention bDayConvention,
            const QuantLib::DayCounter& dayCounter,
            const QuantLib::Handle<QuantLib::Quote>& convAdj);

      protected:
        QuantLib::Handle<QuantLib::Quote> convAdjHandle_;
    };

    class SwapRateHelper : public RateHelper {
      public:
        SwapRateHelper(
            const QuantLib::Handle<QuantLib::Quote>& quote,
            const QuantLib::Period& p,
            const QuantLib::Natural fixingDays,
            const QuantLib::Calendar& calendar,
            const QuantLib::Frequency& fixedFrequency,
            QuantLib::BusinessDayConvention fixedConvention,
            const QuantLib::DayCounter& fixedDayCounter,
            const boost::shared_ptr<QuantLib::IborIndex>& index);
    };

    // Processes the set of curve bootstrapping instruments
    // and selects a subset according to the given rules and parameters
    std::vector<std::string> qlRateHelperSelection(
        const std::vector<std::string>& instrumentIDs,
        const std::vector<QuantLib::Size>& priority,
        const QuantLib::Natural nFutures,
        const QuantLib::Natural frontFuturesRollingDays,
        RateHelper::DepoInclusionCriteria depoInclusionCriteria);

}

#endif

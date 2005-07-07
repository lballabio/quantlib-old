
/*
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

#ifdef HAVE_CONFIG_H
#include <qla/config.hpp>
#endif
#include <qla/termstructures.hpp>
#include <qla/typefactory.hpp>
#include <qla/generalutils.hpp>

#include <ql/date.hpp>

namespace QuantLibAddin {

    double 
    RateHelper::setQuote(double quote) {
        double diff = quote - quote_->value();
        quote_->setValue(quote);
        return diff;
    }

    DepositRateHelper::DepositRateHelper(ObjHandler::ArgumentStack& arguments) {
        std::string dayCounterID    = OH_POP_ARGUMENT(std::string, arguments);
        std::string conventionID    = OH_POP_ARGUMENT(std::string, arguments);
        std::string calendarID      = OH_POP_ARGUMENT(std::string, arguments);
        long fixingDays             = OH_POP_ARGUMENT(long, arguments);
        std::string timeUnitsID     = OH_POP_ARGUMENT(std::string, arguments);
        long maturity               = OH_POP_ARGUMENT(long, arguments);
        double quote                = OH_POP_ARGUMENT(double, arguments);
        
        quote_ = boost::shared_ptr<QuantLib::SimpleQuote>(new QuantLib::SimpleQuote(quote));
        QuantLib::Handle<QuantLib::Quote> quoteH(quote_);
        QuantLib::TimeUnit timeUnits =
            Create<QuantLib::TimeUnit>()(timeUnitsID);
        QuantLib::Calendar calendar =
            Create<QuantLib::Calendar>()(calendarID);
        QuantLib::BusinessDayConvention convention = 
            Create<QuantLib::BusinessDayConvention>()(conventionID);
        QuantLib::DayCounter dayCounter =
            Create<QuantLib::DayCounter>()(dayCounterID);
        
        rateHelper_ = boost::shared_ptr<QuantLib::RateHelper>(
            new QuantLib::DepositRateHelper(quoteH,
                                            maturity,
                                            timeUnits,
                                            fixingDays,
                                            calendar,
                                            convention,
                                            dayCounter));
    }
    
    SwapRateHelper::SwapRateHelper(ObjHandler::ArgumentStack& arguments) {
        std::string floatingConventionID    = OH_POP_ARGUMENT(std::string, arguments);
        std::string floatingFrequencyID     = OH_POP_ARGUMENT(std::string, arguments);
        std::string fixedDayCounterID       = OH_POP_ARGUMENT(std::string, arguments);
        std::string fixedConventionID       = OH_POP_ARGUMENT(std::string, arguments);
        std::string fixedFrequencyID        = OH_POP_ARGUMENT(std::string, arguments);
        std::string calendarID              = OH_POP_ARGUMENT(std::string, arguments);
        long fixingDays                     = OH_POP_ARGUMENT(long, arguments);
        std::string timeUnitsID             = OH_POP_ARGUMENT(std::string, arguments);
        long maturity                       = OH_POP_ARGUMENT(long, arguments);
        double quote                        = OH_POP_ARGUMENT(double, arguments);
        
        quote_ = boost::shared_ptr<QuantLib::SimpleQuote>(new QuantLib::SimpleQuote(quote));
        QuantLib::Handle<QuantLib::Quote> quoteH(quote_);
        QuantLib::TimeUnit timeUnits =
            Create<QuantLib::TimeUnit>()(timeUnitsID);
        QuantLib::Calendar calendar =
            Create<QuantLib::Calendar>()(calendarID);
        QuantLib::Frequency fixedFrequency =
            Create<QuantLib::Frequency>()(fixedFrequencyID);
        QuantLib::BusinessDayConvention fixedConvention = 
            Create<QuantLib::BusinessDayConvention>()(fixedConventionID);
        QuantLib::DayCounter fixedDayCounter =
            Create<QuantLib::DayCounter>()(fixedDayCounterID);
        QuantLib::Frequency floatingFrequency =
            Create<QuantLib::Frequency>()(floatingFrequencyID);
        QuantLib::BusinessDayConvention floatingConvention = 
            Create<QuantLib::BusinessDayConvention>()(floatingConventionID);
        
        rateHelper_ = boost::shared_ptr<QuantLib::RateHelper>(
            new QuantLib::SwapRateHelper(quoteH,
                                         maturity,
                                         timeUnits,
                                         fixingDays,
                                         calendar,
                                         fixedFrequency,
                                         fixedConvention,
                                         fixedDayCounter,
                                         floatingFrequency,
                                         floatingConvention));
    }

    FutureRateHelper::FutureRateHelper(ObjHandler::ArgumentStack& arguments) {
        QuantLib::Integer decade     = OH_POP_ARGUMENT(long, arguments);    
        std::string calendarID       = OH_POP_ARGUMENT(std::string, arguments);
        std::string bDayConventionID = OH_POP_ARGUMENT(std::string, arguments);
        std::string dayCounterID     = OH_POP_ARGUMENT(std::string, arguments);
        QuantLib::Integer months     = OH_POP_ARGUMENT(long, arguments);    
        std::string immDateID        = OH_POP_ARGUMENT(std::string, arguments);
        double price                 = OH_POP_ARGUMENT(double, arguments);

        QuantLib::DayCounter dayCounter =
            Create<QuantLib::DayCounter>()(dayCounterID);
        QuantLib::BusinessDayConvention bDayConvention = 
            Create<QuantLib::BusinessDayConvention>()(bDayConventionID);
        QuantLib::Calendar calendar =
            Create<QuantLib::Calendar>()(calendarID);
        QuantLib::Date expiry = FutIDtoExpiryDate(immDateID, calendar, bDayConvention, decade);

        quote_ = boost::shared_ptr<QuantLib::SimpleQuote>(new QuantLib::SimpleQuote(price));
        rateHelper_ = boost::shared_ptr<QuantLib::RateHelper>(
            new QuantLib::FuturesRateHelper(
                QuantLib::Handle<QuantLib::Quote>(quote_),
                expiry,
                months,
                calendar,
                bDayConvention,
                dayCounter));
    }

    PiecewiseFlatForward::PiecewiseFlatForward(ObjHandler::ArgumentStack& arguments) {
        std::string dayCounterID    = OH_POP_ARGUMENT(std::string, arguments);
        std::vector<std::string> handlesRateHelper =
            OH_POP_ARGUMENT(std::vector<std::string>, arguments);
        long settlement             = OH_POP_ARGUMENT(long, arguments);
        long evaluation             = OH_POP_ARGUMENT(long, arguments);
        
        QuantLib::Date settlementDate(settlement);
        QuantLib::Date evaluationDate(evaluation);
        QuantLib::Settings::instance().setEvaluationDate(evaluationDate);
        
        std::vector<boost::shared_ptr<QuantLib::RateHelper> > rateHelpersQL;
        std::vector<std::string>::const_iterator i;
        for (i=handlesRateHelper.begin() ; i != handlesRateHelper.end() ; i++) {
            boost::shared_ptr<RateHelper> rateHelper = 
                OH_GET_OBJECT(RateHelper, *i);
            if (!rateHelper)
                QL_FAIL("PiecewiseFlatForward: error retrieving object " + *i);
            const boost::shared_ptr<QuantLib::RateHelper> rateHelperQL = 
                OH_GET_REFERENCE(QuantLib::RateHelper, rateHelper);
            rateHelpersQL.push_back(rateHelperQL);
        }
        
        QuantLib::DayCounter dayCounter =
            Create<QuantLib::DayCounter>()(dayCounterID);
        
        termStructure_ = boost::shared_ptr<QuantLib::YieldTermStructure>(
            new QuantLib::PiecewiseFlatForward(settlementDate,
                                               rateHelpersQL,
                                               dayCounter));
    }
}

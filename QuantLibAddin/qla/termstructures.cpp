
/*
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
#include <qla/optionutils.hpp>
#include <qla/termstructures.hpp>

#include <ql/date.hpp>

namespace QuantLibAddin {

    DepositRateHelper::DepositRateHelper(ObjHandler::ArgStack& args) {
        std::string dayCounterID = ObjHandler::Args<std::string>::popArg(args);
        std::string conventionID = ObjHandler::Args<std::string>::popArg(args);
        std::string calendarID = ObjHandler::Args<std::string>::popArg(args);
        long fixingDays = ObjHandler::Args<long>::popArg(args);
        std::string timeUnitsID = ObjHandler::Args<std::string>::popArg(args);
        long maturity = ObjHandler::Args<long>::popArg(args);
        double quote = ObjHandler::Args<double>::popArg(args);
        
        boost::shared_ptr<QuantLib::Quote> quoteP(new QuantLib::SimpleQuote(quote));
        QuantLib::Handle<QuantLib::Quote> quoteH(quoteP);
        QuantLib::TimeUnit timeUnits = IDtoTimeUnit(timeUnitsID);
        QuantLib::Calendar calendar = IDtoCalendar(calendarID);
        QuantLib::BusinessDayConvention convention = IDtoConvention(conventionID);
        QuantLib::DayCounter dayCounter = IDtoDayCounter(dayCounterID);
        
        rateHelper_ = boost::shared_ptr<QuantLib::RateHelper>(
            new QuantLib::DepositRateHelper(quoteH,
                                            maturity,
                                            timeUnits,
                                            fixingDays,
                                            calendar,
                                            convention,
                                            dayCounter));
    }
    
    SwapRateHelper::SwapRateHelper(ObjHandler::ArgStack& args) {
        std::string floatingConventionID = ObjHandler::Args<std::string>::popArg(args);
        std::string floatingFrequencyID = ObjHandler::Args<std::string>::popArg(args);
        std::string fixedDayCounterID = ObjHandler::Args<std::string>::popArg(args);
        std::string fixedConventionID = ObjHandler::Args<std::string>::popArg(args);
        std::string fixedFrequencyID = ObjHandler::Args<std::string>::popArg(args);
        std::string calendarID = ObjHandler::Args<std::string>::popArg(args);
        long fixingDays = ObjHandler::Args<long>::popArg(args);
        std::string timeUnitsID = ObjHandler::Args<std::string>::popArg(args);
        long maturity = ObjHandler::Args<long>::popArg(args);
        double quote = ObjHandler::Args<double>::popArg(args);
        
        boost::shared_ptr<QuantLib::Quote> quoteP(new QuantLib::SimpleQuote(quote));
        QuantLib::Handle<QuantLib::Quote> quoteH(quoteP);
        QuantLib::TimeUnit timeUnits = IDtoTimeUnit(timeUnitsID);
        QuantLib::Calendar calendar = IDtoCalendar(calendarID);
        QuantLib::Frequency fixedFrequency = IDtoFrequency(fixedFrequencyID);
        QuantLib::BusinessDayConvention fixedConvention =
            IDtoConvention(fixedConventionID);
        QuantLib::DayCounter fixedDayCounter = IDtoDayCounter(fixedDayCounterID);
        QuantLib::Frequency floatingFrequency = IDtoFrequency(floatingFrequencyID);
        QuantLib::BusinessDayConvention floatingConvention =
            IDtoConvention(floatingConventionID);
        
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
    
    PiecewiseFlatForward::PiecewiseFlatForward(ObjHandler::ArgStack& args) {
        std::string dayCounterID = ObjHandler::Args<std::string>::popArg(args);
        std::vector<std::string> handlesRateHelper =
            ObjHandler::Args<std::vector<std::string> >::popArg(args);
        long settlement = ObjHandler::Args<long>::popArg(args);
        long evaluation = ObjHandler::Args<long>::popArg(args);
        
        QuantLib::Date settlementDate(settlement);
        QuantLib::Date evaluationDate(evaluation);
        QuantLib::Settings::instance().setEvaluationDate(evaluationDate);
        
        std::vector<boost::shared_ptr<QuantLib::RateHelper> > rateHelpersQL;
        std::vector<std::string>::const_iterator i;
        for (i=handlesRateHelper.begin() ; i != handlesRateHelper.end() ; i++) {
            boost::shared_ptr<RateHelper> rateHelper = 
                boost::dynamic_pointer_cast<RateHelper>(QL_OBJECT_GET(*i));
            if (!rateHelper)
                QL_FAIL("PiecewiseFlatForward: error retrieving object " + *i);
            const boost::shared_ptr<QuantLib::RateHelper> rateHelperQL = 
                boost::static_pointer_cast<QuantLib::RateHelper>(
                    rateHelper->getReference());
            rateHelpersQL.push_back(rateHelperQL);
        }
        
        QuantLib::DayCounter dayCounter = IDtoDayCounter(dayCounterID);
        
        termStructure_ = boost::shared_ptr<QuantLib::YieldTermStructure>(
            new QuantLib::PiecewiseFlatForward(settlementDate,
                                               rateHelpersQL,
                                               dayCounter));
    }
    
}

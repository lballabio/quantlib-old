
/*
 Copyright (C) 2005 Walter Penschke

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

#if defined(HAVE_CONFIG_H)
    #include <qla/config.hpp>
#endif

#include <qla/zerocurve.hpp>
#include <qla/optionutils.hpp>
#include <qla/instrumentutils.hpp>

namespace QuantLibAddin {

    ZeroCurve::ZeroCurve(ObjHandler::ArgStack& args) {
        std::string dayCounterID  = ObjHandler::Args<std::string>::popArg(args);
        std::vector < double > yields 
            = ObjHandler::Args< std::vector < double > >::popArg(args);
        std::vector < long > dates 
            = ObjHandler::Args< std::vector < long > >::popArg(args);

        QuantLib::DayCounter dayCounter     = IDtoDayCounter(dayCounterID);
        const std::vector<QuantLib::Rate> yieldsQL = 
            doubleVectorToRateVector(yields);
        const std::vector<QuantLib::Date> datesQL = 
            longVectorToDateVector(dates);

        zeroCurve_ = 
            boost::shared_ptr<QuantLib::ZeroCurve>(
                new QuantLib::ZeroCurve(datesQL,
                                        yieldsQL,
                                        dayCounter));

        std::vector<QuantLib::Date> retrievedDates = zeroCurve_->dates();
        std::vector<QuantLib::Date>::iterator someIter;
        std::vector<long> derivedDates;
        std::vector<double> derivedRates;
        for (someIter = retrievedDates.begin(); 
             someIter != retrievedDates.end(); someIter++) 
        {
            QuantLib::Date date = *someIter;
            QuantLib::Rate rate = zeroCurve_->zeroRate(*someIter, dayCounter, QuantLib::Continuous).rate();
            derivedDates.push_back(date.serialNumber());
            derivedRates.push_back(rate);
        }

        // Setup object properties
        ObjHandler::any_ptr anyDates(new boost::any(derivedDates));
        ObjHandler::any_ptr anyRates(new boost::any(derivedRates));

        ObjHandler::ObjectProperty propDates(FIELD_DATES, anyDates);
        ObjHandler::ObjectProperty propRates(FIELD_RATES, anyRates);

        properties_.push_back(propDates);
        properties_.push_back(propRates);

    }
}



/*
 Copyright (C) 2005 Plamen Neykov
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
#include <qla/generalutils.hpp>
#include <qla/enumfactory.hpp>

// indexes to the Property vector
// FIXME - need a cleaner way to achieve this
#define FIELD_RATES  "RATES"
#define FIELD_DATES  "DATES"
#define IDX_RATES    0
#define IDX_DATES    1

namespace QuantLibAddin {

    ZeroCurve::ZeroCurve(ObjHandler::ArgumentStack& arguments) {
        std::string dayCounterID  = OH_POP_ARGUMENT(std::string, arguments);
        std::vector < double > yields 
            = OH_POP_ARGUMENT(std::vector < double >, arguments);
        std::vector < long > dates 
            = OH_POP_ARGUMENT(std::vector < long >, arguments);

		QuantLib::DayCounter dayCounter = CreateEnum<QuantLib::DayCounter>::create(dayCounterID);
        const std::vector<QuantLib::Date> datesQL = 
            longVectorToDateVector(dates);

        zeroCurve_ = 
            boost::shared_ptr<QuantLib::ZeroCurve>(
                new QuantLib::ZeroCurve(datesQL,
                                        yields,
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



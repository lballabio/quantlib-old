
/*
 Copyright (C) 2005 Eric Ehlers

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

#ifndef qla_generalutils_hpp
#define qla_generalutils_hpp

#include <ql/DayCounters/all.hpp>
#include <ql/basicdataformatters.hpp>
#include <ql/calendar.hpp>
#include <ql/Math/matrix.hpp>

#define QL_OBJECT_GET(X) ObjHandler::ObjectHandler::instance().retrieveObject(X)

namespace QuantLibAddin {

    QuantLib::DayCounter IDtoDayCounter(
        const std::string &dayCounterID);
    
    QuantLib::Calendar IDtoCalendar(
        const std::string &calendarID);
    
    QuantLib::TimeUnit IDtoTimeUnit(
        const std::string &timeUnitID);
    
    QuantLib::BusinessDayConvention IDtoConvention(
        const std::string &conventionID);

    QuantLib::Frequency IDtoFrequency(
        const std::string &frequencyID);

    std::vector<QuantLib::Date> longVectorToDateVector(
        const std::vector < long > &v);

    QuantLib::Matrix vectorVectorToMatrix(
        const std::vector < std::vector < double > > &vv);

}

#endif


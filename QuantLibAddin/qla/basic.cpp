
/*
 Copyright (C) 2006 Marco Marchioro--StatPro Italia

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

#if defined(HAVE_CONFIG_H)     // Dynamically created by configure
    #include <qla/config.hpp>
#endif
#include <ql/calendar.hpp>
#include <ql/daycounter.hpp>

#include <qla/basic.hpp>
#include <qla/clientutils.hpp>
#include <qla/typefactory.hpp>


using namespace ObjHandler;

namespace QuantLibAddin {

    long qlAdvanceCalendar(
            const long &lStartDate,
            const long &n,
            const std::string &timeUnitsID,
            const std::string &calendarID,
            const std::string &conventionID){

        QuantLib::Date startDate=createQLDate(lStartDate);


        QuantLib::Calendar calendar =
            Create<QuantLib::Calendar>()(calendarID);

        QuantLib::TimeUnit timeUnits =
            Create<QuantLib::TimeUnit>()(timeUnitsID);

        QuantLib::BusinessDayConvention convention =
            Create<QuantLib::BusinessDayConvention>()(conventionID);

        QuantLib::Date endDate=calendar.advance(startDate,
                                                n,
                                                timeUnits,
                                                convention);

        return endDate.serialNumber();                
    }

    double qlYearFraction(
        const std::string &bDayConvID,
        const long &lStartDate,
        const long &lEndDate,
        const long &lRefStartDate,
        const long &lRefEndDate){

        QuantLib::DayCounter dayCounter = Create<QuantLib::DayCounter>()(bDayConvID);
        QuantLib::Date startDate=createQLDate(lStartDate);
        QuantLib::Date endDate=createQLDate(lEndDate);

        QuantLib::Date refStartDate, refEndDate;
        if(lRefStartDate>0)
            refStartDate = createQLDate(lRefStartDate);
        if(lRefEndDate>0)
            refEndDate = createQLDate(lRefEndDate);
        return dayCounter.yearFraction(startDate, endDate, refStartDate, refEndDate);
    }    

}


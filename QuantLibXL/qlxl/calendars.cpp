/*
 Copyright (C) 2004 Ferdinando Ametrano

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

/*! \file calendars.cpp
    \brief QuantLib Excel calendar functions
*/

#include <qlxl/qlxlfoper.hpp>
#include <ql/functions/calendars.hpp>

extern "C"
{


    using namespace QuantLib;

    LPXLOPER EXCEL_EXPORT xlholidayList(
                        XlfOper xlCalendar,
                        XlfOper xlDate1,
                        XlfOper xlDate2,
                        XlfOper xlIncludeWeekEnds)
    {
        EXCEL_BEGIN;

        Calendar cal = QlXlfOper(xlCalendar).AsCalendar();
		Date d1 = QlXlfOper(xlDate1).AsDate();
		Date d2 = QlXlfOper(xlDate2).AsDate();
		bool includeWeekEnds = xlIncludeWeekEnds.AsBool();
		std::vector<Date> holidays = holidayList(cal, d1, d2, includeWeekEnds);
		Size n = holidays.size();
		std::vector<double> hol(n);
		for (Size i = 0; i < n; i++) {
			hol[i]=holidays[i].serialNumber();
		}
		return XlfOper(n, 1, hol.begin());

        EXCEL_END;
    }



}

/*
 Copyright (C) 2002 Ferdinando Ametrano

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it under the
 terms of the QuantLib license.  You should have received a copy of the
 license along with this program; if not, please email ferdinando@ametrano.net
 The license is also available online at http://quantlib.org/html/license.html

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
*/
/*! \file datef.cpp
    \brief QuantLib Excel date functions

    \fullpath
    qlxl/%datef.cpp
*/

// $Id$

#include <qlxl/qlxl.hpp>
#include <qlxl/datef.hpp>

extern "C"
{


    using namespace QuantLib;

    LPXLOPER EXCEL_EXPORT xlaccrualDays(
                        XlfOper xlDate1,
                        XlfOper xlDate2,
                        XlfOper xlDayCountType)
    {
        EXCEL_BEGIN;

        std::string inputString(xlDayCountType.AsString());
        std::string s = StringFormatter::toLowercase(inputString);
        DayCounter dc = DayCounters::Actual365();


        if (s == "1" || s == "act365" || s == "act/365")
            dc = DayCounters::Actual365();
        else if (s == "2" || s == "act360" || s == "act/360")
            dc = DayCounters::Actual360();
        else if (s == "3" || s == "actacte" || s == "act/act(e)" || s == "act/act(Euro)")
            dc = DayCounters::ActualActual(DayCounters::ActualActual::Euro);
        else if (s == "4" || s == "30/360" || s == "30/360us")
            dc = DayCounters::Thirty360(DayCounters::Thirty360::USA);
        else if (s == "5" || s == "30e/360" || s == "30/360e" || s == "30/360eu")
            dc = DayCounters::Thirty360(DayCounters::Thirty360::European);
        else if (s == "6" || s == "30/360i" || s == "30/360it")
            dc = DayCounters::Thirty360(DayCounters::Thirty360::Italian);
        else if (s == "7" || s == "actact" || s == "act/act" || s == "act/act(b)" || s == "act/act (Bond)")
            dc = DayCounters::ActualActual(DayCounters::ActualActual::Bond);
        else if (s == "8" || s == "actacth" || s == "act/act(h)" || s == "act/act (ISDA)")
            dc = DayCounters::ActualActual(DayCounters::ActualActual::Historical);
        else if (s == "9" || s == "30/360isda")
            dc = DayCounters::Thirty360(DayCounters::Thirty360::USA);
        else if (s == "10"|| s == "30e/360isda")
            dc = DayCounters::Thirty360(DayCounters::Thirty360::European);
        else
            throw Error("Unknown day counter: " + inputString);


		Date d1(xlDate1.AsInt());
		Date d2(xlDate2.AsInt());

		double result = Functions::accrualDays(dc, d1, d2);
        return XlfOper(result);

        EXCEL_END;
    }

    LPXLOPER EXCEL_EXPORT xlaccrualFactor(
                        XlfOper xlDate1,
                        XlfOper xlDate2,
                        XlfOper xlDayCountType,
                        XlfOper xlDate3,
                        XlfOper xlDate4)
    {
        EXCEL_BEGIN;

        std::string inputString(xlDayCountType.AsString());
        std::string s = StringFormatter::toLowercase(inputString);
        DayCounter dc = DayCounters::Actual365();


        if (s == "1" || s == "act365" || s == "act/365")
            dc = DayCounters::Actual365();
        else if (s == "2" || s == "act360" || s == "act/360")
            dc = DayCounters::Actual360();
        else if (s == "3" || s == "actacte" || s == "act/act(e)" || s == "act/act(Euro)")
            dc = DayCounters::ActualActual(DayCounters::ActualActual::Euro);
        else if (s == "4" || s == "30/360" || s == "30/360us")
            dc = DayCounters::Thirty360(DayCounters::Thirty360::USA);
        else if (s == "5" || s == "30e/360" || s == "30/360e" || s == "30/360eu")
            dc = DayCounters::Thirty360(DayCounters::Thirty360::European);
        else if (s == "6" || s == "30/360i" || s == "30/360it")
            dc = DayCounters::Thirty360(DayCounters::Thirty360::Italian);
        else if (s == "7" || s == "actact" || s == "act/act" || s == "act/act(b)" || s == "act/act (Bond)")
            dc = DayCounters::ActualActual(DayCounters::ActualActual::Bond);
        else if (s == "8" || s == "actacth" || s == "act/act(h)" || s == "act/act (ISDA)")
            dc = DayCounters::ActualActual(DayCounters::ActualActual::Historical);
        else if (s == "9" || s == "30/360isda")
            dc = DayCounters::Thirty360(DayCounters::Thirty360::USA);
        else if (s == "10"|| s == "30e/360isda")
            dc = DayCounters::Thirty360(DayCounters::Thirty360::European);
        else
            throw Error("Unknown day counter: " + inputString);


		Date d1(xlDate1.AsInt());
		Date d2(xlDate2.AsInt());

        Date d3 = Date();
        Date d4 = Date();
/*
        if (!xlDate3.IsMissing()) d3(xlDate3.AsInt());
        if (!xlDate4.IsMissing)() d4(xlDate4.AsInt());
*/

        double result = Functions::accrualFactor(dc, d1, d2, d3, d4);
        return XlfOper(result);

        EXCEL_END;
    }



}

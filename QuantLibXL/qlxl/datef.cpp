/*
 Copyright (C) 2002, 2003 Ferdinando Ametrano

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

/*! \file datef.cpp
    \brief QuantLib Excel date functions
*/

#include <qlxl/qlxlfoper.hpp>
#include <ql/functions/daycounters.hpp>

extern "C"
{


    using namespace QuantLib;

    LPXLOPER EXCEL_EXPORT xlaccrualDays(
                        XlfOper xlDate1,
                        XlfOper xlDate2,
                        XlfOper xlDayCountType)
    {
        EXCEL_BEGIN;

		Date d1 = QlXlfOper(xlDate1).AsDate();
		Date d2 = QlXlfOper(xlDate2).AsDate();
        DayCounter dc = QlXlfOper(xlDayCountType).AsDayCounter();
		double result = accrualDays(dc, d1, d2);
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

		Date d1 = QlXlfOper(xlDate1).AsDate();
		Date d2 = QlXlfOper(xlDate2).AsDate();
        DayCounter dc = QlXlfOper(xlDayCountType).AsDayCounter();
		Date d3 = QlXlfOper(xlDate3).AsDate();
		Date d4 = QlXlfOper(xlDate4).AsDate();

        double result = accrualFactor(dc, d1, d2, d3, d4);
        return XlfOper(result);

        EXCEL_END;
    }



}

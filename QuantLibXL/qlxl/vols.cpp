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
/*! \file vols.cpp
    \brief QuantLib Excel volatility functions

    \fullpath
    qlxl/%vols.cpp
*/

// $Id$

#include <qlxl/qlxl.hpp>
#include <qlxl/vols.hpp>

extern "C"
{


    using namespace QuantLib;

    LPXLOPER EXCEL_EXPORT xlBlackVol(XlfOper xlrefDate,
                                     XlfOper xlDayCountType,
                                     XlfOper xldates,
                                     XlfOper xlstrikes,
                                     XlfOper xlblackVolSurface,
                                     XlfOper xldate1,
                                     XlfOper xldate2,
                                     XlfOper xlstrike,
                                     XlfOper xlinterpolation2DType,
                                     XlfOper xlallowExtrapolation) {
        EXCEL_BEGIN;
        Date refDate= Date(xlrefDate.AsInt());

        std::string inputString(xlDayCountType.AsString());
        std::string s = StringFormatter::toLowercase(inputString);
        DayCounter dc = DayCounters::Actual365();

        if (s == "1" || s == "act365" || s == "act/365")
            dc = DayCounters::Actual365();
        else if (s == "2" || s == "act360" || s == "act/360")
            dc = DayCounters::Actual360();
        else if (s == "3" || s == "actacte" || s == "act/act(e)"
                          || s == "act/act(Euro)")
            dc = DayCounters::ActualActual(DayCounters::ActualActual::Euro);
        else if (s == "4" || s == "30/360" || s == "30/360us")
            dc = DayCounters::Thirty360(DayCounters::Thirty360::USA);
        else if (s == "5" || s == "30e/360" || s == "30/360e"
                          || s == "30/360eu")
            dc = DayCounters::Thirty360(DayCounters::Thirty360::European);
        else if (s == "6" || s == "30/360i" || s == "30/360it")
            dc = DayCounters::Thirty360(DayCounters::Thirty360::Italian);
        else if (s == "7" || s == "actact" || s == "act/act"
                          || s == "act/act(b)" || s == "act/act (Bond)")
            dc = DayCounters::ActualActual(DayCounters::ActualActual::Bond);
        else if (s == "8" || s == "actacth" || s == "act/act(h)"
                          || s == "act/act (ISDA)")
            dc = DayCounters::ActualActual(
                        DayCounters::ActualActual::Historical);
        else if (s == "9" || s == "30/360isda")
            dc = DayCounters::Thirty360(DayCounters::Thirty360::USA);
        else if (s == "10"|| s == "30e/360isda")
            dc = DayCounters::Thirty360(DayCounters::Thirty360::European);
        else
            throw Error("Unknown day counter: " + inputString);


        std::vector<double> doubleDates = xldates.AsDoubleVector();
        std::vector<Date> dates(doubleDates.size());
        Size i;
        for (i=0; i<doubleDates.size(); i++) {
            dates[i]=Date(int(doubleDates[i]));
        }

        std::vector<double> strikes = xlstrikes.AsDoubleVector();


        // Initialization of the counter variables
        Size j, rowNo = strikes.size(), colNo = doubleDates.size();

        XlfRef matrix_range = xlblackVolSurface.AsRef();
        QL_REQUIRE(matrix_range.GetNbCols()==colNo,
            "the matrix range must be NxM");
        QL_REQUIRE(matrix_range.GetNbRows()==rowNo,
            "the matrix range must be NxM");


        // XlfExcel::Coerce method (internally called) will return to Excel
        // if one of the cell was invalidated and need to be recalculated.
        // Excel will calculate this cell and call again our function.
        // Thus we copy first all the data to avoid to partially compute the
        // average for nothing since one of the cell might be uncalculated.

        // Allocate the vectors where to copy the values.
        Math::Matrix data_matrix(rowNo, colNo);
        for (i = 0; i < rowNo; ++i) {
            for (j = 0; j < colNo; ++j) {
                data_matrix[i][j] = matrix_range(i,j).AsDouble();
            }
        }

        Date date1= Date(xldate1.AsInt());
        Date date2= Date(xldate2.AsInt());
        double strike = xlstrike.AsDouble();

        double result = Functions::blackVol(refDate, dc,
            dates, strikes, data_matrix,
            date1, date2, strike,
            xlinterpolation2DType.AsInt(),
            xlallowExtrapolation.AsBool());
        return XlfOper(result);
        EXCEL_END;
    }


}


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
/*! \file qlxlfoper.hpp
    \brief XlfOper specialization for QuantLib classes

    \fullpath
    qlxl/%qlxlfoper.hpp
*/

// $Id$

#include <qlxl/qlxlfoper.hpp>

using namespace QuantLib;
using QuantLib::DayCounter;
using QuantLib::Date;
using QuantLib::Math::Matrix;


QlXlfOper::QlXlfOper(const XlfOper& xlfOper)
: xlfOper_(xlfOper) {}

QuantLib::DayCounter QlXlfOper::AsDayCounter() const {

    std::string inputString(xlfOper_.AsString());
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

    return dc;

}

QuantLib::Date QlXlfOper::AsDate() const {
    return Date(xlfOper_.AsInt());
}

std::vector<QuantLib::Date> QlXlfOper::AsDateVector() const {
    std::vector<double> doubleDates = xlfOper_.AsDoubleVector();
    std::vector<Date> dates(doubleDates.size());
    Size i;
    for (i=0; i<doubleDates.size(); i++) {
        dates[i]=Date(int(doubleDates[i]));
    }
    return dates;
}

QuantLib::Math::Matrix QlXlfOper::AsMatrix() const {

    XlfRef matrix_range = xlfOper_.AsRef();
    Size rowNo = matrix_range.GetNbRows();
    Size colNo = matrix_range.GetNbCols();
    Matrix data_matrix(rowNo, colNo);
    for (Size i = 0; i < rowNo; ++i) {
        for (Size j = 0; j < colNo; ++j) {
            data_matrix[i][j] = matrix_range(i,j).AsDouble();
        }
    }

    return data_matrix;
}


QuantLib::Option::Type QlXlfOper::AsOptionType() const {

    std::string inputString(xlfOper_.AsString());
    std::string s = StringFormatter::toLowercase(inputString);
    Option::Type type;
    if (s == "c" || s == "call") {
        type = Option::Call;
    } else if (s == "p" || s == "put") {
        type = Option::Put;
    } else if (s == "s" || s == "straddle") {
        type = Option::Straddle;
    } else
        throw Error("Unknown option type");

    return type;
}



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
/*! \file qlxlfoper.hpp
    \brief XlfOper specialization for QuantLib classes

    \fullpath
    qlxl/%qlxlfoper.hpp
*/

// $Id$

#include <qlxl/qlxlfoper.hpp>

using namespace QuantLib;
using namespace QuantLib::Math;
using namespace QuantLib::VolTermStructures;
//using QuantLib::DayCounter;
//using QuantLib::Date;
//using QuantLib::Math::Matrix;
//using QuantLib::Math::LinearInterpolation;
//using QuantLib::Math::BilinearInterpolation;


QlXlfOper::QlXlfOper(const XlfOper& xlfOper)
: xlfOper_(xlfOper) {}

DayCounter QlXlfOper::AsDayCounter() const {

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

Date QlXlfOper::AsDate() const {
    return Date(xlfOper_.AsInt());
}

std::vector<Date> QlXlfOper::AsDateVector() const {
    std::vector<double> doubleDates = xlfOper_.AsDoubleVector();
    std::vector<Date> dates(doubleDates.size());
    Size i;
    for (i=0; i<doubleDates.size(); i++) {
        dates[i]=Date(int(doubleDates[i]));
    }
    return dates;
}

Math::Matrix QlXlfOper::AsMatrix() const {

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


Option::Type QlXlfOper::AsOptionType() const {

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

RelinkableHandle<BlackVolTermStructure> QlXlfOper::AsBlackVolTermStructure(
    const Date& referenceDate, int interpolationType) const {


    XlfRef range = xlfOper_.AsRef();
    Size rowNo = range.GetNbRows();
    Size colNo = range.GetNbCols();
    if (rowNo==1 && colNo==1) {
        // constant vol
        double vol = range(0,0).AsDouble();
        return Handle<BlackVolTermStructure>(new
            BlackConstantVol(referenceDate,
            vol));
    } else if (rowNo>=1 && colNo==2) {
        // vertical time dependent vol
        std::vector<Date> dates(rowNo);
        std::vector<double> vols(rowNo);
        for (Size j = 0; j<rowNo; j++) {
            dates[j] = QlXlfOper(range(j, 0)).AsDate();
            vols[j] = range(j, 1).AsDouble();
        }
        Handle<BlackVarianceCurve> ts(new
            BlackVarianceCurve(referenceDate,dates,vols));
        switch (interpolationType) {
            case 1:
                return ts;
                break;
            case 2:
                #if defined(QL_PATCH_MICROSOFT)
                ts->setInterpolation(CubicSpline());
                #else
                ts->setInterpolation<CubicSpline>();
                #endif
                return ts;
                break;
            default:
                throw IllegalArgumentError(
                    "interpolate: invalid interpolation type");
        }
    } else if (rowNo==2 && colNo>=1) {
        // horizontal time dependent vol
        std::vector<Date> dates(colNo);
        std::vector<double> vols(colNo);
        for (Size j = 0; j<colNo; j++) {
            dates[j] = QlXlfOper(range(0, j)).AsDate();
            vols[j] = range(1, j).AsDouble();
        }
        Handle<BlackVarianceCurve> ts(new
            BlackVarianceCurve(referenceDate,dates,vols));
        switch (interpolationType) {
            case 1:
                return ts;
                break;
            case 2:
                #if defined(QL_PATCH_MICROSOFT)
                ts->setInterpolation(CubicSpline());
                #else
                ts->setInterpolation<CubicSpline>();
                #endif
                return ts;
                break;
            default:
                throw IllegalArgumentError(
                    "interpolate: invalid interpolation type");
        }
    } else if (rowNo>3 && colNo>2) {
        // time/strike (horizontal/vertical) dependent vol
        // at least 3 strikes for the smile,
        // no less than 2 dates for a time structure
        std::vector<Date> dates(colNo-1);
        std::vector<double> strikes(rowNo-1);
        Matrix vols(rowNo-1, colNo-1);
        Size i, j;
        for (j = 1; j<colNo; j++) {
            dates[j-1] = QlXlfOper(range(0, j)).AsDate();
        }
        for (i = 1; i<rowNo; i++) {
            strikes[i-1] = range(i, 0).AsDouble();
        }
        for (j = 1; j<colNo; j++) {
            for (i = 1; i<rowNo; i++) {
                vols[i-1][j-1] = range(i, j).AsDouble();
            }
        }
        Handle<BlackVarianceSurface> ts(new
            BlackVarianceSurface(referenceDate, dates, strikes, vols));
        switch (interpolationType) {
            case 1:
                return ts;
                break;
            case 2:
                #if defined(QL_PATCH_MICROSOFT)
                ts->setInterpolation(CubicSpline());
                #else
                ts->setInterpolation<CubicSpline>();
                #endif
                return ts;
                break;
            default:
                throw IllegalArgumentError(
                    "interpolate: invalid interpolation type");
        }
    } else
        throw Error("Not a vol surface range");

}


RelinkableHandle<TermStructure> QlXlfOper::AsTermStructure(
    const Date& referenceDate) const {

    // Should we add today to the interface of AsTermStructure ?
    Date today=referenceDate;

    XlfRef range = xlfOper_.AsRef();
    Size rowNo = range.GetNbRows();
    Size colNo = range.GetNbCols();
    if (rowNo==1 && colNo==1) {
        // constant rate continuos compounding act/365
        double forwardRate = range(0,0).AsDouble();
        return Handle<TermStructure>(new
            TermStructures::FlatForward(today,
                                        referenceDate,
                                        forwardRate,
                                        DayCounters::Actual365()));
    } else if (rowNo>1 && colNo==2 && range(0,1).AsDouble()==1.0) {
        // vertical discount grid


        std::vector<Date> dates(rowNo);
        std::vector<DiscountFactor> discounts(rowNo);
        for (Size j = 0; j<rowNo; j++) {
            dates[j] = QlXlfOper(range(j, 0)).AsDate();
            discounts[j] = range(j, 1).AsDouble();
        }
        Date today=dates[0];

        return Handle<TermStructure>(new
            TermStructures::DiscountCurve(today,
                                          dates,
                                          discounts,
                                          DayCounters::Actual365()));
    } else if (rowNo==2 && colNo>1 && range(1,0).AsDouble()==1.0) {
        // horizontal discount grid


        std::vector<Date> dates(colNo);
        std::vector<DiscountFactor> discounts(colNo);
        for (Size j = 0; j<colNo; j++) {
            dates[j] = QlXlfOper(range(0, j)).AsDate();
            discounts[j] = range(1, j).AsDouble();
        }
        Date today=dates[0];

        return Handle<TermStructure>(new
            TermStructures::DiscountCurve(today,
                                          dates,
                                          discounts,
                                          DayCounters::Actual365()));
    } else if (rowNo>1 && colNo==2) {
        // vertical piecewise forward (annual continuos compounding act/365) grid
        std::vector<Date> dates(rowNo);
        std::vector<Rate> forwards(rowNo);
        for (Size j = 0; j<rowNo; j++) {
            dates[j] = QlXlfOper(range(j, 0)).AsDate();
            forwards[j] = range(j, 1).AsDouble();
        }
        Date today=dates[0];

        return Handle<TermStructure>(new
            TermStructures::PiecewiseFlatForward(today,
                                                 dates,
                                                 forwards,
                                                 DayCounters::Actual365()));
    } else if (rowNo==2 && colNo>1) {
        // horizontal piecewise forward (annual continuos compounding act/365) grid
        std::vector<Date> dates(colNo);
        std::vector<Rate> forwards(colNo);
        for (Size j = 0; j<colNo; j++) {
            dates[j] = QlXlfOper(range(0, j)).AsDate();
            forwards[j] = range(1, j).AsDouble();
        }
        Date today=dates[0];

        return Handle<TermStructure>(new
            TermStructures::PiecewiseFlatForward(today,
                                                 dates,
                                                 forwards,
                                                 DayCounters::Actual365()));
    } else
        throw Error("Not a yield term structure range");

}

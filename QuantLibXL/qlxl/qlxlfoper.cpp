
/*
 Copyright (C) 2002, 2003, 2004 Ferdinando Ametrano

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

*/

#include <qlxl/qlxlfoper.hpp>
#include <ql/Calendars/all.hpp>
#include <ql/Functions/daycounters.hpp>
#include <ql/Volatilities/blackconstantvol.hpp>
#include <ql/Volatilities/blackvariancecurve.hpp>
#include <ql/Volatilities/blackvariancesurface.hpp>
#include <ql/Math/interpolationtraits.hpp>
#include <ql/TermStructures/flatforward.hpp>
#include <ql/TermStructures/discountcurve.hpp>
#include <ql/TermStructures/piecewiseflatforward.hpp>
#include <boost/shared_ptr.hpp>

using namespace QuantLib;

QlXlfOper::QlXlfOper(const XlfOper& xlfOper)
: xlfOper_(xlfOper) {}

Calendar QlXlfOper::AsCalendar() const {

    std::string inputString(xlfOper_.AsString());
    std::string s = StringFormatter::toLowercase(inputString);
    Calendar cal = TARGET();

    if (s == "target" || s == "euro" || s == "eur")
        return TARGET();
    else if (s == "unitedstates" || s == "us")
        return UnitedStates(UnitedStates::Settlement);
    else if (s == "usexchange" || s == "nyexchange" || s == "nyse")
        return UnitedStates(UnitedStates::Exchange);
    else if (s == "usbonds" || s == "usbond")
        return UnitedStates(UnitedStates::GovernmentBond);
    else if (s == "unitedkingdom" || s == "uk")
        return UnitedKingdom(UnitedKingdom::Settlement);
    else if (s == "ukexchange" || s == "londonexchange")
        return UnitedKingdom(UnitedKingdom::Exchange);
    else if (s == "ukmetals" || s == "londonmetals")
        return UnitedKingdom(UnitedKingdom::Metals);
    else if (s == "beijing")
        return Beijing();
    else if (s == "budapest")
        return Budapest();
    else if (s == "copenhagen")
        return Copenhagen();
    else if (s == "germany")
        return Germany(Germany::Settlement);
    else if (s == "germanyexchange")
        return Germany(Germany::FrankfurtStockExchange);
    else if (s == "germanyxetra" || s == "xetra")
        return Germany(Germany::Xetra);
    else if (s == "germanyeurex" || s == "eurex")
        return Germany(Germany::Eurex);
    else if (s == "helsinki")
        return Helsinki();
    else if (s == "hongkong")
        return HongKong();
    else if (s == "italy")
        return Italy(Italy::Settlement);
    else if (s == "italyexchange" || s == "milanexchange")
        return Italy(Italy::Exchange);
    else if (s == "johannesburg" || s == "jhb")
        return Johannesburg();
    else if (s == "oslo")
        return Oslo();
    else if (s == "riyadh")
        return Riyadh();
    else if (s == "seoul")
        return Seoul();
    else if (s == "singapore")
        return Singapore();
    else if (s == "stockholm")
        return Stockholm();
    else if (s == "sydney")
        return Sydney();
    else if (s == "taiwan")
        return Taiwan();
    else if (s == "tokyo")
        return Tokyo();
    else if (s == "toronto")
        return Toronto();
    else if (s == "warsaw")
        return Warsaw();
    else if (s == "wellington")
        return Wellington();
    else if (s == "zurich" || s == "zur")
        return Zurich();
    else if (s == "null")
        return NullCalendar();
    else
        QL_FAIL("Unknown calendar: " + inputString);

    return cal;

}

DayCounter QlXlfOper::AsDayCounter() const {

    std::string inputString(xlfOper_.AsString());
    return dayCounterFromString(inputString);
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

Matrix QlXlfOper::AsMatrix() const {

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
    } else
        QL_FAIL("Unknown option type");

    return type;
}

Handle<BlackVolTermStructure> QlXlfOper::AsBlackVolTermStructure(
    const Date& referenceDate, int interpolationType) const {


    XlfRef range = xlfOper_.AsRef();
    Size rowNo = range.GetNbRows();
    Size colNo = range.GetNbCols();
    if (rowNo==1 && colNo==1) {
        // constant vol
        double vol = range(0,0).AsDouble();
        return Handle<BlackVolTermStructure>(
            boost::shared_ptr<BlackVolTermStructure>(new
                BlackConstantVol(referenceDate, vol)));
    } else if (rowNo>=1 && colNo==2) {
        // vertical time dependent vol
        std::vector<Date> dates(rowNo);
        std::vector<double> vols(rowNo);
        for (Size j = 0; j<rowNo; j++) {
            dates[j] = QlXlfOper(range(j, 0)).AsDate();
            vols[j] = range(j, 1).AsDouble();
        }
        boost::shared_ptr<BlackVarianceCurve> ts(new
                BlackVarianceCurve(referenceDate,dates,vols));
        switch (interpolationType) {
            case 1:
                return Handle<BlackVolTermStructure>(ts);
                break;
            case 2:
                #if defined(QL_PATCH_MICROSOFT)
                ts->setInterpolation(Cubic());
                #else
                ts->setInterpolation<Cubic>();
                #endif
                return Handle<BlackVolTermStructure>(ts);
                break;
            default:
                QL_FAIL("interpolate: invalid interpolation type");
        }
    } else if (rowNo==2 && colNo>=1) {
        // horizontal time dependent vol
        std::vector<Date> dates(colNo);
        std::vector<double> vols(colNo);
        for (Size j = 0; j<colNo; j++) {
            dates[j] = QlXlfOper(range(0, j)).AsDate();
            vols[j] = range(1, j).AsDouble();
        }
        boost::shared_ptr<BlackVarianceCurve> ts(new
            BlackVarianceCurve(referenceDate,dates,vols));
        switch (interpolationType) {
            case 1:
                return Handle<BlackVolTermStructure>(ts);
                break;
            case 2:
                #if defined(QL_PATCH_MICROSOFT)
                ts->setInterpolation(Cubic());
                #else
                ts->setInterpolation<Cubic>();
                #endif
                return Handle<BlackVolTermStructure>(ts);
                break;
            default:
                QL_FAIL("interpolate: invalid interpolation type");
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
        boost::shared_ptr<BlackVarianceSurface> ts(new
            BlackVarianceSurface(referenceDate, dates, strikes, vols));
        switch (interpolationType) {
            case 1:
                return Handle<BlackVolTermStructure>(ts);
                break;
            case 2:
                #if defined(QL_PATCH_MICROSOFT)
                ts->setInterpolation(Cubic());
                #else
                ts->setInterpolation<Cubic>();
                #endif
                return Handle<BlackVolTermStructure>(ts);
                break;
            default:
                QL_FAIL("interpolate: invalid interpolation type");
        }
    } else
        QL_FAIL("Not a vol surface range");

}


Handle<YieldTermStructure> QlXlfOper::AsTermStructure(
    const Date& referenceDate) const {

    XlfRef range = xlfOper_.AsRef();
    Size rowNo = range.GetNbRows();
    Size colNo = range.GetNbCols();
    if (rowNo==1 && colNo==1) {
        // constant rate continuos compounding act/365
        double forwardRate = range(0,0).AsDouble();
        return Handle<YieldTermStructure>(
            boost::shared_ptr<YieldTermStructure>(new
                FlatForward(referenceDate, forwardRate)));
    } else if (rowNo>1 && colNo==2 && range(0,1).AsDouble()==1.0) {
        // vertical discount grid


        std::vector<Date> dates(rowNo);
        std::vector<DiscountFactor> discounts(rowNo);
        for (Size j = 0; j<rowNo; j++) {
            dates[j] = QlXlfOper(range(j, 0)).AsDate();
            discounts[j] = range(j, 1).AsDouble();
        }
        Date today=dates[0];

        return Handle<YieldTermStructure>(
            boost::shared_ptr<YieldTermStructure>(new
                DiscountCurve(dates, discounts)));
    } else if (rowNo==2 && colNo>1 && range(1,0).AsDouble()==1.0) {
        // horizontal discount grid


        std::vector<Date> dates(colNo);
        std::vector<DiscountFactor> discounts(colNo);
        for (Size j = 0; j<colNo; j++) {
            dates[j] = QlXlfOper(range(0, j)).AsDate();
            discounts[j] = range(1, j).AsDouble();
        }
        Date today=dates[0];

        return Handle<YieldTermStructure>(
            boost::shared_ptr<YieldTermStructure>(new
                DiscountCurve(dates, discounts)));
    } else if (rowNo>1 && colNo==2) {
        // vertical piecewise forward grid (annual continuos act/365)
        std::vector<Date> dates(rowNo);
        std::vector<Rate> forwards(rowNo);
        for (Size j = 0; j<rowNo; j++) {
            dates[j] = QlXlfOper(range(j, 0)).AsDate();
            forwards[j] = range(j, 1).AsDouble();
        }
        Date today=dates[0];

        return Handle<YieldTermStructure>(
            boost::shared_ptr<YieldTermStructure>(new
                PiecewiseFlatForward(dates, forwards)));
    } else if (rowNo==2 && colNo>1) {
        // horizontal piecewise forward grid (annual continuos act/365)
        std::vector<Date> dates(colNo);
        std::vector<Rate> forwards(colNo);
        for (Size j = 0; j<colNo; j++) {
            dates[j] = QlXlfOper(range(0, j)).AsDate();
            forwards[j] = range(1, j).AsDouble();
        }
        Date today=dates[0];

        return Handle<YieldTermStructure>(
            boost::shared_ptr<YieldTermStructure>(new
                PiecewiseFlatForward(dates, forwards)));
    } else
        QL_FAIL("Not a yield term structure range");

}

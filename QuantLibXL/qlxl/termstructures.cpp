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

/*! \file termstructures.cpp
    \brief QuantLib Excel yield term structure functions

*/

#include <qlxl/qlxlfoper.hpp>

extern "C"
{


    using namespace QuantLib;

    LPXLOPER EXCEL_EXPORT xlDiscount(XlfOper xlrefDate,
                                     XlfOper xltermStructure,
                                     XlfOper xldate,
                                     XlfOper xlallowExtrapolation) {
        EXCEL_BEGIN;

        WIZARD_NO_CALC;

        Date refDate = QlXlfOper(xlrefDate).AsDate();
        Handle<YieldTermStructure>
            termStructure =
            QlXlfOper(xltermStructure).AsTermStructure(refDate);
        Date date = QlXlfOper(xldate).AsDate();

        double result = termStructure->discount(date,
            xlallowExtrapolation.AsBool());
        return XlfOper(result);
        EXCEL_END;
    }

    LPXLOPER EXCEL_EXPORT xlZero(XlfOper xlrefDate,
                                 XlfOper xltermStructure,
                                 XlfOper xldate,
                                 XlfOper xlallowExtrapolation) {
        EXCEL_BEGIN;

        WIZARD_NO_CALC;

        Date refDate = QlXlfOper(xlrefDate).AsDate();
        Handle<YieldTermStructure>
            termStructure =
            QlXlfOper(xltermStructure).AsTermStructure(refDate);
        Date date = QlXlfOper(xldate).AsDate();

        double result = termStructure->zeroYield(date,
            xlallowExtrapolation.AsBool());
        return XlfOper(result);
        EXCEL_END;
    }

    LPXLOPER EXCEL_EXPORT xlForward(XlfOper xlrefDate,
                                    XlfOper xltermStructure,
                                    XlfOper xldate1,
                                    XlfOper xldate2,
                                    XlfOper xlallowExtrapolation) {
        EXCEL_BEGIN;

        WIZARD_NO_CALC;

        Date refDate = QlXlfOper(xlrefDate).AsDate();
        Handle<YieldTermStructure>
            termStructure =
            QlXlfOper(xltermStructure).AsTermStructure(refDate);
        Date date1 = QlXlfOper(xldate1).AsDate();
        Date date2 = QlXlfOper(xldate2).AsDate();

        double result = termStructure->forward(date1, date2,
            xlallowExtrapolation.AsBool());
        return XlfOper(result);
        EXCEL_END;
    }

}

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

/*! \file vols.cpp
    \brief QuantLib Excel volatility functions

*/

#include <qlxl/qlxlfoper.hpp>
#include <ql/Volatilities/localvolsurface.hpp>

extern "C"
{


    using namespace QuantLib;


    LPXLOPER EXCEL_EXPORT xlBlackVol(XlfOper xlrefDate,
                                     XlfOper xldate1,
                                     XlfOper xldate2,
                                     XlfOper xlstrike,
                                     XlfOper xlblackVolSurface,
                                     XlfOper interpolationType,
                                     XlfOper xlallowExtrapolation) {
        EXCEL_BEGIN;

        WIZARD_NO_CALC;

        Date refDate = QlXlfOper(xlrefDate).AsDate();
        Handle<BlackVolTermStructure> volSurface =
            QlXlfOper(xlblackVolSurface).AsBlackVolTermStructure(refDate,
            interpolationType.AsInt());
        Date date1 = QlXlfOper(xldate1).AsDate();
        Date date2 = QlXlfOper(xldate2).AsDate();
        double strike = xlstrike.AsDouble();

        double result = volSurface->blackForwardVol(date1, date2, strike,
            xlallowExtrapolation.AsBool());
        return XlfOper(result);
        EXCEL_END;
    }

    LPXLOPER EXCEL_EXPORT xlLocalVol(XlfOper xlrefDate,
                                     XlfOper xlunderlying,
                                     XlfOper xlevalDate,
                                     XlfOper xlassetLevel,
                                     XlfOper xldividendYield,
                                     XlfOper xlriskFree,
                                     XlfOper xlblackVolSurface,
                                     XlfOper xlinterpolationType,
                                     XlfOper xlallowExtrapolation) {
        EXCEL_BEGIN;

        WIZARD_NO_CALC;

        Date refDate    = QlXlfOper(xlrefDate).AsDate();
        double underlying = xlunderlying.AsDouble();

        Handle<YieldTermStructure> dividendTS =
            QlXlfOper(xldividendYield).AsTermStructure(refDate);
        Handle<YieldTermStructure> riskFreeTS =
            QlXlfOper(xlriskFree).AsTermStructure(refDate);
        Handle<BlackVolTermStructure> blackVolSurface =
            QlXlfOper(xlblackVolSurface).AsBlackVolTermStructure(refDate,
            xlinterpolationType.AsInt());

        double assetLevel = xlassetLevel.AsDouble();
        Date evalDate      = QlXlfOper(xlevalDate).AsDate();

        LocalVolSurface locVol(blackVolSurface,
            riskFreeTS, dividendTS, underlying);

        bool allowExtrapolation = xlallowExtrapolation.AsBool();
        double result = locVol.localVol(evalDate, assetLevel,
            allowExtrapolation);
        return XlfOper(result);
        EXCEL_END;
    }

}

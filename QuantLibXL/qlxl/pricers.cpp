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
/*! \file pricers.cpp
    \brief QuantLib Excel pricers

    \fullpath
    qlxl/%pricers.cpp
*/

// $Id$

#include <qlxl/qlxl.hpp>
#include <qlxl/pricers.hpp>

extern "C"
{

    using namespace QuantLib;
    using QuantLib::Pricers::EuropeanOption;
    using QuantLib::Pricers::McEuropean;
    using QuantLib::Pricers::FdEuropean;
    using QuantLib::Pricers::FdAmericanOption;

        
    LPXLOPER EXCEL_EXPORT xlEuropeanOption(
                        XlfOper xltype,
                        XlfOper xlunderlying,
                        XlfOper xlstrike,
                        XlfOper xldividendYield,
                        XlfOper xlriskFreeRate,
                        XlfOper xlmaturity,
                        XlfOper xlvolatility)
    {
        EXCEL_BEGIN;

        std::string temp(xltype.AsString());
        temp = StringFormatter::toLowercase(temp);

        Option::Type type;
        if (temp == "c" || temp == "call") {
            type = Option::Call;
        } else if (temp == "p" || temp == "put") {
            type = Option::Put;
        } else if (temp == "s" || temp == "straddle") {
            type = Option::Straddle;
        } else
            throw Error("Unknown option type");

        double underlying    = xlunderlying.AsDouble();
        double strike        = xlstrike.AsDouble();
        double dividendYield = xldividendYield.AsDouble();
        double riskFreeRate  = xlriskFreeRate.AsDouble();
        double maturity      = xlmaturity.AsDouble();
        double volatility    = xlvolatility.AsDouble();




        EuropeanOption eur(type, underlying, strike, dividendYield,
           riskFreeRate, maturity, volatility);
        double results[7];
        results[0] = eur.value();
        results[1] = eur.delta();
        results[2] = eur.gamma();
        results[3] = eur.theta();
        results[4] = eur.vega();
        results[5] = eur.rho();
        results[6] = eur.dividendRho();

        return XlfOper(1,7,results);
        EXCEL_END;
    }

    LPXLOPER EXCEL_EXPORT xlEuropeanOption_FD(
                        XlfOper xltype,
                        XlfOper xlunderlying,
                        XlfOper xlstrike,
                        XlfOper xldividendYield,
                        XlfOper xlriskFreeRate,
                        XlfOper xlmaturity,
                        XlfOper xlvolatility,
                        XlfOper xltimeSteps,
                        XlfOper xlgridPoints)
    {
        EXCEL_BEGIN;

        std::string temp(xltype.AsString());
        temp = StringFormatter::toLowercase(temp);

        Option::Type type;
        if (temp == "c" || temp == "call") {
            type = Option::Call;
        } else if (temp == "p" || temp == "put") {
            type = Option::Put;
        } else if (temp == "s" || temp == "straddle") {
            type = Option::Straddle;
        } else
            throw Error("Unknown option type");

        double underlying    = xlunderlying.AsDouble();
        double strike        = xlstrike.AsDouble();
        double dividendYield = xldividendYield.AsDouble();
        double riskFreeRate  = xlriskFreeRate.AsDouble();
        double maturity      = xlmaturity.AsDouble();
        double volatility    = xlvolatility.AsDouble();
        Size timeSteps       = xltimeSteps.AsDouble();
        Size gridPoints      = xlgridPoints.AsDouble();




        FdEuropean eur(type, underlying, strike, dividendYield,
           riskFreeRate, maturity, volatility, timeSteps, gridPoints);
        double results[7];
        results[0] = eur.value();
        results[1] = eur.delta();
        results[2] = eur.gamma();
        results[3] = eur.theta();
        results[4] = eur.vega();
        results[5] = eur.rho();
        results[6] = eur.dividendRho();

        return XlfOper(1,7,results);
        EXCEL_END;
    }

    LPXLOPER EXCEL_EXPORT xlEuropeanOption_MC(
                        XlfOper xltype,
                        XlfOper xlunderlying,
                        XlfOper xlstrike,
                        XlfOper xldividendYield,
                        XlfOper xlriskFreeRate,
                        XlfOper xlmaturity,
                        XlfOper xlvolatility,
                        XlfOper xlantitheticVariance,
                        XlfOper xlsamples)
    {
        EXCEL_BEGIN;

        std::string temp(xltype.AsString());
        temp = StringFormatter::toLowercase(temp);

        Option::Type type;
        if (temp == "c" || temp == "call") {
            type = Option::Call;
        } else if (temp == "p" || temp == "put") {
            type = Option::Put;
        } else if (temp == "s" || temp == "straddle") {
            type = Option::Straddle;
        } else
            throw Error("Unknown option type");

        double underlying       = xlunderlying.AsDouble();
        double strike           = xlstrike.AsDouble();
        double dividendYield    = xldividendYield.AsDouble();
        double riskFreeRate     = xlriskFreeRate.AsDouble();
        double maturity         = xlmaturity.AsDouble();
        double volatility       = xlvolatility.AsDouble();
        bool antitheticVariance = xlantitheticVariance.AsBool();
        Size samples            = xlsamples.AsDouble();




        McEuropean eur(type, underlying, strike, dividendYield,
           riskFreeRate, maturity, volatility, antitheticVariance);
        double results[2];
        results[0] = eur.valueWithSamples(samples);
        results[1] = eur.errorEstimate();

        return XlfOper(2,1,results);
        EXCEL_END;
    }




    LPXLOPER EXCEL_EXPORT xlAmericanOption_FD(
                        XlfOper xltype,
                        XlfOper xlunderlying,
                        XlfOper xlstrike,
                        XlfOper xldividendYield,
                        XlfOper xlriskFreeRate,
                        XlfOper xlmaturity,
                        XlfOper xlvolatility,
                        XlfOper xltimeSteps,
                        XlfOper xlgridPoints)
    {
        EXCEL_BEGIN;

        std::string temp(xltype.AsString());
        temp = StringFormatter::toLowercase(temp);

        Option::Type type;
        if (temp == "c" || temp == "call") {
            type = Option::Call;
        } else if (temp == "p" || temp == "put") {
            type = Option::Put;
        } else if (temp == "s" || temp == "straddle") {
            type = Option::Straddle;
        } else
            throw Error("Unknown option type");

        double underlying    = xlunderlying.AsDouble();
        double strike        = xlstrike.AsDouble();
        double dividendYield = xldividendYield.AsDouble();
        double riskFreeRate  = xlriskFreeRate.AsDouble();
        double maturity      = xlmaturity.AsDouble();
        double volatility    = xlvolatility.AsDouble();
        Size timeSteps       = xltimeSteps.AsDouble();
        Size gridPoints      = xlgridPoints.AsDouble();




        FdAmericanOption eur(type, underlying, strike, dividendYield,
           riskFreeRate, maturity, volatility, timeSteps, gridPoints);
        double results[7];
        results[0] = eur.value();
        results[1] = eur.delta();
        results[2] = eur.gamma();
        results[3] = eur.theta();
        results[4] = eur.vega();
        results[5] = eur.rho();
        results[6] = eur.dividendRho();

        return XlfOper(1,7,results);
        EXCEL_END;
    }





}

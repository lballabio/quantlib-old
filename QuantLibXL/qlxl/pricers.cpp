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

#include <qlxl/qlxlfoper.hpp>

extern "C"
{

    using namespace QuantLib;
    using QuantLib::Pricers::EuropeanOption;
    using QuantLib::Pricers::FdEuropean;
    using QuantLib::Pricers::McEuropean;
    using QuantLib::Pricers::CliquetOption;
    using QuantLib::Pricers::McCliquetOption;
    using QuantLib::Pricers::PerformanceOption;
    using QuantLib::Pricers::McPerformanceOption;
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

        Option::Type type = QlXlfOper(xltype).AsOptionType();
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

        Option::Type type = QlXlfOper(xltype).AsOptionType();
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

        Option::Type type = QlXlfOper(xltype).AsOptionType();
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



    LPXLOPER EXCEL_EXPORT xlCliquetOption(
                        XlfOper xltype,
                        XlfOper xlunderlying,
                        XlfOper xlmoneyness,
                        XlfOper xldividendYield,
                        XlfOper xlriskFreeRate,
                        XlfOper xltimes,
                        XlfOper xlvolatility)
    {
        EXCEL_BEGIN;

        Option::Type type = QlXlfOper(xltype).AsOptionType();
        double underlying       = xlunderlying.AsDouble();
        double moneyness           = xlmoneyness.AsDouble();
        std::vector<double> dividendYield    = xldividendYield.AsDoubleVector();
        std::vector<double> riskFreeRate     = xlriskFreeRate.AsDoubleVector();
        std::vector<double> volatility       = xlvolatility.AsDoubleVector();
        std::vector<Time> times         = xltimes.AsDoubleVector();


        CliquetOption cliquet(type, underlying, moneyness, dividendYield,
           riskFreeRate, times, volatility);
        double results[7];
        results[0] = cliquet.value();
        results[1] = cliquet.delta();
        results[2] = cliquet.gamma();
        results[3] = cliquet.theta();
        results[4] = cliquet.vega();
        results[5] = cliquet.rho();
        results[6] = cliquet.dividendRho();

        return XlfOper(1,7,results);
        EXCEL_END;
    }

    
    LPXLOPER EXCEL_EXPORT xlCliquetOption_MC(
                        XlfOper xltype,
                        XlfOper xlunderlying,
                        XlfOper xlmoneyness,
                        XlfOper xldividendYield,
                        XlfOper xlriskFreeRate,
                        XlfOper xltimes,
                        XlfOper xlvolatility,
                        XlfOper xlantitheticVariance,
                        XlfOper xlsamples)
    {
        EXCEL_BEGIN;

        Option::Type type = QlXlfOper(xltype).AsOptionType();
        double underlying       = xlunderlying.AsDouble();
        double moneyness           = xlmoneyness.AsDouble();
        std::vector<double> dividendYield    = xldividendYield.AsDoubleVector();
        std::vector<double> riskFreeRate     = xlriskFreeRate.AsDoubleVector();
        std::vector<Time> times         = xltimes.AsDoubleVector();
        std::vector<double> volatility       = xlvolatility.AsDoubleVector();
        bool antitheticVariance = xlantitheticVariance.AsBool();
        Size samples            = xlsamples.AsDouble();

        McCliquetOption cliquet(type, underlying, moneyness, dividendYield,
           riskFreeRate, times, volatility, antitheticVariance);
        double results[2];
        results[0] = cliquet.valueWithSamples(samples);
        results[1] = cliquet.errorEstimate();

        return XlfOper(2,1,results);
        EXCEL_END;
    }


    
    LPXLOPER EXCEL_EXPORT xlPerformanceOption(
                        XlfOper xltype,
                        XlfOper xlunderlying,
                        XlfOper xlmoneyness,
                        XlfOper xldividendYield,
                        XlfOper xlriskFreeRate,
                        XlfOper xltimes,
                        XlfOper xlvolatility)
    {
        EXCEL_BEGIN;

        Option::Type type = QlXlfOper(xltype).AsOptionType();
        double underlying       = xlunderlying.AsDouble();
        double moneyness           = xlmoneyness.AsDouble();
        std::vector<double> dividendYield    = xldividendYield.AsDoubleVector();
        std::vector<double> riskFreeRate     = xlriskFreeRate.AsDoubleVector();
        std::vector<Time> times         = xltimes.AsDoubleVector();
        std::vector<double> volatility       = xlvolatility.AsDoubleVector();


        PerformanceOption perfCliquet(type, underlying, moneyness, dividendYield,
           riskFreeRate, times, volatility);
        double results[7];
        results[0] = perfCliquet.value();
        results[1] = perfCliquet.delta();
        results[2] = perfCliquet.gamma();
        results[3] = perfCliquet.theta();
        results[4] = perfCliquet.vega();
        results[5] = perfCliquet.rho();
        results[6] = perfCliquet.dividendRho();

        return XlfOper(1,7,results);
        EXCEL_END;
    }

    
    LPXLOPER EXCEL_EXPORT xlPerformanceOption_MC(
                        XlfOper xltype,
                        XlfOper xlunderlying,
                        XlfOper xlmoneyness,
                        XlfOper xldividendYield,
                        XlfOper xlriskFreeRate,
                        XlfOper xltimes,
                        XlfOper xlvolatility,
                        XlfOper xlantitheticVariance,
                        XlfOper xlsamples)
    {
        EXCEL_BEGIN;

        Option::Type type = QlXlfOper(xltype).AsOptionType();
        double underlying       = xlunderlying.AsDouble();
        double moneyness           = xlmoneyness.AsDouble();
        std::vector<double> dividendYield    = xldividendYield.AsDoubleVector();
        std::vector<double> riskFreeRate     = xlriskFreeRate.AsDoubleVector();
        std::vector<Time> times         = xltimes.AsDoubleVector();
        std::vector<double> volatility       = xlvolatility.AsDoubleVector();
        bool antitheticVariance = xlantitheticVariance.AsBool();
        Size samples            = xlsamples.AsDouble();




        McPerformanceOption perfCliquet(type, underlying, moneyness, dividendYield,
           riskFreeRate, times, volatility, antitheticVariance);
        double results[2];
        results[0] = perfCliquet.valueWithSamples(samples);
        results[1] = perfCliquet.errorEstimate();

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

        Option::Type type = QlXlfOper(xltype).AsOptionType();
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

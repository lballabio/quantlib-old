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

/*! \file pricers.cpp
    \brief QuantLib Excel pricers
*/

#include <qlxl/qlxlfoper.hpp>
#include <ql/DayCounters/actual365.hpp>
#include <ql/Pricers/fdeuropean.hpp>
#include <ql/Pricers/fdamericanoption.hpp>
#include <ql/Pricers/cliquetoption.hpp>
#include <ql/Pricers/mccliquetoption.hpp>
#include <ql/Pricers/performanceoption.hpp>
#include <ql/Pricers/mcperformanceoption.hpp>

extern "C"
{

    using namespace QuantLib;

        
    LPXLOPER EXCEL_EXPORT xlEuropeanOption_FD(
                        XlfOper xltype,
                        XlfOper xlunderlying,
                        XlfOper xlstrike,
                        XlfOper xldividendYield,
                        XlfOper xlriskFreeRate,
                        XlfOper xlvalueDate,
                        XlfOper xlmaturityDate,
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
        Date valueDate       = QlXlfOper(xlvalueDate).AsDate();
        Date maturityDate    = QlXlfOper(xlmaturityDate).AsDate();
        double maturity      = Actual365().yearFraction(valueDate, maturityDate);
        double volatility    = xlvolatility.AsDouble();
        Size timeSteps       = xltimeSteps.AsInt();
        Size gridPoints      = xlgridPoints.AsInt();




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


        CliquetOptionPricer cliquet(type, underlying, moneyness, dividendYield,
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
                        XlfOper xlrefDate,
                        XlfOper xlfixingDates,
                        XlfOper xlvolatility,
                        XlfOper xlinterpolationType,
                        XlfOper xlaccruedCoupon,
                        XlfOper xllastFixing,
                        XlfOper xllocalCap,
                        XlfOper xllocalFloor,
                        XlfOper xlglobalCap,
                        XlfOper xlglobalFloor,
                        XlfOper xlredemptionOnly,
                        XlfOper xlantitheticVariance,
                        XlfOper xlsamples)
    {
        EXCEL_BEGIN;

        Option::Type type = QlXlfOper(xltype).AsOptionType();
        double underlying = xlunderlying.AsDouble();
        double moneyness  = xlmoneyness.AsDouble();

        Date refDate = QlXlfOper(xlrefDate).AsDate();

        RelinkableHandle<TermStructure> dividendYieldTS =
            QlXlfOper(xldividendYield).AsTermStructure(refDate);
        RelinkableHandle<TermStructure> riskFreeRateTS  =
            QlXlfOper(xlriskFreeRate).AsTermStructure(refDate);
        RelinkableHandle<BlackVolTermStructure> volTS =
            QlXlfOper(xlvolatility).AsBlackVolTermStructure(refDate,
            xlinterpolationType.AsInt());

        std::vector<Date> fixingDates=QlXlfOper(xlfixingDates).AsDateVector();
        std::vector<Time> fixingTimes(fixingDates.size());
        std::vector<Time> dividends(fixingDates.size());
        std::vector<Time> rates(fixingDates.size());
        std::vector<Time> vols(fixingDates.size());
        fixingTimes[0] = riskFreeRateTS->dayCounter().yearFraction(
            refDate, fixingDates[0]);
        dividends[0]   = dividendYieldTS->forward(
            refDate, fixingDates[0]);
        rates[0]       = riskFreeRateTS->forward(
            refDate, fixingDates[0]);
        vols[0]        = volTS->blackForwardVol(
            refDate, fixingDates[0], underlying);
        for (Size i = 1; i<fixingDates.size(); i++) {
            fixingTimes[i] = riskFreeRateTS->dayCounter().yearFraction(
                refDate, fixingDates[i]);
            dividends[i]   = dividendYieldTS->forward(
                fixingDates[i-1], fixingDates[i]);
            rates[i]       = riskFreeRateTS->forward(
                fixingDates[i-1], fixingDates[i]);
            vols[i]        = volTS->blackForwardVol(
                fixingDates[i-1], fixingDates[i], underlying);
        }


        double accruedCoupon = xlaccruedCoupon.AsDouble();
        if (accruedCoupon<0)   accruedCoupon = Null<double>();

        double lastFixing    = xllastFixing.AsDouble();
        if (lastFixing<0)      lastFixing = Null<double>();

        double localCap      = xllocalCap.AsDouble();
        if (localCap<0)        localCap = Null<double>();

        double localFloor    = xllocalFloor.AsDouble();
        if (localFloor<0)      localFloor = Null<double>();

        double globalCap     = xlglobalCap.AsDouble();
        if (globalCap<0)       globalCap = Null<double>();

        double globalFloor   = xlglobalFloor.AsDouble();
        if (globalFloor<0)     globalFloor = Null<double>();

        bool redemptionOnly     = xlredemptionOnly.AsBool();
        bool antitheticVariance = xlantitheticVariance.AsBool();
        int samples            = xlsamples.AsInt();

        McCliquetOption cliquet(type, underlying, moneyness, dividendYieldTS,
           riskFreeRateTS, volTS, fixingTimes, accruedCoupon, lastFixing,
           localCap, localFloor, globalCap, globalFloor, redemptionOnly);
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
                        XlfOper xlrefDate,
                        XlfOper xlfixingDates,
                        XlfOper xlvolatility,
                        XlfOper xlinterpolationType,
                        XlfOper xlantitheticVariance,
                        XlfOper xlsamples)
    {
        EXCEL_BEGIN;

        Option::Type type = QlXlfOper(xltype).AsOptionType();
        double underlying       = xlunderlying.AsDouble();
        double moneyness           = xlmoneyness.AsDouble();
        Size samples            = xlsamples.AsInt();

        Date refDate = QlXlfOper(xlrefDate).AsDate();

        RelinkableHandle<TermStructure> dividendYieldTS =
            QlXlfOper(xldividendYield).AsTermStructure(refDate);
        RelinkableHandle<TermStructure> riskFreeRateTS  =
            QlXlfOper(xlriskFreeRate).AsTermStructure(refDate);
        RelinkableHandle<BlackVolTermStructure> volTS =
            QlXlfOper(xlvolatility).AsBlackVolTermStructure(refDate,
            xlinterpolationType.AsInt());
        
        std::vector<Date> fixingDates=QlXlfOper(xlfixingDates).AsDateVector();
        std::vector<Time> fixingTimes(fixingDates.size());
        for (Size i = 0; i<fixingDates.size(); i++) {
            fixingTimes[i] = riskFreeRateTS->dayCounter().yearFraction(
                refDate, fixingDates[i]);
        }



        McPerformanceOption perfCliquet(type, underlying, moneyness, dividendYieldTS,
           riskFreeRateTS, volTS, fixingTimes);
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
                        XlfOper xlvalueDate,
                        XlfOper xlmaturityDate,
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
        Date valueDate       = QlXlfOper(xlvalueDate).AsDate();
        Date maturityDate    = QlXlfOper(xlmaturityDate).AsDate();
        double maturity      = Actual365().yearFraction(valueDate, maturityDate);
        double volatility    = xlvolatility.AsDouble();
        Size timeSteps       = xltimeSteps.AsInt();
        Size gridPoints      = xlgridPoints.AsInt();




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

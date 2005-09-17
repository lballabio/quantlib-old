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
#include <ql/Instruments/europeanoption.hpp>
#include <ql/Instruments/payoffs.hpp>
#include <ql/Pricers/mccliquetoption.hpp>
#include <ql/Pricers/mcperformanceoption.hpp>
#include <ql/PricingEngines/Vanilla/fdeuropeanengine.hpp>
#include <ql/PricingEngines/Vanilla/fdamericanengine.hpp>
#include <ql/Processes/blackscholesprocess.hpp>

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
        
    	WIZARD_NO_CALC;
        
        Option::Type type    = QlXlfOper(xltype).AsOptionType();
        double underlying    = xlunderlying.AsDouble();
        double strike        = xlstrike.AsDouble();
        Date valueDate       = QlXlfOper(xlvalueDate).AsDate();
        Date maturityDate    = QlXlfOper(xlmaturityDate).AsDate();
        double maturity      = Actual365Fixed().yearFraction(valueDate, maturityDate);
        Size timeSteps       = xltimeSteps.AsInt();
        Size gridPoints      = xlgridPoints.AsInt();
        
        DayCounter dc = Actual365Fixed();
        
        boost::shared_ptr<Quote> spot(new SimpleQuote(underlying));
        
        Handle<YieldTermStructure> dividendYieldTS =
            QlXlfOper(xldividendYield).AsTermStructure(valueDate);
        Handle<YieldTermStructure> riskFreeRateTS  =
            QlXlfOper(xlriskFreeRate).AsTermStructure(valueDate);
        Handle<BlackVolTermStructure> volTS =
            QlXlfOper(xlvolatility).AsBlackVolTermStructure(valueDate,1);
        
        boost::shared_ptr<GenericStochasticProcess> stochProcess(
            new BlackScholesProcess(Handle<Quote>(spot),
                                    dividendYieldTS,
                                    riskFreeRateTS,
                                    volTS));
        
        boost::shared_ptr<StrikedTypePayoff> payoff(
            new PlainVanillaPayoff(type, strike));
        
        boost::shared_ptr<Exercise> exercise(
            new EuropeanExercise(maturityDate));
        
        boost::shared_ptr<PricingEngine> engine(
            new FDEuropeanEngine(timeSteps, gridPoints));
        
        EuropeanOption option(stochProcess, payoff, exercise, engine);
        
        double results[4];
        results[0] = option.NPV();
        results[1] = option.delta();
        results[2] = option.gamma();
        results[3] = option.theta();
	// it looks like the following greeks are not provided
        // results[4] = option.vega();
        // results[5] = option.rho();
        // results[6] = option.dividendRho();
        
        return XlfOper(1,sizeof(results)/sizeof(results[0]),results);
        
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

    	WIZARD_NO_CALC;

        Option::Type type = QlXlfOper(xltype).AsOptionType();
        double underlying = xlunderlying.AsDouble();
        double moneyness  = xlmoneyness.AsDouble();

        Date refDate = QlXlfOper(xlrefDate).AsDate();

        Handle<YieldTermStructure> dividendYieldTS =
            QlXlfOper(xldividendYield).AsTermStructure(refDate);
        Handle<YieldTermStructure> riskFreeRateTS  =
            QlXlfOper(xlriskFreeRate).AsTermStructure(refDate);
        Handle<BlackVolTermStructure> volTS =
            QlXlfOper(xlvolatility).AsBlackVolTermStructure(refDate,
            xlinterpolationType.AsInt());

        std::vector<Date> fixingDates=QlXlfOper(xlfixingDates).AsDateVector();
        std::vector<Time> fixingTimes(fixingDates.size());
        std::vector<Time> dividends(fixingDates.size());
        std::vector<Time> rates(fixingDates.size());
        std::vector<Time> vols(fixingDates.size());
        fixingTimes[0] = riskFreeRateTS->dayCounter().yearFraction(
            refDate, fixingDates[0]);
        dividends[0]   = dividendYieldTS->forwardRate(
            refDate, fixingDates[0],
            QuantLib::Actual365Fixed(), Continuous, NoFrequency);
        rates[0]       = riskFreeRateTS->forwardRate(
            refDate, fixingDates[0],
            QuantLib::Actual365Fixed(), Continuous, NoFrequency);
        vols[0]        = volTS->blackForwardVol(
            refDate, fixingDates[0], underlying);
        for (Size i = 1; i<fixingDates.size(); i++) {
            fixingTimes[i] = riskFreeRateTS->dayCounter().yearFraction(
                refDate, fixingDates[i]);
            dividends[i]   = dividendYieldTS->forwardRate(
                fixingDates[i-1], fixingDates[i],
                QuantLib::Actual365Fixed(), Continuous, NoFrequency);
            rates[i]       = riskFreeRateTS->forwardRate(
                fixingDates[i-1], fixingDates[i],
                QuantLib::Actual365Fixed(), Continuous, NoFrequency);
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

    	WIZARD_NO_CALC;

        Option::Type type = QlXlfOper(xltype).AsOptionType();
        double underlying       = xlunderlying.AsDouble();
        double moneyness           = xlmoneyness.AsDouble();
        Size samples            = xlsamples.AsInt();

        Date refDate = QlXlfOper(xlrefDate).AsDate();

        Handle<YieldTermStructure> dividendYieldTS =
            QlXlfOper(xldividendYield).AsTermStructure(refDate);
        Handle<YieldTermStructure> riskFreeRateTS  =
            QlXlfOper(xlriskFreeRate).AsTermStructure(refDate);
        Handle<BlackVolTermStructure> volTS =
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

    	WIZARD_NO_CALC;
        
        Option::Type type    = QlXlfOper(xltype).AsOptionType();
        double underlying    = xlunderlying.AsDouble();
        double strike        = xlstrike.AsDouble();
        Date valueDate       = QlXlfOper(xlvalueDate).AsDate();
        Date maturityDate    = QlXlfOper(xlmaturityDate).AsDate();
        double maturity      = Actual365Fixed().yearFraction(valueDate, maturityDate);
        Size timeSteps       = xltimeSteps.AsInt();
        Size gridPoints      = xlgridPoints.AsInt();
        
        DayCounter dc = Actual365Fixed();
        
        boost::shared_ptr<Quote> spot(new SimpleQuote(underlying));
        
        Handle<YieldTermStructure> dividendYieldTS =
            QlXlfOper(xldividendYield).AsTermStructure(valueDate);
        Handle<YieldTermStructure> riskFreeRateTS  =
            QlXlfOper(xlriskFreeRate).AsTermStructure(valueDate);
        Handle<BlackVolTermStructure> volTS =
            QlXlfOper(xlvolatility).AsBlackVolTermStructure(valueDate,1);
        
        boost::shared_ptr<GenericStochasticProcess> stochProcess(
            new BlackScholesProcess(Handle<Quote>(spot),
                                    dividendYieldTS,
                                    riskFreeRateTS,
                                    volTS));
        
        boost::shared_ptr<StrikedTypePayoff> payoff(
            new PlainVanillaPayoff(type, strike));
        
        boost::shared_ptr<Exercise> exercise(
            new AmericanExercise(valueDate, maturityDate));
        
        boost::shared_ptr<PricingEngine> engine(
            new FDEuropeanEngine(timeSteps, gridPoints));
        
        VanillaOption option(stochProcess, payoff, exercise, engine);
        
        double results[4];
        results[0] = option.NPV();
        results[1] = option.delta();
        results[2] = option.gamma();
        results[3] = option.theta();
	// it looks like the following greeks are not provided
        // results[4] = option.vega();
        // results[5] = option.rho();
        // results[6] = option.dividendRho();
        
        return XlfOper(1,sizeof(results)/sizeof(results[0]),results);
        
        EXCEL_END;
    }
    
}

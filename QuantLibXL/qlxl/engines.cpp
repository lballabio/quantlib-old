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

/*! \file engines.cpp
    \brief QuantLib Excel pricers based on the engine framework

    \fullpath
    qlxl/%engines.cpp
*/

#include <qlxl/qlxl.hpp>
#include <qlxl/qlxlfoper.hpp>
#include <ql/PricingEngines/vanillaengines.hpp>
#include <ql/PricingEngines/mceuropeanengine.hpp>
#include <ql/PricingEngines/quantoengines.hpp>
#include <ql/PricingEngines/forwardengines.hpp>

extern "C"
{

    using namespace QuantLib;

    LPXLOPER EXCEL_EXPORT xlEuropeanOption(
                        XlfOper xltype,
                        XlfOper xlunderlying,
                        XlfOper xlstrike,
                        XlfOper xldividendYield,
                        XlfOper xlriskFree,
                        XlfOper xlrefDate,
                        XlfOper xlmaturityDate,
                        XlfOper xlvolatility,
                        XlfOper xlinterpolationType)
    {
        EXCEL_BEGIN;

        Handle<AnalyticEuropeanEngine> engine(new
            AnalyticEuropeanEngine);

        VanillaOption::arguments* arguments =
            dynamic_cast<VanillaOption::arguments*>(
                engine->arguments());

        Date refDate = QlXlfOper(xlrefDate).AsDate();

        arguments->payoff = Handle<Payoff>(
            new PlainVanillaPayoff(QlXlfOper(xltype).AsOptionType(),
                                   xlstrike.AsDouble()));
        arguments->underlying = xlunderlying.AsDouble();
        arguments->dividendTS = QlXlfOper(xldividendYield).AsTermStructure(refDate);
        arguments->riskFreeTS = QlXlfOper(xlriskFree).AsTermStructure(refDate);
        arguments->exerciseType = Exercise::European;
        Date maturityDate = QlXlfOper(xlmaturityDate).AsDate();
        arguments->maturity = arguments->riskFreeTS->dayCounter().yearFraction(
            refDate, maturityDate);
        arguments->volTS = QlXlfOper(xlvolatility).AsBlackVolTermStructure(refDate,
            xlinterpolationType.AsInt());

        arguments->validate();
        engine->calculate();

        const VanillaOption::results* vResults =
            dynamic_cast<const VanillaOption::results*>(
                engine->results());
        double results[8];
        results[0] = vResults->value;
        results[1] = vResults->delta;
        results[2] = vResults->gamma;
        results[3] = vResults->theta;
        results[4] = vResults->vega;
        results[5] = vResults->rho;
        results[6] = vResults->dividendRho;
        results[7] = vResults->strikeSensitivity;

        return XlfOper(1,8,results);
        EXCEL_END;
    }



    LPXLOPER EXCEL_EXPORT xlEuropeanOption_MC(
                        XlfOper xltype,
                        XlfOper xlunderlying,
                        XlfOper xlstrike,
                        XlfOper xldividendYield,
                        XlfOper xlriskFree,
                        XlfOper xlrefDate,
                        XlfOper xlmaturityDate,
                        XlfOper xlvolatility,
                        XlfOper xlantitheticVariance,
                        XlfOper xlsamples)
    {
        EXCEL_BEGIN;

        // some of them hard coded for the time being
        Size maxTimeStepPerYear = Null<Size>();
        maxTimeStepPerYear = 1;
        bool antitheticVariate = xlantitheticVariance.AsBool();
        bool controlVariate = false;
        Size nSamples= Null<Size>();
        nSamples= xlsamples.AsInt();
        double tolerance = Null<double>();
        Size maxSamples = Null<Size>();
        long mcSeed = 0;

        Handle<PricingEngine> engine(new
            MCEuropeanEngine<PseudoRandom>(maxTimeStepPerYear,
                                           antitheticVariate, controlVariate, 
                                           nSamples, tolerance, 
                                           maxSamples, mcSeed));

        VanillaOption::arguments* arguments =
            dynamic_cast<VanillaOption::arguments*>(
                engine->arguments());

        Date refDate = QlXlfOper(xlrefDate).AsDate();

        arguments->payoff = Handle<Payoff>(
            new PlainVanillaPayoff(QlXlfOper(xltype).AsOptionType(),
                                   xlstrike.AsDouble()));
        arguments->underlying = xlunderlying.AsDouble();
        arguments->dividendTS = QlXlfOper(xldividendYield).AsTermStructure(refDate);
        arguments->riskFreeTS = QlXlfOper(xlriskFree).AsTermStructure(refDate);
        arguments->exerciseType = Exercise::European;
        Date maturityDate = QlXlfOper(xlmaturityDate).AsDate();
        arguments->maturity = arguments->riskFreeTS->dayCounter().yearFraction(
            refDate, maturityDate);
        // hard coded for the time being
        int interpolationType = 2;
        arguments->volTS = QlXlfOper(xlvolatility).AsBlackVolTermStructure(refDate,
            interpolationType);

        arguments->validate();
        engine->calculate();

        const VanillaOption::results* vResults =
            dynamic_cast<const VanillaOption::results*>(
                engine->results());
        double results[2];
        results[0] = vResults->value;
        results[1] = vResults->errorEstimate;

        return XlfOper(2, 1, results);
        EXCEL_END;
    }

    
    LPXLOPER EXCEL_EXPORT xlQuantoEuropeanOption(
                        XlfOper xltype,
                        XlfOper xlunderlying,
                        XlfOper xlstrike,
                        XlfOper xldividendYield,
                        XlfOper xlriskFree,
                        XlfOper xlrefDate,
                        XlfOper xlmaturityDate,
                        XlfOper xlvolatility,
                        XlfOper xlinterpolationType,
                        XlfOper xlforeignRiskFreeRate,
                        XlfOper xlexchangeVolatility,
                        XlfOper xlcorrelation)
    {
        EXCEL_BEGIN;

        Handle<AnalyticEuropeanEngine> baseEngine(new
            AnalyticEuropeanEngine);

        Handle<QuantoEngine<VanillaOption::arguments,
                            VanillaOption::results> >
            quantoEngine(new QuantoEngine<VanillaOption::arguments,
                                          VanillaOption::results>(baseEngine));

        QuantoOptionArguments<VanillaOption::arguments>* arguments =
            dynamic_cast
            <QuantoOptionArguments<VanillaOption::arguments>*>(
            quantoEngine->arguments());

        Date refDate = QlXlfOper(xlrefDate).AsDate();

        arguments->payoff = Handle<Payoff>(
            new PlainVanillaPayoff(QlXlfOper(xltype).AsOptionType(),
                                   xlstrike.AsDouble()));
        arguments->underlying = xlunderlying.AsDouble();
        arguments->dividendTS = QlXlfOper(xldividendYield).AsTermStructure(refDate);
        arguments->riskFreeTS = QlXlfOper(xlriskFree).AsTermStructure(refDate);
        arguments->stoppingTimes = std::vector<Time>();
        arguments->exerciseType = Exercise::European;
        Date maturityDate = QlXlfOper(xlmaturityDate).AsDate();
        arguments->maturity = arguments->riskFreeTS->dayCounter().yearFraction(
            refDate, maturityDate);
        arguments->volTS = QlXlfOper(xlvolatility).AsBlackVolTermStructure(refDate,
            xlinterpolationType.AsInt());
        arguments->foreignRiskFreeTS =
            QlXlfOper(xlforeignRiskFreeRate).AsTermStructure(refDate);
        arguments->exchRateVolTS =
            QlXlfOper(xlexchangeVolatility).AsBlackVolTermStructure(refDate,
            xlinterpolationType.AsInt());
        arguments->correlation = xlcorrelation.AsDouble();

        arguments->validate();
        quantoEngine->calculate();

        const VanillaOption::results* vResults =
            dynamic_cast<const VanillaOption::results*>(
                quantoEngine->results());
        double results[10];
        results[0] = vResults->value;
        results[1] = vResults->delta;
        results[2] = vResults->gamma;
        results[3] = vResults->theta;
        results[4] = vResults->vega;
        results[5] = vResults->rho;
        results[6] = vResults->dividendRho;

        const QuantoOptionResults<VanillaOption::results>* qResults =
            dynamic_cast<const QuantoOptionResults<VanillaOption::results>*>(
                quantoEngine->results());
        results[7] = qResults->qvega;
        results[8] = qResults->qrho;
        results[9] = qResults->qlambda;

        return XlfOper(1,10,results);
        EXCEL_END;
    }

    LPXLOPER EXCEL_EXPORT xlForwardEuropeanOption(
                        XlfOper xltype,
                        XlfOper xlunderlying,
                        XlfOper xlmoneyness,
                        XlfOper xldividendYield,
                        XlfOper xlriskFree,
                        XlfOper xlrefDate,
                        XlfOper xlresetDate,
                        XlfOper xlmaturityDate,
                        XlfOper xlvolatility,
                        XlfOper xlinterpolationType)
    {
        EXCEL_BEGIN;

        Handle<AnalyticEuropeanEngine> baseEngine(new
            AnalyticEuropeanEngine);
        Handle<ForwardEngine<VanillaOption::arguments,
                             VanillaOption::results> >
            forwardEngine(new ForwardEngine<VanillaOption::arguments,
                                            VanillaOption::results>(baseEngine));

        ForwardOptionArguments<VanillaOption::arguments>*
            arguments = dynamic_cast
            <ForwardOptionArguments<VanillaOption::arguments>*>(
            forwardEngine->arguments());

        Date refDate = QlXlfOper(xlrefDate).AsDate();

        arguments->underlying = xlunderlying.AsDouble();
        // dummy strike
        // ForwardOptionParameter should not include strike
        arguments->payoff = Handle<Payoff>(
            new PlainVanillaPayoff(QlXlfOper(xltype).AsOptionType(),
                                   arguments->underlying));
        arguments->moneyness = xlmoneyness.AsDouble();
        arguments->dividendTS = QlXlfOper(xldividendYield) .AsTermStructure(refDate);
        arguments->riskFreeTS = QlXlfOper(xlriskFree).AsTermStructure(refDate);
        arguments->resetDate = QlXlfOper(xlresetDate).AsDate();
        arguments->stoppingTimes = std::vector<Time>();
        arguments->exerciseType = Exercise::European;
        Date maturityDate = QlXlfOper(xlmaturityDate).AsDate();
        arguments->maturity = arguments->riskFreeTS->dayCounter().yearFraction(
            refDate, maturityDate);
        arguments->volTS = QlXlfOper(xlvolatility).AsBlackVolTermStructure(refDate,
            xlinterpolationType.AsInt());

        arguments->validate();
        forwardEngine->calculate();

        const VanillaOption::results* vResults =
            dynamic_cast<const VanillaOption::results*>(
                forwardEngine->results());
        double results[7];
        results[0] = vResults->value;
        results[1] = vResults->delta;
        results[2] = vResults->gamma;
        results[3] = vResults->theta;
        results[4] = vResults->vega;
        results[5] = vResults->rho;
        results[6] = vResults->dividendRho;

        return XlfOper(1,7,results);
        EXCEL_END;
    }



    LPXLOPER EXCEL_EXPORT xlPerformanceEuropeanOption(
                        XlfOper xltype,
                        XlfOper xlunderlying,
                        XlfOper xlmoneyness,
                        XlfOper xldividendYield,
                        XlfOper xlriskFree,
                        XlfOper xlrefDate,
                        XlfOper xlresetDate,
                        XlfOper xlmaturityDate,
                        XlfOper xlvolatility,
                        XlfOper xlinterpolationType)
    {
        EXCEL_BEGIN;

        Handle<AnalyticEuropeanEngine> baseEngine(new
            AnalyticEuropeanEngine);

        Handle<ForwardPerformanceEngine<VanillaOption::arguments,
                                        VanillaOption::results> >
            forwardPerformanceEngine(
                new ForwardPerformanceEngine<VanillaOption::arguments,
                                             VanillaOption::results>(
                    baseEngine));

        ForwardOptionArguments<VanillaOption::arguments>*
            arguments = dynamic_cast
            <ForwardOptionArguments<VanillaOption::arguments>*>(
            forwardPerformanceEngine->arguments());

        Date refDate = QlXlfOper(xlrefDate).AsDate();

        // underlying is needed to interpolate on the vol surface
        arguments->underlying = xlunderlying.AsDouble();
        // dummy strike
        // ForwardPerformanceOptionParameter should not include strike
        arguments->payoff = Handle<Payoff>(
            new PlainVanillaPayoff(QlXlfOper(xltype).AsOptionType(),
                                   arguments->underlying));
        arguments->moneyness = xlmoneyness.AsDouble();
        arguments->dividendTS = QlXlfOper(xldividendYield) .AsTermStructure(refDate);
        arguments->riskFreeTS = QlXlfOper(xlriskFree).AsTermStructure(refDate);
        arguments->resetDate = QlXlfOper(xlresetDate).AsDate();
        arguments->stoppingTimes = std::vector<Time>();
        arguments->exerciseType = Exercise::European;
        Date maturityDate = QlXlfOper(xlmaturityDate).AsDate();
        arguments->maturity = arguments->riskFreeTS->dayCounter().yearFraction(
            refDate, maturityDate);
        arguments->volTS = QlXlfOper(xlvolatility).AsBlackVolTermStructure(refDate,
            xlinterpolationType.AsInt());

        arguments->validate();
        forwardPerformanceEngine->calculate();

        const VanillaOption::results* vResults =
            dynamic_cast<const VanillaOption::results*>(
                forwardPerformanceEngine->results());
        double results[7];
        results[0] = vResults->value;
        results[1] = vResults->delta;
        results[2] = vResults->gamma;
        results[3] = vResults->theta;
        results[4] = vResults->vega;
        results[5] = vResults->rho;
        results[6] = vResults->dividendRho;

        return XlfOper(1,7,results);
        EXCEL_END;
    }

}

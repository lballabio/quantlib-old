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
/*! \file engines.cpp
    \brief QuantLib Excel pricers based on the engine framework

    \fullpath
    qlxl/%engines.cpp
*/

// $Id$

#include <qlxl/qlxl.hpp>
#include <qlxl/qlxlfoper.hpp>

extern "C"
{

    using namespace QuantLib;
    using QuantLib::PricingEngines::EuropeanAnalyticalEngine;

    using QuantLib::PricingEngines::VanillaOptionArguments;
    using QuantLib::PricingEngines::VanillaOptionResults;

    using QuantLib::PricingEngines::QuantoOptionArguments;
    using QuantLib::PricingEngines::QuantoOptionResults;
    using QuantLib::PricingEngines::QuantoVanillaAnalyticEngine;

    using QuantLib::PricingEngines::ForwardOptionArguments;
    using QuantLib::PricingEngines::ForwardVanillaAnalyticEngine;

    using QuantLib::PricingEngines::ForwardPerformanceVanillaAnalyticEngine;

    LPXLOPER EXCEL_EXPORT xlQuantoEuropeanOption(
                        XlfOper xltype,
                        XlfOper xlunderlying,
                        XlfOper xlstrike,
                        XlfOper xldividendYield,
                        XlfOper xlriskFreeRate,
                        XlfOper xlmaturity,
                        XlfOper xlvolatility,
                        XlfOper xlforeignRiskFreeRate,
                        XlfOper xlexchangeVolatility,
                        XlfOper xlcorrelation)
    {
        EXCEL_BEGIN;

        Handle<EuropeanAnalyticalEngine> baseEngine(new
            EuropeanAnalyticalEngine);
        Handle<QuantoVanillaAnalyticEngine> quantoEngine(new
            QuantoVanillaAnalyticEngine(baseEngine));

        PricingEngines::QuantoOptionArguments<VanillaOptionArguments>* arguments =
            dynamic_cast
            <QuantoOptionArguments<VanillaOptionArguments>*>(
            quantoEngine->arguments());

        arguments->type = QlXlfOper(xltype).AsOptionType();
        arguments->underlying = xlunderlying.AsDouble();
        arguments->strike = xlstrike.AsDouble();
        arguments->dividendYield = xldividendYield .AsDouble();
        arguments->riskFreeRate= xlriskFreeRate.AsDouble();
        arguments->residualTime = xlmaturity.AsDouble();
        arguments->volatility = xlvolatility.AsDouble();
        arguments->foreignRiskFreeRate =
            xlforeignRiskFreeRate.AsDouble();
        arguments->exchangeRateVolatility =
            xlexchangeVolatility.AsDouble();
        arguments->correlation = xlcorrelation.AsDouble();

        arguments->validate();
        quantoEngine->calculate();

        const VanillaOptionResults* vResults =
            dynamic_cast<const VanillaOptionResults*>(
                quantoEngine->results());
        double results[10];
        results[0] = vResults->value;
        results[1] = vResults->delta;
        results[2] = vResults->gamma;
        results[3] = vResults->theta;
        results[4] = vResults->vega;
        results[5] = vResults->rho;
        results[6] = vResults->dividendRho;

        const QuantoOptionResults<VanillaOptionResults>* qResults =
            dynamic_cast<const QuantoOptionResults<VanillaOptionResults>*>(
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
                        XlfOper xlriskFreeRate,
                        XlfOper xlresetTime,
                        XlfOper xlmaturity,
                        XlfOper xlvolatility)
    {
        EXCEL_BEGIN;

        Handle<EuropeanAnalyticalEngine> baseEngine(new
            EuropeanAnalyticalEngine);
        Handle<ForwardVanillaAnalyticEngine> forwardEngine(new
            ForwardVanillaAnalyticEngine(baseEngine));

        PricingEngines::ForwardOptionArguments<VanillaOptionArguments>*
            arguments = dynamic_cast
            <ForwardOptionArguments<VanillaOptionArguments>*>(
            forwardEngine->arguments());

        arguments->type = QlXlfOper(xltype).AsOptionType();
        arguments->underlying = xlunderlying.AsDouble();
        // dummy strike
        // ForwardOptionParameter shoul not include strike
        arguments->strike = arguments->underlying;
        arguments->moneyness = xlmoneyness.AsDouble();
        arguments->dividendYield = xldividendYield .AsDouble();
        arguments->riskFreeRate= xlriskFreeRate.AsDouble();
        arguments->resetTime = xlresetTime.AsDouble();
        arguments->residualTime = xlmaturity.AsDouble();
        arguments->volatility = xlvolatility.AsDouble();

        arguments->validate();
        forwardEngine->calculate();

        const VanillaOptionResults* vResults =
            dynamic_cast<const VanillaOptionResults*>(
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
                        XlfOper xlriskFreeRate,
                        XlfOper xlresetTime,
                        XlfOper xlmaturity,
                        XlfOper xlvolatility)
    {
        EXCEL_BEGIN;

        Handle<EuropeanAnalyticalEngine> baseEngine(new
            EuropeanAnalyticalEngine);
        Handle<ForwardPerformanceVanillaAnalyticEngine> performanceEngine(new
            ForwardPerformanceVanillaAnalyticEngine(baseEngine));

        PricingEngines::ForwardOptionArguments<VanillaOptionArguments>*
            arguments = dynamic_cast
            <ForwardOptionArguments<VanillaOptionArguments>*>(
            performanceEngine->arguments());

        arguments->type = QlXlfOper(xltype).AsOptionType();
        // dummy underlying
        // ForwardPerformanceOptionParameter should not include underlying
        arguments->underlying = xlunderlying.AsDouble();
        // dummy strike
        // ForwardPerformanceOptionParameter should not include strike
        arguments->strike = arguments->underlying;
        arguments->moneyness = xlmoneyness.AsDouble();
        arguments->dividendYield = xldividendYield .AsDouble();
        arguments->riskFreeRate= xlriskFreeRate.AsDouble();
        arguments->resetTime = xlresetTime.AsDouble();
        arguments->residualTime = xlmaturity.AsDouble();
        arguments->volatility = xlvolatility.AsDouble();

        arguments->validate();
        performanceEngine->calculate();

        const VanillaOptionResults* vResults =
            dynamic_cast<const VanillaOptionResults*>(
                performanceEngine->results());
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

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

    using QuantLib::PricingEngines::VanillaOptionParameters;
    using QuantLib::PricingEngines::VanillaOptionResults;

    using QuantLib::PricingEngines::QuantoOptionParameters;
    using QuantLib::PricingEngines::QuantoOptionResults;
    using QuantLib::PricingEngines::QuantoVanillaAnalyticEngine;

    using QuantLib::PricingEngines::ForwardOptionParameters;
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

        PricingEngines::QuantoOptionParameters<VanillaOptionParameters>* parameters =
            dynamic_cast
            <QuantoOptionParameters<VanillaOptionParameters>*>(
            quantoEngine->parameters());

        parameters->type = QlXlfOper(xltype).AsOptionType();
        parameters->underlying = xlunderlying.AsDouble();
        parameters->strike = xlstrike.AsDouble();
        parameters->dividendYield = xldividendYield .AsDouble();
        parameters->riskFreeRate= xlriskFreeRate.AsDouble();
        parameters->residualTime = xlmaturity.AsDouble();
        parameters->volatility = xlvolatility.AsDouble();
        parameters->foreignRiskFreeRate =
            xlforeignRiskFreeRate.AsDouble();
        parameters->exchangeRateVolatility =
            xlexchangeVolatility.AsDouble();
        parameters->correlation = xlcorrelation.AsDouble();

        parameters->validate();
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

        PricingEngines::ForwardOptionParameters<VanillaOptionParameters>*
            parameters = dynamic_cast
            <ForwardOptionParameters<VanillaOptionParameters>*>(
            forwardEngine->parameters());

        parameters->type = QlXlfOper(xltype).AsOptionType();
        parameters->underlying = xlunderlying.AsDouble();
        // dummy strike
        // ForwardOptionParameter shoul not include strike
        parameters->strike = parameters->underlying;
        parameters->moneyness = xlmoneyness.AsDouble();
        parameters->dividendYield = xldividendYield .AsDouble();
        parameters->riskFreeRate= xlriskFreeRate.AsDouble();
        parameters->resetTime = xlresetTime.AsDouble();
        parameters->residualTime = xlmaturity.AsDouble();
        parameters->volatility = xlvolatility.AsDouble();

        parameters->validate();
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

        PricingEngines::ForwardOptionParameters<VanillaOptionParameters>*
            parameters = dynamic_cast
            <ForwardOptionParameters<VanillaOptionParameters>*>(
            performanceEngine->parameters());

        parameters->type = QlXlfOper(xltype).AsOptionType();
        // dummy underlying
        // ForwardPerformanceOptionParameter should not include underlying
        parameters->underlying = xlunderlying.AsDouble();
        // dummy strike
        // ForwardPerformanceOptionParameter should not include strike
        parameters->strike = parameters->underlying;
        parameters->moneyness = xlmoneyness.AsDouble();
        parameters->dividendYield = xldividendYield .AsDouble();
        parameters->riskFreeRate= xlriskFreeRate.AsDouble();
        parameters->resetTime = xlresetTime.AsDouble();
        parameters->residualTime = xlmaturity.AsDouble();
        parameters->volatility = xlvolatility.AsDouble();

        parameters->validate();
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

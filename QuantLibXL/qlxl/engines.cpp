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
#include <ql/quantlib.hpp>

extern "C"
{

    using namespace QuantLib;
    using QuantLib::PricingEngines::EuropeanAnalyticalEngine;
    using QuantLib::PricingEngines::QuantoVanillaAnalyticEngine;
    using QuantLib::PricingEngines::VanillaOptionParameters;
    using QuantLib::PricingEngines::QuantoOptionParameters;
    using QuantLib::PricingEngines::VanillaOptionResults;
    using QuantLib::PricingEngines::QuantoOptionResults;

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
        parameters->type = type;
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
        double results[7];
        results[0] = vResults->value;
        results[1] = vResults->delta;
        results[2] = vResults->gamma;
        results[3] = vResults->theta;
        results[4] = vResults->vega;
        results[5] = vResults->rho;
        results[6] = vResults->dividendRho;

/*
        const QuantoOptionResults* qResults =
            dynamic_cast<const QuantoOptionResults*>(
                quantoEngine->results());
        results[7] = qResults->qvega;
        results[8] = qResults->qrho;
        results[9] = qResults->qlambda;
*/
        return XlfOper(1,7,results);
        EXCEL_END;
    }

}

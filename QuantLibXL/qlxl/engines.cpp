/*
 Copyright (C) 2002, 2003 Ferdinando Ametrano

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
    using namespace QuantLib::PricingEngines;

    LPXLOPER EXCEL_EXPORT xlEuropeanOption(
                        XlfOper xltype,
                        XlfOper xlunderlying,
                        XlfOper xlstrike,
                        XlfOper xldividendYield,
                        XlfOper xlriskFree,
                        XlfOper xlvalueDate,
                        XlfOper xlmaturityDate,
                        XlfOper xlvolatility)
    {
        EXCEL_BEGIN;

        Handle<AnalyticalVanillaEngine> engine(new
            AnalyticalVanillaEngine);

        VanillaOptionArguments* arguments =
            dynamic_cast<VanillaOptionArguments*>(
                engine->arguments());

        Date valueDate = QlXlfOper(xlvalueDate).AsDate();

        arguments->type = QlXlfOper(xltype).AsOptionType();
        arguments->underlying = xlunderlying.AsDouble();
        arguments->strike = xlstrike.AsDouble();
        arguments->dividendTS = QlXlfOper(xldividendYield) .AsTermStructure(valueDate);
        arguments->riskFreeTS = QlXlfOper(xlriskFree).AsTermStructure(valueDate);
        arguments->exercise = EuropeanExercise(QlXlfOper(xlmaturityDate).AsDate());
        arguments->volTS = QlXlfOper(xlvolatility).AsBlackVolTermStructure(valueDate);

        arguments->validate();
        engine->calculate();

        const VanillaOptionResults* vResults =
            dynamic_cast<const VanillaOptionResults*>(
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



    LPXLOPER EXCEL_EXPORT xlQuantoEuropeanOption(
                        XlfOper xltype,
                        XlfOper xlunderlying,
                        XlfOper xlstrike,
                        XlfOper xldividendYield,
                        XlfOper xlriskFree,
                        XlfOper xlvalueDate,
                        XlfOper xlmaturityDate,
                        XlfOper xlvolatility,
                        XlfOper xlforeignRiskFreeRate,
                        XlfOper xlexchangeVolatility,
                        XlfOper xlcorrelation)
    {
        EXCEL_BEGIN;

        Handle<AnalyticalVanillaEngine> baseEngine(new
            AnalyticalVanillaEngine);

        Handle<QuantoEngine<VanillaOptionArguments,
                            VanillaOptionResults> >
            quantoEngine(new QuantoEngine<VanillaOptionArguments,
                                          VanillaOptionResults>(baseEngine));

        PricingEngines::QuantoOptionArguments<VanillaOptionArguments>* arguments =
            dynamic_cast
            <QuantoOptionArguments<VanillaOptionArguments>*>(
            quantoEngine->arguments());

        Date valueDate = QlXlfOper(xlvalueDate).AsDate();

        arguments->type = QlXlfOper(xltype).AsOptionType();
        arguments->underlying = xlunderlying.AsDouble();
        arguments->strike = xlstrike.AsDouble();
        arguments->dividendTS = QlXlfOper(xldividendYield).AsTermStructure(valueDate);
        arguments->riskFreeTS = QlXlfOper(xlriskFree).AsTermStructure(valueDate);
        arguments->exercise = EuropeanExercise(QlXlfOper(xlmaturityDate).AsDate());
        arguments->volTS = QlXlfOper(xlvolatility).AsBlackVolTermStructure(valueDate);
        arguments->foreignRiskFreeTS =
            QlXlfOper(xlforeignRiskFreeRate).AsTermStructure(valueDate);
        arguments->exchRateVolTS =
            QlXlfOper(xlexchangeVolatility).AsBlackVolTermStructure(valueDate);
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
                        XlfOper xlriskFree,
                        XlfOper xlvalueDate,
                        XlfOper xlresetDate,
                        XlfOper xlmaturityDate,
                        XlfOper xlvolatility)
    {
        EXCEL_BEGIN;

        Handle<AnalyticalVanillaEngine> baseEngine(new
            AnalyticalVanillaEngine);
        Handle<ForwardEngine<VanillaOptionArguments,
                             VanillaOptionResults> >
            forwardEngine(new ForwardEngine<VanillaOptionArguments,
                                            VanillaOptionResults>(baseEngine));

        PricingEngines::ForwardOptionArguments<VanillaOptionArguments>*
            arguments = dynamic_cast
            <ForwardOptionArguments<VanillaOptionArguments>*>(
            forwardEngine->arguments());

        Date valueDate = QlXlfOper(xlvalueDate).AsDate();

        arguments->type = QlXlfOper(xltype).AsOptionType();
        arguments->underlying = xlunderlying.AsDouble();
        // dummy strike
        // ForwardOptionParameter should not include strike
        arguments->strike = arguments->underlying;
        arguments->moneyness = xlmoneyness.AsDouble();
        arguments->dividendTS = QlXlfOper(xldividendYield) .AsTermStructure(valueDate);
        arguments->riskFreeTS = QlXlfOper(xlriskFree).AsTermStructure(valueDate);
        arguments->resetDate = QlXlfOper(xlresetDate).AsDate();
        arguments->exercise = EuropeanExercise(QlXlfOper(xlmaturityDate).AsDate());
        arguments->volTS = QlXlfOper(xlvolatility).AsBlackVolTermStructure(valueDate);

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
                        XlfOper xlriskFree,
                        XlfOper xlvalueDate,
                        XlfOper xlresetDate,
                        XlfOper xlmaturityDate,
                        XlfOper xlvolatility)
    {
        EXCEL_BEGIN;

        Handle<AnalyticalVanillaEngine> baseEngine(new
            AnalyticalVanillaEngine);

        Handle<ForwardPerformanceEngine<VanillaOptionArguments,
                                        VanillaOptionResults> >
            forwardPerformanceEngine(
                new ForwardPerformanceEngine<VanillaOptionArguments,
                                             VanillaOptionResults>(
                    baseEngine));

        PricingEngines::ForwardOptionArguments<VanillaOptionArguments>*
            arguments = dynamic_cast
            <ForwardOptionArguments<VanillaOptionArguments>*>(
            forwardPerformanceEngine->arguments());

        Date valueDate = QlXlfOper(xlvalueDate).AsDate();

        arguments->type = QlXlfOper(xltype).AsOptionType();
        // underlying is needed to interpolate on the vol surface
        arguments->underlying = xlunderlying.AsDouble();
        // dummy strike
        // ForwardPerformanceOptionParameter should not include strike
        arguments->strike = arguments->underlying;
        arguments->moneyness = xlmoneyness.AsDouble();
        arguments->dividendTS = QlXlfOper(xldividendYield) .AsTermStructure(valueDate);
        arguments->riskFreeTS = QlXlfOper(xlriskFree).AsTermStructure(valueDate);
        arguments->resetDate = QlXlfOper(xlresetDate).AsDate();
        arguments->exercise = EuropeanExercise(QlXlfOper(xlmaturityDate).AsDate());
        arguments->volTS = QlXlfOper(xlvolatility).AsBlackVolTermStructure(valueDate);

        arguments->validate();
        forwardPerformanceEngine->calculate();

        const VanillaOptionResults* vResults =
            dynamic_cast<const VanillaOptionResults*>(
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

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
*/

#include <qlxl/qlxlfoper.hpp>
#include <ql/stochasticprocess.hpp>
#include <ql/PricingEngines/Vanilla/all.hpp>
#include <ql/PricingEngines/Quanto/all.hpp>
#include <ql/PricingEngines/Forward/all.hpp>
#include <ql/PricingEngines/Cliquet/all.hpp>
#include <ql/Instruments/quantovanillaoption.hpp>
#include <ql/Instruments/forwardvanillaoption.hpp>
#include <boost/shared_ptr.hpp>

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

    	WIZARD_NO_CALC;

        Date refDate = QlXlfOper(xlrefDate).AsDate();

        boost::shared_ptr<SimpleQuote> spot(new
            SimpleQuote(xlunderlying.AsDouble()));

        boost::shared_ptr<BlackScholesProcess> stochProcess(new
            BlackScholesProcess(
                Handle<Quote>(spot),
                QlXlfOper(xldividendYield).AsTermStructure(refDate),
                QlXlfOper(xlriskFree).AsTermStructure(refDate),
                QlXlfOper(xlvolatility).AsBlackVolTermStructure(refDate,
                                                xlinterpolationType.AsInt())));

        boost::shared_ptr<StrikedTypePayoff> payoff(new
            PlainVanillaPayoff(QlXlfOper(xltype).AsOptionType(),
                               xlstrike.AsDouble()));

        boost::shared_ptr<Exercise> exercise(new
            EuropeanExercise(QlXlfOper(xlmaturityDate).AsDate()));

        boost::shared_ptr<PricingEngine> engine(new AnalyticEuropeanEngine);

        VanillaOption option(stochProcess, payoff, exercise, engine);

        double results[8];
        results[0] = option.NPV();
        results[1] = option.delta();
        results[2] = option.gamma();
        results[3] = option.theta();
        results[4] = option.vega();
        results[5] = option.rho();
        results[6] = option.dividendRho();
        results[7] = option.strikeSensitivity();

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

    	WIZARD_NO_CALC;

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

        boost::shared_ptr<PricingEngine> engine(new
            MCEuropeanEngine<PseudoRandom>(maxTimeStepPerYear,
                                           antitheticVariate, controlVariate,
                                           nSamples, tolerance,
                                           maxSamples, mcSeed));

        // hard coded for the time being
        int interpolationType = 2;

        Date refDate = QlXlfOper(xlrefDate).AsDate();

        boost::shared_ptr<SimpleQuote> spot(new
            SimpleQuote(xlunderlying.AsDouble()));

        boost::shared_ptr<BlackScholesProcess> stochProcess(new
            BlackScholesProcess(
                Handle<Quote>(spot),
                QlXlfOper(xldividendYield).AsTermStructure(refDate),
                QlXlfOper(xlriskFree).AsTermStructure(refDate),
                QlXlfOper(xlvolatility).AsBlackVolTermStructure(refDate,
                                                interpolationType)));

        boost::shared_ptr<StrikedTypePayoff> payoff(new
            PlainVanillaPayoff(QlXlfOper(xltype).AsOptionType(),
                               xlstrike.AsDouble()));

        boost::shared_ptr<Exercise> exercise(new
            EuropeanExercise(QlXlfOper(xlmaturityDate).AsDate()));

        VanillaOption option(stochProcess, payoff, exercise, engine);

        double results[2];
        results[0] = option.NPV();
        results[1] = option.errorEstimate();

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

    	WIZARD_NO_CALC;

        boost::shared_ptr<VanillaOption::engine> baseEngine(
            new AnalyticEuropeanEngine);

        boost::shared_ptr<QuantoEngine<VanillaOption::arguments,
                                       VanillaOption::results> > quantoEngine(
            new QuantoEngine<VanillaOption::arguments,
                             VanillaOption::results>(baseEngine));

        Date refDate = QlXlfOper(xlrefDate).AsDate();

        boost::shared_ptr<SimpleQuote> spot(new
            SimpleQuote(xlunderlying.AsDouble()));

        boost::shared_ptr<BlackScholesProcess> stochProcess(new
            BlackScholesProcess(
                Handle<Quote>(spot),
                QlXlfOper(xldividendYield).AsTermStructure(refDate),
                QlXlfOper(xlriskFree).AsTermStructure(refDate),
                QlXlfOper(xlvolatility).AsBlackVolTermStructure(refDate,
                                                xlinterpolationType.AsInt())));

        boost::shared_ptr<StrikedTypePayoff> payoff(new
            PlainVanillaPayoff(QlXlfOper(xltype).AsOptionType(),
                               xlstrike.AsDouble()));

        boost::shared_ptr<Exercise> exercise(new
            EuropeanExercise(QlXlfOper(xlmaturityDate).AsDate()));

        QuantoVanillaOption quantoOption(
            QlXlfOper(xlforeignRiskFreeRate).AsTermStructure(refDate),
            QlXlfOper(xlexchangeVolatility).AsBlackVolTermStructure(
                refDate, xlinterpolationType.AsInt()),
                Handle<Quote>(boost::shared_ptr<Quote>(new
                    SimpleQuote(xlcorrelation.AsDouble()))),
            stochProcess,
            payoff,
            exercise,
            quantoEngine);

        double results[10];
        results[0] = quantoOption.NPV();
        results[1] = quantoOption.delta();
        results[2] = quantoOption.gamma();
        results[3] = quantoOption.theta();
        results[4] = quantoOption.vega();
        results[5] = quantoOption.rho();
        results[6] = quantoOption.dividendRho();
        results[7] = quantoOption.qvega();
        results[8] = quantoOption.qrho();
        results[9] = quantoOption.qlambda();

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

    	WIZARD_NO_CALC;

        boost::shared_ptr<VanillaOption::engine> baseEngine(
            new AnalyticEuropeanEngine);

        boost::shared_ptr<ForwardEngine<VanillaOption::arguments,
            VanillaOption::results> > forwardEngine(new
                ForwardEngine<VanillaOption::arguments,
                              VanillaOption::results>(baseEngine));

        Date refDate = QlXlfOper(xlrefDate).AsDate();

        boost::shared_ptr<SimpleQuote> spot(new
            SimpleQuote(xlunderlying.AsDouble()));

        boost::shared_ptr<BlackScholesProcess> stochProcess(new
            BlackScholesProcess(
                Handle<Quote>(spot),
                QlXlfOper(xldividendYield).AsTermStructure(refDate),
                QlXlfOper(xlriskFree).AsTermStructure(refDate),
                QlXlfOper(xlvolatility).AsBlackVolTermStructure(refDate,
                                                xlinterpolationType.AsInt())));

        // dummy strike
        // ForwardOptionParameter should not include strike
        boost::shared_ptr<StrikedTypePayoff> payoff(new
            PlainVanillaPayoff(QlXlfOper(xltype).AsOptionType(),
                               xlunderlying.AsDouble()));

        boost::shared_ptr<Exercise> exercise(new
            EuropeanExercise(QlXlfOper(xlmaturityDate).AsDate()));

        ForwardVanillaOption option(xlmoneyness.AsDouble(),
                                    QlXlfOper(xlresetDate).AsDate(),
                                    stochProcess,
                                    payoff,
                                    exercise,
                                    forwardEngine);

        double results[7];
        results[0] = option.NPV();
        results[1] = option.delta();
        results[2] = option.gamma();
        results[3] = option.theta();
        results[4] = option.vega();
        results[5] = option.rho();
        results[6] = option.dividendRho();

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

    	WIZARD_NO_CALC;

        boost::shared_ptr<VanillaOption::engine> baseEngine(
            new AnalyticEuropeanEngine);

        boost::shared_ptr<ForwardPerformanceEngine<VanillaOption::arguments,
            VanillaOption::results> > engine(new
               ForwardPerformanceEngine<VanillaOption::arguments,
                                        VanillaOption::results>(baseEngine));

        Date refDate = QlXlfOper(xlrefDate).AsDate();

        // underlying is needed to interpolate on the vol surface
        boost::shared_ptr<SimpleQuote> spot(new
            SimpleQuote(xlunderlying.AsDouble()));

        boost::shared_ptr<BlackScholesProcess> stochProcess(new
            BlackScholesProcess(
                Handle<Quote>(spot),
                QlXlfOper(xldividendYield).AsTermStructure(refDate),
                QlXlfOper(xlriskFree).AsTermStructure(refDate),
                QlXlfOper(xlvolatility).AsBlackVolTermStructure(refDate,
                                                xlinterpolationType.AsInt())));

        // dummy strike
        // ForwardOptionParameter should not include strike
        boost::shared_ptr<StrikedTypePayoff> payoff(new
            PlainVanillaPayoff(QlXlfOper(xltype).AsOptionType(),
                               xlunderlying.AsDouble()));

        boost::shared_ptr<Exercise> exercise(new
            EuropeanExercise(QlXlfOper(xlmaturityDate).AsDate()));

        ForwardVanillaOption option(xlmoneyness.AsDouble(),
                                    QlXlfOper(xlresetDate).AsDate(),
                                    stochProcess,
                                    payoff,
                                    exercise,
                                    engine);

        double results[7];
        results[0] = option.NPV();
        results[1] = option.delta();
        results[2] = option.gamma();
        results[3] = option.theta();
        results[4] = option.vega();
        results[5] = option.rho();
        results[6] = option.dividendRho();

        return XlfOper(1,7,results);
        EXCEL_END;
    }



    LPXLOPER EXCEL_EXPORT xlCliquetOption(
                        XlfOper xltype,
                        XlfOper xlunderlying,
                        XlfOper xlmoneyness,
                        XlfOper xldividend,
                        XlfOper xlriskFree,
                        XlfOper xlrefDate,
                        XlfOper xlresetDates,
                        XlfOper xlvolatility,
                        XlfOper xlinterpolationType)
    {
        EXCEL_BEGIN;

        WIZARD_NO_CALC;

        Date refDate = QlXlfOper(xlrefDate).AsDate();

        boost::shared_ptr<SimpleQuote> spot(new
            SimpleQuote(xlunderlying.AsDouble()));

        boost::shared_ptr<BlackScholesProcess> stochProcess(new
            BlackScholesProcess(
                Handle<Quote>(spot),
                QlXlfOper(xldividend).AsTermStructure(refDate),
                QlXlfOper(xlriskFree).AsTermStructure(refDate),
                QlXlfOper(xlvolatility).AsBlackVolTermStructure(refDate,
                                                xlinterpolationType.AsInt())));

        boost::shared_ptr<PercentageStrikePayoff> payoff(new
            PercentageStrikePayoff(QlXlfOper(xltype).AsOptionType(),
                                   xlmoneyness.AsDouble()));

        std::vector<Date> tempDates = QlXlfOper(xlresetDates).AsDateVector();
        std::vector<Date> resetDates(tempDates.size()-1);
        std::copy(tempDates.begin(), tempDates.end()-1, resetDates.begin());

        boost::shared_ptr<EuropeanExercise> exercise(new
            EuropeanExercise(tempDates.back()));

        boost::shared_ptr<PricingEngine> engine(new AnalyticCliquetEngine);

        CliquetOption cliquet(stochProcess, payoff, exercise, resetDates, engine);

        double results[7];
        results[0] = cliquet.NPV();
        results[1] = cliquet.delta();
        results[2] = cliquet.gamma();
        results[3] = cliquet.theta();
        results[4] = cliquet.vega();
        results[5] = cliquet.rho();
        results[6] = cliquet.dividendRho();

        return XlfOper(1,7,results);
        EXCEL_END;
    }

    LPXLOPER EXCEL_EXPORT xlPerformanceOption(
                        XlfOper xltype,
                        XlfOper xlunderlying,
                        XlfOper xlmoneyness,
                        XlfOper xldividend,
                        XlfOper xlriskFree,
                        XlfOper xlrefDate,
                        XlfOper xlresetDates,
                        XlfOper xlvolatility,
                        XlfOper xlinterpolationType)

    {
        EXCEL_BEGIN;

        WIZARD_NO_CALC;

        Date refDate = QlXlfOper(xlrefDate).AsDate();

        boost::shared_ptr<SimpleQuote> spot(new
            SimpleQuote(xlunderlying.AsDouble()));

        boost::shared_ptr<BlackScholesProcess> stochProcess(new
            BlackScholesProcess(
                Handle<Quote>(spot),
                QlXlfOper(xldividend).AsTermStructure(refDate),
                QlXlfOper(xlriskFree).AsTermStructure(refDate),
                QlXlfOper(xlvolatility).AsBlackVolTermStructure(refDate,
                                                xlinterpolationType.AsInt())));

        boost::shared_ptr<PercentageStrikePayoff> payoff(new
            PercentageStrikePayoff(QlXlfOper(xltype).AsOptionType(),
                                   xlmoneyness.AsDouble()));

        std::vector<Date> tempDates = QlXlfOper(xlresetDates).AsDateVector();
        std::vector<Date> resetDates(tempDates.size()-1);
        std::copy(tempDates.begin(), tempDates.end()-1, resetDates.begin());

        boost::shared_ptr<EuropeanExercise> exercise(new
            EuropeanExercise(tempDates.back()));

        boost::shared_ptr<PricingEngine> engine(new AnalyticPerformanceEngine);

        CliquetOption perfCliquet(stochProcess, payoff, exercise, resetDates, engine);

        double results[7];
        results[0] = perfCliquet.NPV();
        results[1] = perfCliquet.delta();
        results[2] = perfCliquet.gamma();
        results[3] = perfCliquet.theta();
        results[4] = perfCliquet.vega();
        results[5] = perfCliquet.rho();
        results[6] = perfCliquet.dividendRho();

        return XlfOper(1,7,results);
        EXCEL_END;
    }


}

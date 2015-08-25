/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2015 Peter Caspers

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it
 under the terms of the QuantLib license.  You should have received a
 copy of the license along with this program; if not, please email
 <quantlib-dev@lists.sf.net>. The license is also available online at
 <http://quantlib.org/license.shtml>.

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
*/

#include "lgm.hpp"
#include "utilities.hpp"
#include <ql/math/optimization/levenbergmarquardt.hpp>
#include <ql/time/calendars/target.hpp>
#include <ql/time/daycounters/actual360.hpp>
#include <ql/time/daycounters/thirty360.hpp>
#include <ql/quotes/simplequote.hpp>
#include <ql/indexes/ibor/euribor.hpp>
#include <ql/termstructures/yield/flatforward.hpp>
#include <ql/models/shortrate/onefactormodels/gsr.hpp>
#include <ql/models/shortrate/calibrationhelpers/swaptionhelper.hpp>
#include <ql/pricingengines/swaption/gaussian1dswaptionengine.hpp>
#include <ql/experimental/models/lgm1.hpp>
#include <ql/experimental/models/cclgm1.hpp>
#include <ql/math/statistics/incrementalstatistics.hpp>
#include <ql/math/randomnumbers/rngtraits.hpp>
#include <ql/methods/montecarlo/multipathgenerator.hpp>
#include <ql/methods/montecarlo/pathgenerator.hpp>

#include <boost/make_shared.hpp>

#include <iostream> // just for debug

using namespace QuantLib;
using boost::unit_test_framework::test_suite;

void LgmTest::testBermudanLgm1fGsr() {

    BOOST_TEST_MESSAGE("Testing consistency of Bermudan swaption pricing in "
                       "LGM1F and GSR models...");

    // for kappa (LGM) = reversion (GSR) = 0.0
    // we have alpha (LGM) = sigma (GSR), so
    // we should get equal Bermudan swaption prices

    SavedSettings backup;

    Date evalDate(12, January, 2015);
    Settings::instance().evaluationDate() = evalDate;
    Handle<YieldTermStructure> yts(
        boost::make_shared<FlatForward>(evalDate, 0.02, Actual365Fixed()));
    boost::shared_ptr<IborIndex> euribor6m =
        boost::make_shared<Euribor>(6 * Months, yts);

    Date effectiveDate = TARGET().advance(evalDate, 2 * Days);
    Date startDate = TARGET().advance(effectiveDate, 1 * Years);
    Date maturityDate = TARGET().advance(startDate, 9 * Years);

    Schedule fixedSchedule(startDate, maturityDate, 1 * Years, TARGET(),
                           ModifiedFollowing, ModifiedFollowing,
                           DateGeneration::Forward, false);
    Schedule floatingSchedule(startDate, maturityDate, 6 * Months, TARGET(),
                              ModifiedFollowing, ModifiedFollowing,
                              DateGeneration::Forward, false);
    boost::shared_ptr<VanillaSwap> underlying = boost::make_shared<VanillaSwap>(
        VanillaSwap(VanillaSwap::Payer, 1.0, fixedSchedule, 0.02, Thirty360(),
                    floatingSchedule, euribor6m, 0.0, Actual360()));

    std::vector<Date> exerciseDates;
    for (Size i = 0; i < 9; ++i) {
        exerciseDates.push_back(TARGET().advance(fixedSchedule[i], -2 * Days));
    }
    boost::shared_ptr<Exercise> exercise =
        boost::make_shared<BermudanExercise>(exerciseDates, false);

    boost::shared_ptr<Swaption> swaption =
        boost::make_shared<Swaption>(underlying, exercise);

    std::vector<Date> stepDates(exerciseDates.begin(), exerciseDates.end() - 1);
    std::vector<Real> sigmas(stepDates.size() + 1);
    for (Size i = 0; i < sigmas.size(); ++i) {
        sigmas[i] = 0.0050 +
                    (0.0080 - 0.0050) * std::exp(-0.2 * static_cast<double>(i));
    }

    Real reversion = 0.0;

    // fix any T forward measure
    boost::shared_ptr<Gsr> gsr =
        boost::make_shared<Gsr>(yts, stepDates, sigmas, reversion, 50.0);

    boost::shared_ptr<Lgm1> lgm =
        boost::make_shared<Lgm1>(yts, stepDates, sigmas, reversion);

    boost::shared_ptr<PricingEngine> swaptionEngineGsr =
        boost::make_shared<Gaussian1dSwaptionEngine>(gsr, 64, 7.0, true, false);

    boost::shared_ptr<PricingEngine> swaptionEngineLgm =
        boost::make_shared<Gaussian1dSwaptionEngine>(lgm, 64, 7.0, true, false);

    swaption->setPricingEngine(swaptionEngineGsr);
    Real npvGsr = swaption->NPV();
    swaption->setPricingEngine(swaptionEngineLgm);
    Real npvLgm = swaption->NPV();

    Real tol = 0.05E-4; // basis point tolerance

    if (std::fabs(npvGsr - npvLgm) > tol)
        BOOST_ERROR(
            "Failed to verify consistency of Bermudan swaption price in Lgm1f ("
            << npvLgm << ") and Gsr (" << npvGsr << ") models, tolerance is "
            << tol);
} // testBermudanLgm1fGsr

void LgmTest::testLgm1fCalibration() {

    BOOST_TEST_MESSAGE(
        "Testing calibration of LGM1F model against GSR parameters...");

    // for fixed kappa != 0.0 we calibrate alpha
    // and compare the effective Hull White parameters
    // with the calibration results for the Gsr model

    SavedSettings backup;

    Date evalDate(12, January, 2015);
    Settings::instance().evaluationDate() = evalDate;
    Handle<YieldTermStructure> yts(
        boost::make_shared<FlatForward>(evalDate, 0.02, Actual365Fixed()));
    boost::shared_ptr<IborIndex> euribor6m =
        boost::make_shared<Euribor>(6 * Months, yts);

    // coterminal basket 1y-9y, 2y-8y, ... 9y-1y

    std::vector<boost::shared_ptr<CalibrationHelper> > basket;
    Real impliedVols[] = {0.4, 0.39, 0.38, 0.35, 0.35, 0.34, 0.33, 0.32, 0.31};
    std::vector<Date> expiryDates;

    for (Size i = 0; i < 9; ++i) {
        boost::shared_ptr<CalibrationHelper> helper =
            boost::make_shared<SwaptionHelper>(
                (i + 1) * Years, (9 - i) * Years,
                Handle<Quote>(boost::make_shared<SimpleQuote>(impliedVols[i])),
                euribor6m, 1 * Years, Thirty360(), Actual360(), yts);
        basket.push_back(helper);
        expiryDates.push_back(boost::static_pointer_cast<SwaptionHelper>(helper)
                                  ->swaption()
                                  ->exercise()
                                  ->dates()
                                  .back());
    }

    std::vector<Date> stepDates(expiryDates.begin(), expiryDates.end() - 1);

    std::vector<Real> gsrInitialSigmas(stepDates.size() + 1, 0.0050);
    std::vector<Real> lgmInitialAlphas(stepDates.size() + 1, 0.0050);

    Real kappa = 0.05;

    // fix any T forward measure
    boost::shared_ptr<Gsr> gsr =
        boost::make_shared<Gsr>(yts, stepDates, gsrInitialSigmas, kappa, 50.0);

    boost::shared_ptr<Lgm1> lgm =
        boost::make_shared<Lgm1>(yts, stepDates, lgmInitialAlphas, kappa);

    boost::shared_ptr<PricingEngine> swaptionEngineGsr =
        boost::make_shared<Gaussian1dSwaptionEngine>(gsr, 64, 7.0, true, false);

    boost::shared_ptr<PricingEngine> swaptionEngineLgm =
        boost::make_shared<Gaussian1dSwaptionEngine>(lgm, 64, 7.0, true, false);

    // calibrate GSR

    LevenbergMarquardt lm(1E-8, 1E-8, 1E-8);
    EndCriteria ec(1000, 500, 1E-8, 1E-8, 1E-8);

    for (Size i = 0; i < basket.size(); ++i) {
        basket[i]->setPricingEngine(swaptionEngineGsr);
    }

    gsr->calibrateVolatilitiesIterative(basket, lm, ec);

    Array gsrSigmas = gsr->volatility();

    // calibrate LGM

    for (Size i = 0; i < basket.size(); ++i) {
        basket[i]->setPricingEngine(swaptionEngineLgm);
    }

    lgm->calibrateAlphasIterative(basket, lm, ec);

    std::vector<Real> lgmHwSigmas;
    std::vector<Real> lgmHwKappas;

    for (Size i = 0; i < gsrSigmas.size(); ++i) {
        lgmHwSigmas.push_back(
            lgm->hullWhiteSigma(static_cast<double>(i) + 0.5));
        lgmHwKappas.push_back(
            lgm->hullWhiteKappa(static_cast<double>(i) + 0.5));
    }

    Real tol0 = 1E-8;
    Real tol = 1E-4;

    for (Size i = 0; i < gsrSigmas.size(); ++i) {
        // check calibration itself, we should match the market prices
        // rather exactly
        if (std::fabs(basket[i]->modelValue() - basket[i]->marketValue()) >
            tol0)
            BOOST_ERROR("Failed to calibrate to market swaption #"
                        << i << ", market price is " << basket[i]->marketValue()
                        << " while model price is " << basket[i]->modelValue());
        // we can not directly compare the gsr model's sigma with
        // the lgm model's equivalent HW sigma (since the former
        // is piecewise constant, while the latter is not), but
        // we can do a rough check on the mid point of each interval
        if (std::fabs(gsrSigmas[i] - lgmHwSigmas[i]) > tol)
            BOOST_ERROR("Failed to verify LGM's equivalent Hull White sigma (#"
                        << i << "), which is " << lgmHwSigmas[i]
                        << " while GSR's sigma is " << gsrSigmas[i] << ")");
    }

} // testLgm1fCalibration

void LgmTest::testLgm3fForeignPayouts() {
    BOOST_TEST_MESSAGE("Testing pricing of foreign payouts under domestic "
                       "measure in LGM3F model...");

    SavedSettings backup;

    Date referenceDate(30, July, 2015);

    Settings::instance().evaluationDate() = referenceDate;

    Handle<YieldTermStructure> eurYts(
        boost::make_shared<FlatForward>(referenceDate, 0.02, Actual365Fixed()));

    Handle<YieldTermStructure> usdYts(
        boost::make_shared<FlatForward>(referenceDate, 0.05, Actual365Fixed()));

    // use different grids for the EUR and USD  models and the FX volatility
    // process to test the piecewise numerical integration ...

    std::vector<Date> volstepdatesEur, volstepdatesUsd, volstepdatesFx;

    volstepdatesEur.push_back(Date(15, July, 2016));
    volstepdatesEur.push_back(Date(15, July, 2017));
    volstepdatesEur.push_back(Date(15, July, 2018));
    volstepdatesEur.push_back(Date(15, July, 2019));
    volstepdatesEur.push_back(Date(15, July, 2020));

    volstepdatesUsd.push_back(Date(13, April, 2016));
    volstepdatesUsd.push_back(Date(13, September, 2016));
    volstepdatesUsd.push_back(Date(13, April, 2017));
    volstepdatesUsd.push_back(Date(13, September, 2017));
    volstepdatesUsd.push_back(Date(13, April, 2018));
    volstepdatesUsd.push_back(Date(15, July, 2018)); // shared with EUR
    volstepdatesUsd.push_back(Date(13, April, 2019));
    volstepdatesUsd.push_back(Date(13, September, 2019));

    volstepdatesFx.push_back(Date(15, July, 2016)); // shared with EUR
    volstepdatesFx.push_back(Date(15, October, 2016));
    volstepdatesFx.push_back(Date(15, May, 2017));
    volstepdatesFx.push_back(Date(13, September, 2017)); // shared with USD
    volstepdatesFx.push_back(Date(15, July, 2018)); //  shared with EUR and USD

    std::vector<Real> eurVols, usdVols, fxSigmas;

    for (Size i = 0; i < volstepdatesEur.size() + 1; ++i) {
        eurVols.push_back(0.0050 +
                          (0.0080 - 0.0050) *
                              std::exp(-0.3 * static_cast<double>(i)));
    }
    for (Size i = 0; i < volstepdatesUsd.size() + 1; ++i) {
        usdVols.push_back(0.0030 +
                          (0.0110 - 0.0030) *
                              std::exp(-0.3 * static_cast<double>(i)));
    }
    for (Size i = 0; i < volstepdatesFx.size() + 1; ++i) {
        fxSigmas.push_back(
            0.15 + (0.20 - 0.15) * std::exp(-0.3 * static_cast<double>(i)));
    }

    boost::shared_ptr<Lgm1::model_type> eurLgm =
        boost::make_shared<Lgm1>(eurYts, volstepdatesEur, eurVols, 0.02);
    boost::shared_ptr<Lgm1::model_type> usdLgm =
        boost::make_shared<Lgm1>(usdYts, volstepdatesUsd, usdVols, 0.04);

    std::vector<boost::shared_ptr<Lgm1::model_type> > singleModels;
    singleModels.push_back(eurLgm);
    singleModels.push_back(usdLgm);

    std::vector<Handle<YieldTermStructure> > curves;
    curves.push_back(eurYts);
    curves.push_back(usdYts);

    std::vector<Handle<Quote> > fxSpots;
    fxSpots.push_back(Handle<Quote>(boost::make_shared<SimpleQuote>(
        std::log(0.90)))); // USD per EUR in log scale

    std::vector<std::vector<Real> > fxVolatilities;
    fxVolatilities.push_back(fxSigmas);

    Matrix c(3, 3);
    // commented out version of the correlation matrix
    // ... this will not be reformmated automatically ...
    // //  FX             EUR         USD
    // c[0][0] = 1.0; c[0][1] = 0.8; c[0][2] = -0.5; // FX
    // c[1][0] = 0.8; c[1][1] = 1.0; c[1][2] = -0.2; // EUR
    // c[2][0] = -0.5; c[2][1] = -0.2; c[2][2] = 1.0; // USD
    // .. like maybe this code ...
    //  FX             EUR         USD
    c[0][0] = 1.0;
    c[0][1] = 0.8;
    c[0][2] = -0.5; // FX
    c[1][0] = 0.8;
    c[1][1] = 1.0;
    c[1][2] = -0.2; // EUR
    c[2][0] = -0.5;
    c[2][1] = -0.2;
    c[2][2] = 1.0; // USD

    boost::shared_ptr<CcLgm1> ccLgm = boost::make_shared<CcLgm1>(
        singleModels, fxSpots, volstepdatesFx, fxVolatilities, c, curves);

    boost::shared_ptr<StochasticProcess> process = ccLgm->stateProcess();

    boost::shared_ptr<StochasticProcess> usdProcess = usdLgm->stateProcess();

    // path generation

    Size n = atoi(getenv("N"));       // number of paths
    Size seed = atoi(getenv("SEED")); // seed
    // maturity of test payoffs
    Time T = 5.0;
    // take large steps, but not only one (since we are testing)
    Size steps = static_cast<Size>(T * 2.0);
    TimeGrid grid(T, steps);
    PseudoRandom::rsg_type sg =
        PseudoRandom::make_sequence_generator(3 * steps, seed);
    PseudoRandom::rsg_type sg2 =
        PseudoRandom::make_sequence_generator(steps, seed);

    MultiPathGenerator<PseudoRandom::rsg_type> pg(process, grid, sg, false);
    PathGenerator<PseudoRandom::rsg_type> pg2(usdProcess, grid, sg2, false);

    std::vector<Sample<MultiPath> > paths;
    for (Size j = 0; j < n; ++j) {
        paths.push_back(pg.next());
    }

    std::vector<Sample<Path> > paths2;
    for (Size j = 0; j < n; ++j) {
        paths2.push_back(pg2.next());
    }

    // test
    // 1 deterministic USD cashflow under EUR numeraire vs. price on USD curve
    // 2 zero bond option USD under EUR numeraire vs. USD numeraire
    // 3 fx option EUR-USD under EUR numeraire vs. analytical price

    IncrementalStatistics stat1, stat2a, stat2b, stat3;
    // same for paths2 since shared time grid
    Size l = paths[0].value[0].length() - 1;
    for (Size j = 0; j < n; ++j) {
        Real fx = std::exp(paths[j].value[0][l]);
        Real zeur = paths[j].value[1][l];
        Real zusd = paths[j].value[2][l];
        Real zusd2 = paths2[j].value[l];
        Real yeur = (zeur - eurLgm->stateProcess()->expectation(0.0, 0.0, T)) /
                    eurLgm->stateProcess()->stdDeviation(0.0, 0.0, T);
        Real yusd = (zusd - usdLgm->stateProcess()->expectation(0.0, 0.0, T)) /
                    usdLgm->stateProcess()->stdDeviation(0.0, 0.0, T);
        Real yusd2 =
            (zusd2 - usdLgm->stateProcess()->expectation(0.0, 0.0, T)) /
            usdLgm->stateProcess()->stdDeviation(0.0, 0.0, T);

        // 1 USD paid at T deflated with EUR numeraire
        stat1.add(1.0 * fx / eurLgm->numeraire(T, yeur));

        // 2 USD zero bond option at T on P(T,T+10) strike 0.5 ...
        // ... under EUR numeraire ...
        Real zbOpt = std::max(usdLgm->zerobond(T + 10.0, T, yusd) - 0.5, 0.0);
        stat2a.add(zbOpt * fx / eurLgm->numeraire(T, yeur));
        // ... and under USD numeraire ...
        Real zbOpt2 = std::max(usdLgm->zerobond(T + 10.0, T, yusd2) - 0.5, 0.0);
        stat2b.add(zbOpt2 / usdLgm->numeraire(T, yusd2));
    }

    std::clog << "det cf pricing " << stat1.mean() << " EUR +- "
              << stat1.errorEstimate() << " curve price "
              << usdYts->discount(5.0) * std::exp(fxSpots[0]->value())
              << std::endl;

    std::clog << "zb option pricing " << stat2a.mean() << " EUR +- "
              << stat2a.errorEstimate() << " domestic numeraire "
              << stat2b.mean() * std::exp(fxSpots[0]->value()) << " EUR +- "
              << stat2b.errorEstimate() * std::exp(fxSpots[0]->value()) << std::endl;

} // testLgm3fForeignPayouts

void LgmTest::testLgm3fFxCalibration() {
    BOOST_TEST_MESSAGE("Testing fx calibration of LGM3F model...");
} // testLgm3fFxCalibration

void LgmTest::testLgm4f() {
    BOOST_TEST_MESSAGE("Testing LGM4F model...");
} // testLgm4fModel

test_suite *LgmTest::suite() {
    test_suite *suite = BOOST_TEST_SUITE("LGM model tests");
    suite->add(QUANTLIB_TEST_CASE(&LgmTest::testBermudanLgm1fGsr));
    suite->add(QUANTLIB_TEST_CASE(&LgmTest::testLgm1fCalibration));
    suite->add(QUANTLIB_TEST_CASE(&LgmTest::testLgm3fForeignPayouts));
    suite->add(QUANTLIB_TEST_CASE(&LgmTest::testLgm3fFxCalibration));
    suite->add(QUANTLIB_TEST_CASE(&LgmTest::testLgm4f));
    return suite;
}

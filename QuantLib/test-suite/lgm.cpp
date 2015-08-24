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
#include <ql/time/calendars/target.hpp>
#include <ql/time/daycounters/actual360.hpp>
#include <ql/time/daycounters/thirty360.hpp>
#include <ql/indexes/ibor/euribor.hpp>
#include <ql/termstructures/yield/flatforward.hpp>
#include <ql/models/shortrate/onefactormodels/gsr.hpp>
#include <ql/pricingengines/swaption/gaussian1dswaptionengine.hpp>
#include <ql/experimental/models/lgm1.hpp>

#include <boost/make_shared.hpp>

#include <iostream> // just for debug

using namespace QuantLib;
using boost::unit_test_framework::test_suite;

void LgmTest::testBermudanLgm1fGsr() {

    BOOST_TEST_MESSAGE("Testing consistency of Bermudan swaption pricing in "
                       "Lgm1f and Gsr models...");

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
}

void LgmTest::testLgm1fCalibration() {

    BOOST_TEST_MESSAGE("Testing calibration of Lgm1f model...");

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
    double[] impliedVols = {0.4, 0.39, 0.38, 0.35, 0.31, 0.27, 0.22, 0.2, 0.2};
    std::vector<Date> expiryDates;

    for (Size i = 0; i < 9; ++i) {
        boost::shared<CalibrationHelper> helper =
            boost::make_shared<SwaptionHelper>(
                (i + 1) * Years, (9 - i) * Years,
                Handle<Quote>(boost::make_shared<SimpleQuote>(impliedVols[i]),
                              euribor6m, 1 * Years, Thirty360(), Actual360(),
                              yts));
        basket.push_back(helper);
        expiryDates.push_back(helper->swaption()->exercise()->dates().back());
    }

    std::vector<Date> stepDates(expiryDates.begin(), expiryDates.end() - 1);

    std::vector<Real> gsrInitialSigmas(stepDates.size() + 1, 0.0050);
    std::vector<Real> lgmInitialAlphas(stepDates.size() + 1, 0.0050);

    Real kappa = 0.03;

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

    LevenbergMarquardt lm(1E-8,1E-8,1E-8);
    EndCriteria ec(1000,500,1E-8,1E-8,1E-8);

    for (Size i = 0; i < basket.size(); ++i) {
        helper[i]->setPricingEngine(swaptionEngineGsr);
    }

    gsr->calibrateVolatilitiesIterative(helpers, method, ec);

    Array gsrSigmas = gsr->volatility();

    // calibrate LGM

    for(Size i=0;i<basket.size();++i) {
        helper[i]->setPricingEngine(swaptionEngineLgm);
    }

    lgm->calibrateAlphasIterative(helpers, method, ec);

    Array lgmAlphas = lgm->alpha();

    for(Size i=0;i<gsrSigmas.size();++i) {
        std::clog << "#" << i << " gsr " << gsrSigmas[i] << " lgm " << lgmAlphas[i] << std::endl;
    }

    Real t=0.0;
    while(t<=10.0) {
        std::clog << t << " " << lgm->hullWhiteSigma(t) << " " << lgm->hullWhiteKappa << std::endl;
        t+=0.1;
    }
    
}

test_suite *LgmTest::suite() {
    test_suite *suite = BOOST_TEST_SUITE("LGM model tests");
    suite->add(QUANTLIB_TEST_CASE(&LgmTest::testBermudanLgm1fGsr));
    suite->add(QUANTLIB_TEST_CASE(&LgmTest::testLgm1fCalibration));

    return suite;
}

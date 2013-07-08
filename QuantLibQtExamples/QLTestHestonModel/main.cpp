/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*!
 Copyright (C) 2005, 2006, 2007, 2009 StatPro Italia srl

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

#include "customutilities.hpp"
#include <ql/quantlib.hpp>
#include <boost/timer.hpp>
#include <boost/shared_ptr.hpp>
#include <iostream>
#include <iomanip>

using namespace QuantLib;

#if defined(QL_ENABLE_SESSIONS)
namespace QuantLib {

    Integer sessionId() { return 0; }

}
#endif

/*
 * Testing Heston Model as is done in unit-test
 */
class HestonModelTest {
  public:
    static void testBlackCalibration();
    static void testDAXCalibration();
    static void testAnalyticVsBlack();
    static void testAnalyticVsCached();
    static void testKahlJaeckelCase();
    static void testMcVsCached();
    static void testFdBarrierVsCached();
    static void testFdVanillaVsCached();
    static void testDifferentIntegrals();
    static void testMultipleStrikesEngine();
    static void testAnalyticPiecewiseTimeDependent();
    static void testDAXCalibrationOfTimeDependentModel();
    static void testAlanLewisReferencePrices();
};

int main(int, char* []) {
    // FLOATING_POINT_EXCEPTION
    HestonModelTest::testBlackCalibration();
    // FLOATING_POINT_EXCEPTION
    HestonModelTest::testDAXCalibration();
    // FLOATING_POINT_EXCEPTION
    HestonModelTest::testAnalyticVsBlack();
    HestonModelTest::testAnalyticVsCached();
    HestonModelTest::testKahlJaeckelCase();
    HestonModelTest::testDifferentIntegrals();
    HestonModelTest::testFdBarrierVsCached();
    HestonModelTest::testFdVanillaVsCached();
    HestonModelTest::testMultipleStrikesEngine();
    HestonModelTest::testMcVsCached();
    HestonModelTest::testAnalyticPiecewiseTimeDependent();
    HestonModelTest::testDAXCalibrationOfTimeDependentModel();
    HestonModelTest::testAlanLewisReferencePrices();
}

namespace {

    struct CalibrationMarketData {
        Handle<Quote> s0;
        Handle<YieldTermStructure> riskFreeTS, dividendYield;
        std::vector<boost::shared_ptr<CalibrationHelper> > options;
    };

    CalibrationMarketData getDAXCalibrationMarketData() {
        /* this example is taken from A. Sepp
           Pricing European-Style Options under Jump Diffusion Processes
           with Stochstic Volatility: Applications of Fourier Transform
           http://math.ut.ee/~spartak/papers/stochjumpvols.pdf
        */

        Date settlementDate(Settings::instance().evaluationDate());

        DayCounter dayCounter = Actual365Fixed();
        Calendar calendar = TARGET();

        Integer t[] = { 13, 41, 75, 165, 256, 345, 524, 703 };
        Rate r[] = { 0.0357,0.0349,0.0341,0.0355,0.0359,0.0368,0.0386,0.0401 };

        std::vector<Date> dates;
        std::vector<Rate> rates;
        dates.push_back(settlementDate);
        rates.push_back(0.0357);
        Size i;
        for (i = 0; i < 8; ++i) {
            dates.push_back(settlementDate + t[i]);
            rates.push_back(r[i]);
        }
        // FLOATING_POINT_EXCEPTION
        Handle<YieldTermStructure> riskFreeTS(
                           boost::shared_ptr<YieldTermStructure>(
                                      new ZeroCurve(dates, rates, dayCounter)));

        Handle<YieldTermStructure> dividendYield(
                                    flatRate(settlementDate, 0.0, dayCounter));

        Volatility v[] =
          { 0.6625,0.4875,0.4204,0.3667,0.3431,0.3267,0.3121,0.3121,
            0.6007,0.4543,0.3967,0.3511,0.3279,0.3154,0.2984,0.2921,
            0.5084,0.4221,0.3718,0.3327,0.3155,0.3027,0.2919,0.2889,
            0.4541,0.3869,0.3492,0.3149,0.2963,0.2926,0.2819,0.2800,
            0.4060,0.3607,0.3330,0.2999,0.2887,0.2811,0.2751,0.2775,
            0.3726,0.3396,0.3108,0.2781,0.2788,0.2722,0.2661,0.2686,
            0.3550,0.3277,0.3012,0.2781,0.2781,0.2661,0.2661,0.2681,
            0.3428,0.3209,0.2958,0.2740,0.2688,0.2627,0.2580,0.2620,
            0.3302,0.3062,0.2799,0.2631,0.2573,0.2533,0.2504,0.2544,
            0.3343,0.2959,0.2705,0.2540,0.2504,0.2464,0.2448,0.2462,
            0.3460,0.2845,0.2624,0.2463,0.2425,0.2385,0.2373,0.2422,
            0.3857,0.2860,0.2578,0.2399,0.2357,0.2327,0.2312,0.2351,
            0.3976,0.2860,0.2607,0.2356,0.2297,0.2268,0.2241,0.2320 };

        Handle<Quote> s0(boost::shared_ptr<Quote>(new SimpleQuote(4468.17)));
        Real strike[] = { 3400,3600,3800,4000,4200,4400,
                          4500,4600,4800,5000,5200,5400,5600 };

        std::vector<boost::shared_ptr<CalibrationHelper> > options;

        for (Size s = 0; s < 13; ++s) {
            for (Size m = 0; m < 8; ++m) {
                Handle<Quote> vol(boost::shared_ptr<Quote>(
                                                    new SimpleQuote(v[s*8+m])));

                Period maturity((int)((t[m]+3)/7.), Weeks); // round to weeks
                options.push_back(boost::shared_ptr<CalibrationHelper>(
                        new HestonModelHelper(maturity, calendar,
                                              s0->value(), strike[s], vol,
                                              riskFreeTS, dividendYield,
                                          CalibrationHelper::ImpliedVolError)));
            }
        }

        CalibrationMarketData marketData
                                    ={ s0, riskFreeTS, dividendYield, options };

        return marketData;
    }

}


void HestonModelTest::testBlackCalibration() {
    BOOST_MESSAGE(
       "Testing Heston model calibration using a flat volatility surface...");

    SavedSettings backup;

    /* calibrate a Heston model to a constant volatility surface without
       smile. expected result is a vanishing volatility of the volatility.
       In addition theta and v0 should be equal to the constant variance */

    Date today = Date::todaysDate();
    Settings::instance().evaluationDate() = today;

    DayCounter dayCounter = Actual360();
    Calendar calendar = NullCalendar();

    Handle<YieldTermStructure> riskFreeTS(flatRate(0.04, dayCounter));
    Handle<YieldTermStructure> dividendTS(flatRate(0.50, dayCounter));

    std::vector<Period> optionMaturities;
    optionMaturities.push_back(Period(1, Months));
    optionMaturities.push_back(Period(2, Months));
    optionMaturities.push_back(Period(3, Months));
    optionMaturities.push_back(Period(6, Months));
    optionMaturities.push_back(Period(9, Months));
    optionMaturities.push_back(Period(1, Years));
    optionMaturities.push_back(Period(2, Years));

    std::vector<boost::shared_ptr<CalibrationHelper> > options;
    Handle<Quote> s0(boost::shared_ptr<Quote>(new SimpleQuote(1.0)));
    Handle<Quote> vol(boost::shared_ptr<Quote>(new SimpleQuote(0.1)));
    Volatility volatility = vol->value();

    for (Size i = 0; i < optionMaturities.size(); ++i) {
        for (Real moneyness = -1.0; moneyness < 2.0; moneyness += 1.0) {
            // FLOATING_POINT_EXCEPTION
            const Time tau = dayCounter.yearFraction(
                                 riskFreeTS->referenceDate(),
                                 calendar.advance(riskFreeTS->referenceDate(),
                                                  optionMaturities[i]));
        const Real fwdPrice = s0->value()*dividendTS->discount(tau)
                            / riskFreeTS->discount(tau);
        const Real strikePrice = fwdPrice * std::exp(-moneyness * volatility
                                                     * std::sqrt(tau));

        options.push_back(boost::shared_ptr<CalibrationHelper>(
                          new HestonModelHelper(optionMaturities[i], calendar,
                                                s0->value(), strikePrice, vol,
                                                riskFreeTS, dividendTS)));
        }
    }

    std::cout << "sigma\tv0\tkappa\ttheta\trho" << std::endl;

    for (Real sigma = 0.1; sigma < 0.7; sigma += 0.2) {
        const Real v0=0.01;
        const Real kappa=0.2;
        const Real theta=0.02;
        const Real rho=-0.75;

        boost::shared_ptr<HestonProcess> process(
            new HestonProcess(riskFreeTS, dividendTS,
                              s0, v0, kappa, theta, sigma, rho));

        boost::shared_ptr<HestonModel> model(new HestonModel(process));
        boost::shared_ptr<PricingEngine> engine(
                                         new AnalyticHestonEngine(model, 96));

        for (Size i = 0; i < options.size(); ++i)
            options[i]->setPricingEngine(engine);

        LevenbergMarquardt om(1e-8, 1e-8, 1e-8);
        model->calibrate(options, om, EndCriteria(400, 40, 1.0e-8,
                                                  1.0e-8, 1.0e-8));

        Real tolerance = 3.0e-3;

        if (model->sigma() > tolerance) {
            BOOST_ERROR("Failed to reproduce expected sigma"
                        << "\n    calculated: " << model->sigma()
                        << "\n    expected:   " << 0.0
                        << "\n    tolerance:  " << tolerance);
        }

        if (std::fabs(model->kappa()
                  *(model->theta()-volatility*volatility)) > tolerance) {
            BOOST_ERROR("Failed to reproduce expected theta"
                        << "\n    calculated: " << model->theta()
                        << "\n    expected:   " << volatility*volatility);
        }

        if (std::fabs(model->v0()-volatility*volatility) > tolerance) {
            BOOST_ERROR("Failed to reproduce expected v0"
                        << "\n    calculated: " << model->v0()
                        << "\n    expected:   " << volatility*volatility);
        }

        std::cout << sigma /*model->sigma()*/ << "\t"
                  << model->v0() << "\t"
                  << model->kappa() << "\t"
                  << model->theta() << "\t"
                  << model->rho() << std::endl;
    }
}


void HestonModelTest::testDAXCalibration() {

    BOOST_MESSAGE(
             "Testing Heston model calibration using DAX volatility data...");

    SavedSettings backup;

    Date settlementDate(5, July, 2002);
    Settings::instance().evaluationDate() = settlementDate;

    CalibrationMarketData marketData = getDAXCalibrationMarketData();

    const Handle<YieldTermStructure> riskFreeTS = marketData.riskFreeTS;
    const Handle<YieldTermStructure> dividendTS = marketData.dividendYield;
    const Handle<Quote> s0 = marketData.s0;

    const std::vector<boost::shared_ptr<CalibrationHelper> > options
                                                    = marketData.options;

    const Real v0=0.1;
    const Real kappa=1.0;
    const Real theta=0.1;
    const Real sigma=0.5;
    const Real rho=-0.5;

    boost::shared_ptr<HestonProcess> process(new HestonProcess(
              riskFreeTS, dividendTS, s0, v0, kappa, theta, sigma, rho));

    boost::shared_ptr<HestonModel> model(new HestonModel(process));

    boost::shared_ptr<PricingEngine> engine(
                                         new AnalyticHestonEngine(model, 64));

    for (Size i = 0; i < options.size(); ++i)
        options[i]->setPricingEngine(engine);

    LevenbergMarquardt om(1e-8, 1e-8, 1e-8);
    model->calibrate(options, om, EndCriteria(400, 40, 1.0e-8, 1.0e-8, 1.0e-8));

    Real sse = 0;
    for (Size i = 0; i < 13*8; ++i) {
        const Real diff = options[i]->calibrationError()*100.0;
        sse += diff*diff;
    }
    Real expected = 177.2; //see article by A. Sepp.
    if (std::fabs(sse - expected) > 1.0) {
        BOOST_FAIL("Failed to reproduce calibration error"
                   << "\n    calculated: " << sse
                   << "\n    expected:   " << expected);
    }

    std::cout << "Calibrated option value: " << std::endl;
    for (Size i = 0; i < 13*8; ++i) {
        std::cout << i << "\t" << options[i]->marketValue() << std::endl;
    }
}

void HestonModelTest::testAnalyticVsBlack() {
    BOOST_MESSAGE("Testing analytic Heston engine against Black formula...");

    SavedSettings backup;

    Date settlementDate = Date::todaysDate();
    Settings::instance().evaluationDate() = settlementDate;
    DayCounter dayCounter = ActualActual();
    Date exerciseDate = settlementDate + 6*Months;

    boost::shared_ptr<StrikedTypePayoff> payoff(
                                     new PlainVanillaPayoff(Option::Put, 30));
    boost::shared_ptr<Exercise> exercise(new EuropeanExercise(exerciseDate));

    Handle<YieldTermStructure> riskFreeTS(flatRate(0.1, dayCounter));
    Handle<YieldTermStructure> dividendTS(flatRate(0.04, dayCounter));

    Handle<Quote> s0(boost::shared_ptr<Quote>(new SimpleQuote(32.0)));

    const Real v0=0.05;
    const Real kappa=5.0;
    const Real theta=0.05;
    const Real sigma=1.0e-4;
    const Real rho=0.0;

    boost::shared_ptr<HestonProcess> process(new HestonProcess(
                   riskFreeTS, dividendTS, s0, v0, kappa, theta, sigma, rho));

    VanillaOption option(payoff, exercise);
    // FLOATING_POINT_EXCEPTION
    boost::shared_ptr<PricingEngine> engine(new AnalyticHestonEngine(
              boost::shared_ptr<HestonModel>(new HestonModel(process)), 144));

    option.setPricingEngine(engine);
    Real calculated = option.NPV();

    Real yearFraction = dayCounter.yearFraction(settlementDate, exerciseDate);
    Real forwardPrice = 32*std::exp((0.1-0.04)*yearFraction);
    Real expected = blackFormula(payoff->optionType(), payoff->strike(),
        forwardPrice, std::sqrt(0.05*yearFraction)) *
                                            std::exp(-0.1*yearFraction);
    Real error = std::fabs(calculated - expected);
    Real tolerance = 2.0e-7;
    if (error > tolerance) {
        BOOST_FAIL("failed to reproduce Black price with AnalyticHestonEngine"
                   << "\n    calculated: " << calculated
                   << "\n    expected:   " << expected
                   << "\n    error:      " << QL_SCIENTIFIC << error);
    }

    engine = boost::shared_ptr<PricingEngine>(new FdHestonVanillaEngine(
              boost::shared_ptr<HestonModel>(new HestonModel(process)),
              200,200,100));
    option.setPricingEngine(engine);

    calculated = option.NPV();
    error = std::fabs(calculated - expected);
    tolerance = 1.0e-3;
    if (error > tolerance) {
        BOOST_FAIL("failed to reproduce Black price with FdHestonVanillaEngine"
                   << "\n    calculated: " << calculated
                   << "\n    expected:   " << expected
                   << "\n    error:      " << QL_SCIENTIFIC << error);
    }

    std::cout << "European call option price with analytic Heston engine: "
              << calculated << std::endl;

}


void HestonModelTest::testAnalyticVsCached() {
    BOOST_MESSAGE("Testing analytic Heston engine against cached values...");

    SavedSettings backup;

    Date settlementDate(27, December, 2004);
    Settings::instance().evaluationDate() = settlementDate;
    DayCounter dayCounter = ActualActual();
    Date exerciseDate(28, March, 2005);

    boost::shared_ptr<StrikedTypePayoff> payoff(
                                  new PlainVanillaPayoff(Option::Call, 1.05));
    boost::shared_ptr<Exercise> exercise(new EuropeanExercise(exerciseDate));

    Handle<YieldTermStructure> riskFreeTS(flatRate(0.0225, dayCounter));
    Handle<YieldTermStructure> dividendTS(flatRate(0.02, dayCounter));

    Handle<Quote> s0(boost::shared_ptr<Quote>(new SimpleQuote(1.0)));
    const Real v0 = 0.1;
    const Real kappa = 3.16;
    const Real theta = 0.09;
    const Real sigma = 0.4;
    const Real rho = -0.2;

    boost::shared_ptr<HestonProcess> process(new HestonProcess(
                   riskFreeTS, dividendTS, s0, v0, kappa, theta, sigma, rho));

    VanillaOption option(payoff, exercise);

    boost::shared_ptr<AnalyticHestonEngine> engine(new AnalyticHestonEngine(
               boost::shared_ptr<HestonModel>(new HestonModel(process)), 64));

    option.setPricingEngine(engine);

    Real expected1 = 0.0404774515;
    Real calculated1 = option.NPV();
    Real tolerance = 1.0e-8;

    if (std::fabs(calculated1 - expected1) > tolerance) {
        BOOST_ERROR("Failed to reproduce cached analytic price"
                    << "\n    calculated: " << calculated1
                    << "\n    expected:   " << expected1);
    }

    std::cout << "Analytic Heston call option price: "
              << calculated1 << std::endl;


    // reference values from www.wilmott.com, technical forum
    // search for "Heston or VG price check"

    Real K[] = {0.9,1.0,1.1};
    Real expected2[] = { 0.1330371,0.0641016, 0.0270645 };
    Real calculated2[6];

    Size i;
    for (i = 0; i < 6; ++i) {
        Date exerciseDate(8+i/3, September, 2005);

        boost::shared_ptr<StrikedTypePayoff> payoff(
                                new PlainVanillaPayoff(Option::Call, K[i%3]));
        boost::shared_ptr<Exercise> exercise(
                                          new EuropeanExercise(exerciseDate));

        Handle<YieldTermStructure> riskFreeTS(flatRate(0.05, dayCounter));
        Handle<YieldTermStructure> dividendTS(flatRate(0.02, dayCounter));

        Real s = riskFreeTS->discount(0.7)/dividendTS->discount(0.7);
        Handle<Quote> s0(boost::shared_ptr<Quote>(new SimpleQuote(s)));

        boost::shared_ptr<HestonProcess> process(new HestonProcess(
                   riskFreeTS, dividendTS, s0, 0.09, 1.2, 0.08, 1.8, -0.45));

        VanillaOption option(payoff, exercise);

        boost::shared_ptr<PricingEngine> engine(new AnalyticHestonEngine(
                   boost::shared_ptr<HestonModel>(new HestonModel(process))));

        option.setPricingEngine(engine);
        calculated2[i] = option.NPV();
    }

    // we are after the value for T=0.7
    Time t1 = dayCounter.yearFraction(settlementDate, Date(8, September,2005));
    Time t2 = dayCounter.yearFraction(settlementDate, Date(9, September,2005));

    for (i = 0; i < 3; ++i) {
        const Real interpolated =
            calculated2[i]+(calculated2[i+3]-calculated2[i])/(t2-t1)*(0.7-t1);

        if (std::fabs(interpolated - expected2[i]) > 100*tolerance) {
            BOOST_ERROR("Failed to reproduce cached analytic prices:"
                        << "\n    calculated: " << interpolated
                        << "\n    expected:   " << expected2[i] );
        }
    }
}


void HestonModelTest::testMcVsCached() {
    BOOST_MESSAGE(
                "Testing Monte Carlo Heston engine against cached values...");

    SavedSettings backup;

    Date settlementDate(27, December, 2004);
    Settings::instance().evaluationDate() = settlementDate;

    DayCounter dayCounter = ActualActual();
    Date exerciseDate(28, March, 2005);

    boost::shared_ptr<StrikedTypePayoff> payoff(
                                   new PlainVanillaPayoff(Option::Put, 1.05));
    boost::shared_ptr<Exercise> exercise(new EuropeanExercise(exerciseDate));

    Handle<YieldTermStructure> riskFreeTS(flatRate(0.7, dayCounter));
    Handle<YieldTermStructure> dividendTS(flatRate(0.4, dayCounter));

    Handle<Quote> s0(boost::shared_ptr<Quote>(new SimpleQuote(1.05)));

    boost::shared_ptr<HestonProcess> process(new HestonProcess(
                   riskFreeTS, dividendTS, s0, 0.3, 1.16, 0.2, 0.8, 0.8,
                   HestonProcess::QuadraticExponentialMartingale));

    VanillaOption option(payoff, exercise);

    boost::shared_ptr<PricingEngine> engine;
    engine = MakeMCEuropeanHestonEngine<PseudoRandom>(process)
        .withStepsPerYear(11)
        .withAntitheticVariate()
        .withSamples(50000)
        .withSeed(1234);

    option.setPricingEngine(engine);

    Real expected = 0.0632851308977151;
    Real calculated = option.NPV();
    Real errorEstimate = option.errorEstimate();
    Real tolerance = 7.5e-4;

    if (std::fabs(calculated - expected) > 2.34*errorEstimate) {
        BOOST_ERROR("Failed to reproduce cached price"
                    << "\n    calculated: " << calculated
                    << "\n    expected:   " << expected
                    << " +/- " << errorEstimate);
    }

    if (errorEstimate > tolerance) {
        BOOST_ERROR("failed to reproduce error estimate"
                    << "\n    calculated: " << errorEstimate
                    << "\n    expected:   " << tolerance);
    }

    std::cout << "European vanilla put option price "
              << "with Monte Carlo Heston engine: "
              << calculated << std::endl;
}

void HestonModelTest::testFdBarrierVsCached() {
    BOOST_MESSAGE("Testing FD barrier Heston engine against cached values...");

    SavedSettings backup;

    DayCounter dc = Actual360();
    Date today = Date::todaysDate();

    Handle<Quote> s0(boost::shared_ptr<Quote>(new SimpleQuote(100.0)));
    Handle<YieldTermStructure> rTS(flatRate(today, 0.08, dc));
    Handle<YieldTermStructure> qTS(flatRate(today, 0.04, dc));

    Date exDate = today + Integer(0.5*360+0.5);
    boost::shared_ptr<Exercise> exercise(new EuropeanExercise(exDate));

    boost::shared_ptr<StrikedTypePayoff> payoff(new
            PlainVanillaPayoff(Option::Call, 90.0));

    boost::shared_ptr<HestonProcess> process(new HestonProcess(
            rTS, qTS, s0, 0.25*0.25, 1.0, 0.25*0.25, 0.001, 0.0));

    boost::shared_ptr<PricingEngine> engine;
    engine = boost::shared_ptr<PricingEngine>(new FdHestonBarrierEngine(
                    boost::shared_ptr<HestonModel>(new HestonModel(process)),
                    200,400,100));

    BarrierOption option(Barrier::DownOut, 95.0, 3.0, payoff, exercise);
    option.setPricingEngine(engine);

    Real calculated = option.NPV();
    Real expected = 9.0246;
    Real error = std::fabs(calculated-expected);
    if (error > 1.0e-3) {
        BOOST_FAIL("failed to reproduce cached price with FD Barrier engine"
                   << "\n    calculated: " << calculated
                   << "\n    expected:   " << expected
                   << "\n    error:      " << QL_SCIENTIFIC << error);
    }

    option = BarrierOption(Barrier::DownIn, 95.0, 3.0, payoff, exercise);
    option.setPricingEngine(engine);

    calculated = option.NPV();
    expected = 7.7627;
    error = std::fabs(calculated-expected);
    if (error > 1.0e-3) {
        BOOST_FAIL("failed to reproduce cached price with FD Barrier engine"
                   << "\n    calculated: " << calculated
                   << "\n    expected:   " << expected
                   << "\n    error:      " << QL_SCIENTIFIC << error);
    }
}

void HestonModelTest::testFdVanillaVsCached() {
    BOOST_MESSAGE("Testing FD vanilla Heston engine against cached values...");

    SavedSettings backup;

    Date settlementDate(27, December, 2004);
    Settings::instance().evaluationDate() = settlementDate;

    DayCounter dayCounter = ActualActual();
    Date exerciseDate(28, March, 2005);

    boost::shared_ptr<StrikedTypePayoff> payoff(
                                   new PlainVanillaPayoff(Option::Put, 1.05));
    boost::shared_ptr<Exercise> exercise(new EuropeanExercise(exerciseDate));

    Handle<YieldTermStructure> riskFreeTS(flatRate(0.7, dayCounter));
    Handle<YieldTermStructure> dividendTS(flatRate(0.4, dayCounter));

    Handle<Quote> s0(boost::shared_ptr<Quote>(new SimpleQuote(1.05)));

    VanillaOption option(payoff, exercise);

    boost::shared_ptr<HestonProcess> process(new HestonProcess(
                   riskFreeTS, dividendTS, s0, 0.3, 1.16, 0.2, 0.8, 0.8));

    boost::shared_ptr<PricingEngine> engine;
    engine = boost::shared_ptr<PricingEngine>(new FdHestonVanillaEngine(
                    boost::shared_ptr<HestonModel>(new HestonModel(process)),
                    100,200,100));
    option.setPricingEngine(engine);

    Real expected = 0.06325;
    Real calculated = option.NPV();
    Real error = std::fabs(calculated - expected);
    Real tolerance = 1.0e-4;

    if (error > tolerance) {
        BOOST_FAIL("failed to reproduce cached price with FD engine"
                   << "\n    calculated: " << calculated
                   << "\n    expected:   " << expected
                   << "\n    error:      " << QL_SCIENTIFIC << error);
    }

    BOOST_MESSAGE("Testing FD vanilla Heston engine for discrete dividends...");

    payoff = boost::shared_ptr<StrikedTypePayoff>(
                          new PlainVanillaPayoff(Option::Call, 95.0));
    s0 = Handle<Quote>(boost::shared_ptr<Quote>(new SimpleQuote(100.0)));

    riskFreeTS = Handle<YieldTermStructure>(flatRate(0.05, dayCounter));
    dividendTS = Handle<YieldTermStructure>(flatRate(0.0, dayCounter));

    exerciseDate = Date(28, March, 2006);
    exercise = boost::shared_ptr<Exercise>(new EuropeanExercise(exerciseDate));

    std::vector<Date> dividendDates;
    std::vector<Real> dividends;
    for (Date d = settlementDate + 3*Months;
              d < exercise->lastDate();
              d += 6*Months) {
        dividendDates.push_back(d);
        dividends.push_back(1.0);
    }

    DividendVanillaOption divOption(payoff, exercise,
                                    dividendDates, dividends);
    process = boost::shared_ptr<HestonProcess>(new HestonProcess(
                   riskFreeTS, dividendTS, s0, 0.04, 1.0, 0.04, 0.001, 0.0));
    engine = boost::shared_ptr<PricingEngine>(new FdHestonVanillaEngine(
                    boost::shared_ptr<HestonModel>(new HestonModel(process)),
                    200,400,100));
    divOption.setPricingEngine(engine);
    calculated = divOption.NPV();
    // Value calculated with an independent FD framework, validated with
    // an independent MC framework
    expected = 12.946;
    error = std::fabs(calculated - expected);
    tolerance = 5.0e-3;

    if (error > tolerance) {
        BOOST_FAIL("failed to reproduce discrete dividend price with FD engine"
                   << "\n    calculated: " << calculated
                   << "\n    expected:   " << expected
                   << "\n    error:      " << QL_SCIENTIFIC << error);
    }

    BOOST_MESSAGE("Testing FD vanilla Heston engine for american exercise...");

    dividendTS = Handle<YieldTermStructure>(flatRate(0.03, dayCounter));
    process = boost::shared_ptr<HestonProcess>(new HestonProcess(
                   riskFreeTS, dividendTS, s0, 0.04, 1.0, 0.04, 0.001, 0.0));
    engine = boost::shared_ptr<PricingEngine>(new FdHestonVanillaEngine(
                    boost::shared_ptr<HestonModel>(new HestonModel(process)),
                    200,400,100));
    payoff = boost::shared_ptr<StrikedTypePayoff>(
                          new PlainVanillaPayoff(Option::Put, 95.0));
    exercise = boost::shared_ptr<Exercise>(new AmericanExercise(
            settlementDate, exerciseDate));
    option = VanillaOption(payoff, exercise);
    option.setPricingEngine(engine);
    calculated = option.NPV();

    Handle<BlackVolTermStructure> volTS(flatVol(settlementDate, 0.2,
                                                  dayCounter));
    boost::shared_ptr<BlackScholesMertonProcess> ref_process(
        new BlackScholesMertonProcess(s0, dividendTS, riskFreeTS, volTS));
    boost::shared_ptr<PricingEngine> ref_engine(
                  new FDAmericanEngine<CrankNicolson>(ref_process, 200, 400));
    option.setPricingEngine(ref_engine);
    expected = option.NPV();

    error = std::fabs(calculated - expected);
    tolerance = 1.0e-3;

    if (error > tolerance) {
        BOOST_FAIL("failed to reproduce american option price with FD engine"
                   << "\n    calculated: " << calculated
                   << "\n    expected:   " << expected
                   << "\n    error:      " << QL_SCIENTIFIC << error);
    }
}

void HestonModelTest::testKahlJaeckelCase() {
    BOOST_MESSAGE(
          "Testing MC and FD Heston engines for the Kahl-Jaeckel example...");

    /* Example taken from Wilmott mag (Sept. 2005).
       "Not-so-complex logarithms in the Heston model",
       Example was also discussed within the Wilmott thread
       "QuantLib code is very high quatlity"
    */

    SavedSettings backup;

    Date settlementDate(30, March, 2007);
    Settings::instance().evaluationDate() = settlementDate;

    DayCounter dayCounter = ActualActual();
    Date exerciseDate(30, March, 2017);

    boost::shared_ptr<StrikedTypePayoff> payoff(
                                   new PlainVanillaPayoff(Option::Call, 200));
    boost::shared_ptr<Exercise> exercise(new EuropeanExercise(exerciseDate));

    Handle<YieldTermStructure> riskFreeTS(flatRate(0.0, dayCounter));
    Handle<YieldTermStructure> dividendTS(flatRate(0.0, dayCounter));

    Handle<Quote> s0(boost::shared_ptr<Quote>(new SimpleQuote(100)));

    boost::shared_ptr<HestonProcess> processNonCentral(new HestonProcess(
                   riskFreeTS, dividendTS, s0, 0.16, 1.0, 0.16, 2.0, -0.8,
                   HestonProcess::NonCentralChiSquareVariance));

    boost::shared_ptr<HestonProcess> processQE_M(new HestonProcess(
                   riskFreeTS, dividendTS, s0, 0.16, 1.0, 0.16, 2.0, -0.8,
                   HestonProcess::QuadraticExponentialMartingale));


    VanillaOption option(payoff, exercise);

    Real tolerance = 0.1;

    boost::shared_ptr<PricingEngine> engine =
        MakeMCEuropeanHestonEngine<PseudoRandom>(processNonCentral)
        .withSteps(10)
        .withAntitheticVariate()
        .withAbsoluteTolerance(tolerance)
        .withSeed(1234);
    option.setPricingEngine(engine);

    const Real expected = 4.95212;
          Real calculated = option.NPV();
          Real errorEstimate = option.errorEstimate();

    if (std::fabs(calculated - expected) > 2.34*errorEstimate) {
        BOOST_ERROR("Failed to reproduce cached price with MC engine"
                    << "\n    calculated: " << calculated
                    << "\n    expected:   " << expected
                    << " +/- " << errorEstimate);
    }

    if (errorEstimate > tolerance) {
        BOOST_ERROR("failed to reproduce error estimate with MC engine"
                    << "\n    calculated: " << errorEstimate
                    << "\n    expected:   " << tolerance);
    }

    engine =
        MakeMCEuropeanHestonEngine<PseudoRandom>(processQE_M)
        .withSteps(100)
        .withAntitheticVariate()
        .withAbsoluteTolerance(tolerance)
        .withSeed(1234);
    option.setPricingEngine(engine);

    calculated = option.NPV();
    errorEstimate = option.errorEstimate();

    if (std::fabs(calculated - expected) > 2.34*errorEstimate) {
        BOOST_ERROR("Failed to reproduce cached price with MC engine"
                    << "\n    calculated: " << calculated
                    << "\n    expected:   " << expected
                    << " +/- " << errorEstimate);
    }

    if (errorEstimate > tolerance) {
        BOOST_ERROR("failed to reproduce error estimate with MC engine"
                    << "\n    calculated: " << errorEstimate
                    << "\n    expected:   " << tolerance);
    }

    engine = boost::shared_ptr<PricingEngine>(new FdHestonVanillaEngine(
                 boost::shared_ptr<HestonModel>(new HestonModel(processQE_M)),
                 200,400,100));
    option.setPricingEngine(engine);

    calculated = option.NPV();
    const Real error = std::fabs(calculated - expected);
    tolerance = 5.0e-2;
    if (error > tolerance) {
        BOOST_FAIL("failed to reproduce cached price with FD engine"
                   << "\n    calculated: " << calculated
                   << "\n    expected:   " << expected
                   << "\n    error:      " << QL_SCIENTIFIC << error);
    }
}

namespace {
    struct HestonParameter {
        Real v0, kappa, theta, sigma, rho; };
}

void HestonModelTest::testDifferentIntegrals() {
    BOOST_MESSAGE(
       "Testing different numerical Heston integration algorithms...");

    SavedSettings backup;

    const Date settlementDate(27, December, 2004);
    Settings::instance().evaluationDate() = settlementDate;

    const DayCounter dayCounter = ActualActual();

    Handle<YieldTermStructure> riskFreeTS(flatRate(0.05, dayCounter));
    Handle<YieldTermStructure> dividendTS(flatRate(0.03, dayCounter));

    const Real strikes[] = { 0.5, 0.7, 1.0, 1.25, 1.5, 2.0 };
    const Integer maturities[] = { 1, 2, 3, 12, 60, 120, 360};
    const Option::Type types[] ={ Option::Put, Option::Call };

    const HestonParameter equityfx      = { 0.07, 2.0, 0.04, 0.55, -0.8 };
    const HestonParameter highCorr      = { 0.07, 1.0, 0.04, 0.55,  0.995 };
    const HestonParameter lowVolOfVol   = { 0.07, 1.0, 0.04, 0.025, -0.75 };
    const HestonParameter highVolOfVol  = { 0.07, 1.0, 0.04, 5.0, -0.75 };
    const HestonParameter kappaEqSigRho = { 0.07, 0.4, 0.04, 0.5, 0.8 };

    std::vector<HestonParameter> params;
    params.push_back(equityfx);
    params.push_back(highCorr);
    params.push_back(lowVolOfVol);
    params.push_back(highVolOfVol);
    params.push_back(kappaEqSigRho);

    const Real tol[] = { 1e-3, 1e-3, 0.2, 0.01, 1e-3 };

    for (std::vector<HestonParameter>::const_iterator iter = params.begin();
         iter != params.end(); ++iter) {

        Handle<Quote> s0(boost::shared_ptr<Quote>(new SimpleQuote(1.0)));
        boost::shared_ptr<HestonProcess> process(new HestonProcess(
            riskFreeTS, dividendTS,
            s0, iter->v0, iter->kappa,
            iter->theta, iter->sigma, iter->rho));

        boost::shared_ptr<HestonModel> model(new HestonModel(process));

        boost::shared_ptr<AnalyticHestonEngine> lobattoEngine(
                              new AnalyticHestonEngine(model, 1e-10,
                                                       1000000));
        boost::shared_ptr<AnalyticHestonEngine> laguerreEngine(
                                        new AnalyticHestonEngine(model, 128));
        boost::shared_ptr<AnalyticHestonEngine> legendreEngine(
            new AnalyticHestonEngine(
                model, AnalyticHestonEngine::Gatheral,
                AnalyticHestonEngine::Integration::gaussLegendre(512)));
        boost::shared_ptr<AnalyticHestonEngine> chebyshevEngine(
            new AnalyticHestonEngine(
                model, AnalyticHestonEngine::Gatheral,
                AnalyticHestonEngine::Integration::gaussChebyshev(512)));
        boost::shared_ptr<AnalyticHestonEngine> chebyshev2ndEngine(
            new AnalyticHestonEngine(
                model, AnalyticHestonEngine::Gatheral,
                AnalyticHestonEngine::Integration::gaussChebyshev2nd(512)));

        Real maxLegendreDiff    = 0.0;
        Real maxChebyshevDiff   = 0.0;
        Real maxChebyshev2ndDiff= 0.0;
        Real maxLaguerreDiff    = 0.0;

        for (Size i=0; i < LENGTH(maturities); ++i) {
            boost::shared_ptr<Exercise> exercise(
                new EuropeanExercise(settlementDate
                                     + Period(maturities[i], Months)));

            for (Size j=0; j < LENGTH(strikes); ++j) {
                for (Size k=0; k < LENGTH(types); ++k) {

                    boost::shared_ptr<StrikedTypePayoff> payoff(
                        new PlainVanillaPayoff(types[k], strikes[j]));

                    VanillaOption option(payoff, exercise);

                    option.setPricingEngine(lobattoEngine);
                    const Real lobattoNPV = option.NPV();

                    option.setPricingEngine(laguerreEngine);
                    const Real laguerre = option.NPV();

                    option.setPricingEngine(legendreEngine);
                    const Real legendre = option.NPV();

                    option.setPricingEngine(chebyshevEngine);
                    const Real chebyshev = option.NPV();

                    option.setPricingEngine(chebyshev2ndEngine);
                    const Real chebyshev2nd = option.NPV();

                    maxLaguerreDiff
                        = std::max(maxLaguerreDiff,
                                   std::fabs(lobattoNPV-laguerre));
                    maxLegendreDiff
                        = std::max(maxLegendreDiff,
                                   std::fabs(lobattoNPV-legendre));
                    maxChebyshevDiff
                        = std::max(maxChebyshevDiff,
                                   std::fabs(lobattoNPV-chebyshev));
                    maxChebyshev2ndDiff
                        = std::max(maxChebyshev2ndDiff,
                                   std::fabs(lobattoNPV-chebyshev2nd));

                }
            }
        }
        const Real maxDiff = std::max(std::max(
            std::max(maxLaguerreDiff,maxLegendreDiff),
                                     maxChebyshevDiff), maxChebyshev2ndDiff);

        const Real tr = tol[iter - params.begin()];
        if (maxDiff > tr) {
            BOOST_ERROR("Failed to reproduce Heston pricing values "
                        "within given tolerance"
                        << "\n    maxDifference: " << maxDiff
                        << "\n    tolerance:     " << tr);
        }
    }
}

void HestonModelTest::testMultipleStrikesEngine() {
    BOOST_MESSAGE("Testing multiple-strikes FD Heston engine...");

    SavedSettings backup;

    Date settlementDate(27, December, 2004);
    Settings::instance().evaluationDate() = settlementDate;

    DayCounter dayCounter = ActualActual();
    Date exerciseDate(28, March, 2006);

    boost::shared_ptr<Exercise> exercise(new EuropeanExercise(exerciseDate));

    Handle<YieldTermStructure> riskFreeTS(flatRate(0.06, dayCounter));
    Handle<YieldTermStructure> dividendTS(flatRate(0.02, dayCounter));

    Handle<Quote> s0(boost::shared_ptr<Quote>(new SimpleQuote(1.05)));

    boost::shared_ptr<HestonProcess> process(new HestonProcess(
                     riskFreeTS, dividendTS, s0, 0.16, 2.5, 0.09, 0.8, -0.8));
    boost::shared_ptr<HestonModel> model(new HestonModel(process));

    std::vector<Real> strikes;
    strikes.push_back(1.0);  strikes.push_back(0.5);
    strikes.push_back(0.75); strikes.push_back(1.5); strikes.push_back(2.0);

    boost::shared_ptr<FdHestonVanillaEngine> singleStrikeEngine(
                             new FdHestonVanillaEngine(model, 20, 400, 50));
    boost::shared_ptr<FdHestonVanillaEngine> multiStrikeEngine(
                             new FdHestonVanillaEngine(model, 20, 400, 50));
    multiStrikeEngine->enableMultipleStrikesCaching(strikes);

    Real relTol = 5e-3;
    for (Size i=0; i < strikes.size(); ++i) {
        boost::shared_ptr<StrikedTypePayoff> payoff(
                           new PlainVanillaPayoff(Option::Put, strikes[i]));

        VanillaOption aOption(payoff, exercise);
        aOption.setPricingEngine(multiStrikeEngine);

        Real npvCalculated   = aOption.NPV();
        Real deltaCalculated = aOption.delta();
        Real gammaCalculated = aOption.gamma();
        Real thetaCalculated = aOption.theta();

        aOption.setPricingEngine(singleStrikeEngine);
        Real npvExpected   = aOption.NPV();
        Real deltaExpected = aOption.delta();
        Real gammaExpected = aOption.gamma();
        Real thetaExpected = aOption.theta();

        if (std::fabs(npvCalculated-npvExpected)/npvExpected > relTol) {
            BOOST_FAIL("failed to reproduce price with FD multi strike engine"
                       << "\n    calculated: " << npvCalculated
                       << "\n    expected:   " << npvExpected
                       << "\n    error:      " << QL_SCIENTIFIC << relTol);
        }
        if (std::fabs(deltaCalculated-deltaExpected)/deltaExpected > relTol) {
            BOOST_FAIL("failed to reproduce delta with FD multi strike engine"
                       << "\n    calculated: " << deltaCalculated
                       << "\n    expected:   " << deltaExpected
                       << "\n    error:      " << QL_SCIENTIFIC << relTol);
        }
        if (std::fabs(gammaCalculated-gammaExpected)/gammaExpected > relTol) {
            BOOST_FAIL("failed to reproduce gamma with FD multi strike engine"
                       << "\n    calculated: " << gammaCalculated
                       << "\n    expected:   " << gammaExpected
                       << "\n    error:      " << QL_SCIENTIFIC << relTol);
        }
        if (std::fabs(thetaCalculated-thetaExpected)/thetaExpected > relTol) {
            BOOST_FAIL("failed to reproduce theta with FD multi strike engine"
                       << "\n    calculated: " << thetaCalculated
                       << "\n    expected:   " << thetaExpected
                       << "\n    error:      " << QL_SCIENTIFIC << relTol);
        }
    }
}



void HestonModelTest::testAnalyticPiecewiseTimeDependent() {
    BOOST_MESSAGE("Testing analytic piecewise time dependent Heston prices...");

    SavedSettings backup;

    Date settlementDate(27, December, 2004);
    Settings::instance().evaluationDate() = settlementDate;
    DayCounter dayCounter = ActualActual();
    Date exerciseDate(28, March, 2005);

    boost::shared_ptr<StrikedTypePayoff> payoff(
                                  new PlainVanillaPayoff(Option::Call, 1.0));
    boost::shared_ptr<Exercise> exercise(new EuropeanExercise(exerciseDate));

    std::vector<Date> dates;
    dates.push_back(settlementDate); dates.push_back(Date(01, January, 2007));
    std::vector<Rate> irates;
    irates.push_back(0.0); irates.push_back(0.2);
    Handle<YieldTermStructure> riskFreeTS(
            boost::shared_ptr<YieldTermStructure>(
                                    new ZeroCurve(dates, irates, dayCounter)));

    std::vector<Rate> qrates;
    qrates.push_back(0.0); qrates.push_back(0.3);
    Handle<YieldTermStructure> dividendTS(
            boost::shared_ptr<YieldTermStructure>(
                                    new ZeroCurve(dates, qrates, dayCounter)));


    const Real v0 = 0.1;
    Handle<Quote> s0(boost::shared_ptr<Quote>(new SimpleQuote(1.0)));

    ConstantParameter theta(0.09, PositiveConstraint());
    ConstantParameter kappa(3.16, PositiveConstraint());
    ConstantParameter sigma(4.40, PositiveConstraint());
    ConstantParameter rho  (-0.8, BoundaryConstraint(-1.0, 1.0));

    boost::shared_ptr<PiecewiseTimeDependentHestonModel> model(
        new PiecewiseTimeDependentHestonModel(riskFreeTS, dividendTS,
                                              s0, v0, theta, kappa,
                                              sigma, rho, TimeGrid(20.0, 2)));

    VanillaOption option(payoff, exercise);
    option.setPricingEngine(boost::shared_ptr<PricingEngine>(
                                           new AnalyticPTDHestonEngine(model)));

    const Real calculated = option.NPV();
    boost::shared_ptr<HestonProcess> hestonProcess(
        new HestonProcess(riskFreeTS, dividendTS, s0, v0,
                          kappa(0.0), theta(0.0), sigma(0.0), rho(0.0)));
    boost::shared_ptr<HestonModel> hestonModel(new HestonModel(hestonProcess));
    option.setPricingEngine(boost::shared_ptr<PricingEngine>(
                                    new AnalyticHestonEngine(hestonModel)));

    const Real expected = option.NPV();

    if (std::fabs(calculated-expected) > 1e-12) {
        BOOST_FAIL("failed to reproduce heston prices "
                   << "\n    calculated: " << calculated
                   << "\n    expected:   " << expected);
    }
}

void HestonModelTest::testDAXCalibrationOfTimeDependentModel() {
    BOOST_MESSAGE(
             "Testing Time dependent Heston model calibration ...");

    SavedSettings backup;

    Date settlementDate(5, July, 2002);
    Settings::instance().evaluationDate() = settlementDate;

    CalibrationMarketData marketData = getDAXCalibrationMarketData();

    const Handle<YieldTermStructure> riskFreeTS = marketData.riskFreeTS;
    const Handle<YieldTermStructure> dividendTS = marketData.dividendYield;
    const Handle<Quote> s0 = marketData.s0;

    const std::vector<boost::shared_ptr<CalibrationHelper> > options
                                                    = marketData.options;

    std::vector<Time> modelTimes;
    modelTimes.push_back(0.25);
    modelTimes.push_back(10.0);
    const TimeGrid modelGrid(modelTimes.begin(), modelTimes.end());

    const Real v0=0.1;
    ConstantParameter sigma( 0.5, PositiveConstraint());
    ConstantParameter theta( 0.1, PositiveConstraint());
    ConstantParameter rho( -0.5, BoundaryConstraint(-1.0, 1.0));

    std::vector<Time> pTimes(1, 0.25);
    PiecewiseConstantParameter kappa(pTimes, PositiveConstraint());

    for (Size i=0; i < pTimes.size()+1; ++i) {
        kappa.setParam(i, 10.0);
    }

    boost::shared_ptr<PiecewiseTimeDependentHestonModel> model(
        new PiecewiseTimeDependentHestonModel(riskFreeTS, dividendTS,
                                              s0, v0, theta, kappa,
                                              sigma, rho, modelGrid));

    boost::shared_ptr<PricingEngine> engine(new AnalyticPTDHestonEngine(model));
    for (Size i = 0; i < options.size(); ++i)
        options[i]->setPricingEngine(engine);

    LevenbergMarquardt om(1e-8, 1e-8, 1e-8);
    model->calibrate(options, om, EndCriteria(400, 40, 1.0e-8, 1.0e-8, 1.0e-8));

    Real sse = 0;
    for (Size i = 0; i < 13*8; ++i) {
        const Real diff = options[i]->calibrationError()*100.0;
        sse += diff*diff;
    }

    Real expected = 74.4;
    if (std::fabs(sse - expected) > 1.0) {
        BOOST_FAIL("Failed to reproduce calibration error"
                   << "\n    calculated: " << sse
                   << "\n    expected:   " << expected);
    }
}

void HestonModelTest::testAlanLewisReferencePrices() {
    BOOST_MESSAGE("Testing Alan Lewis Reference Prices ...");

    /*
     * testing Alan Lewis reference prices posted in
     * http://wilmott.com/messageview.cfm?catid=34&threadid=90957
     */

    SavedSettings backup;

    const Date settlementDate(5, July, 2002);
    Settings::instance().evaluationDate() = settlementDate;

    const Date maturityDate(5, July, 2003);
    const boost::shared_ptr<Exercise> exercise(
        new EuropeanExercise(maturityDate));

    const DayCounter dayCounter = Actual365Fixed();
    const Handle<YieldTermStructure> riskFreeTS(flatRate(0.01, dayCounter));
    const Handle<YieldTermStructure> dividendTS(flatRate(0.02, dayCounter));

    const Handle<Quote> s0(boost::shared_ptr<Quote>(new SimpleQuote(100.0)));

    const Real v0    =  0.04;
    const Real rho   = -0.5;
    const Real sigma =  1.0;
    const Real kappa =  4.0;
    const Real theta =  0.25;

    const boost::shared_ptr<HestonProcess> process(new HestonProcess(
        riskFreeTS, dividendTS, s0, v0, kappa, theta, sigma, rho));
    const boost::shared_ptr<HestonModel> model(new HestonModel(process));

    const boost::shared_ptr<PricingEngine> laguerreEngine(
        new AnalyticHestonEngine(model, 128u));

    const boost::shared_ptr<PricingEngine> gaussLobattoEngine(
        new AnalyticHestonEngine(model, QL_EPSILON, 100000u));

    const Real strikes[] = { 80, 90, 100, 110, 120 };
    const Option::Type types[] = { Option::Put, Option::Call };
    const boost::shared_ptr<PricingEngine> engines[]
        = { laguerreEngine, gaussLobattoEngine };

    const Real expectedResults[][2] = {
        { 7.958878113256768285213263077598987193482161301733,
          26.774758743998854221382195325726949201687074848341 },
        { 12.017966707346304987709573290236471654992071308187,
          20.933349000596710388139445766564068085476194042256 },
        { 17.055270961270109413522653999411000974895436309183,
          16.070154917028834278213466703938231827658768230714 },
        { 23.017825898442800538908781834822560777763225722188,
          12.132211516709844867860534767549426052805766831181 },
        { 29.811026202682471843340682293165857439167301370697,
          9.024913483457835636553375454092357136489051667150  }
    };

    const Real tol = 1e-12; // 3e-15 works on linux/ia32,
                            // but keep some buffer for other platforms

    for (Size i=0; i < LENGTH(strikes); ++i) {
        const Real strike = strikes[i];

        for (Size j=0; j < LENGTH(types); ++j) {
            const Option::Type type = types[j];

            for (Size k=0; k < LENGTH(engines); ++k) {
                const boost::shared_ptr<PricingEngine> engine = engines[k];

                const boost::shared_ptr<StrikedTypePayoff> payoff(
                    new PlainVanillaPayoff(type, strike));

                VanillaOption option(payoff, exercise);
                option.setPricingEngine(engine);

                const Real expected = expectedResults[i][j];
                const Real calculated = option.NPV();
                const Real relError = std::fabs(calculated-expected)/expected;

                if (relError > tol) {
                    BOOST_FAIL(
                           "failed to reproduce Alan Lewis Reference prices "
                        << "\n    strike     : " << strike
                        << "\n    option type: " << type
                        << "\n    engine type: " << k
                        << "\n    rel. error : " << relError);
                }
            }
        }
    }
}


//int main(int, char* []) {

//    try {

//        boost::timer timer;
//        std::cout << std::endl;

//        //---------------------------------------------------------------------------

//        // set up dates
//        Calendar calendar = TARGET();
//        Date todaysDate(15, May, 1998);
//        Date settlementDate(17, May, 1998);
//        Settings::instance().evaluationDate() = todaysDate;

//        // our options
//        Option::Type type(Option::Put);
//        Real underlying = 36;
//        Real strike = 40;
//        Spread dividendYield = 0.00;
//        Rate riskFreeRate = 0.06;
//        Volatility volatility = 0.20;
//        Date maturity(17, May, 1999);
//        DayCounter dayCounter = Actual365Fixed();

//        std::cout << "Option type = "  << type << std::endl;
//        std::cout << "Maturity = "        << maturity << std::endl;
//        std::cout << "Underlying price = "        << underlying << std::endl;
//        std::cout << "Strike = "                  << strike << std::endl;
//        std::cout << "Risk-free interest rate = " << io::rate(riskFreeRate)
//                  << std::endl;
//        std::cout << "Dividend yield = " << io::rate(dividendYield)
//                  << std::endl;
//        std::cout << "Volatility = " << io::volatility(volatility)
//                  << std::endl;
//        std::cout << std::endl;
//        std::string method;
//        std::cout << std::endl ;

//        // write column headings
//        Size widths[] = { 35, 14, 14, 14 };
//        std::cout << std::setw(widths[0]) << std::left << "Method"
//                  << std::setw(widths[1]) << std::left << "European"
//                  << std::setw(widths[2]) << std::left << "Bermudan"
//                  << std::setw(widths[3]) << std::left << "American"
//                  << std::endl;

//        std::vector<Date> exerciseDates;
//        for (Integer i=1; i<=4; i++)
//            exerciseDates.push_back(settlementDate + 3*i*Months);

//        boost::shared_ptr<Exercise> europeanExercise(
//                                         new EuropeanExercise(maturity));

//        boost::shared_ptr<Exercise> bermudanExercise(
//                                         new BermudanExercise(exerciseDates));

//        boost::shared_ptr<Exercise> americanExercise(
//                                         new AmericanExercise(settlementDate,
//                                                              maturity));

//        Handle<Quote> underlyingH(
//            boost::shared_ptr<Quote>(new SimpleQuote(underlying)));

//        // bootstrap the yield/dividend/vol curves
//        Handle<YieldTermStructure> flatTermStructure(
//            boost::shared_ptr<YieldTermStructure>(
//                new FlatForward(settlementDate, riskFreeRate, dayCounter)));
//        Handle<YieldTermStructure> flatDividendTS(
//            boost::shared_ptr<YieldTermStructure>(
//                new FlatForward(settlementDate, dividendYield, dayCounter)));
//        Handle<BlackVolTermStructure> flatVolTS(
//            boost::shared_ptr<BlackVolTermStructure>(
//                new BlackConstantVol(settlementDate, calendar, volatility,
//                                     dayCounter)));
//        boost::shared_ptr<StrikedTypePayoff> payoff(
//                                        new PlainVanillaPayoff(type, strike));
//        boost::shared_ptr<BlackScholesMertonProcess> bsmProcess(
//                 new BlackScholesMertonProcess(underlyingH, flatDividendTS,
//                                               flatTermStructure, flatVolTS));

//        // options
//        VanillaOption europeanOption(payoff, europeanExercise);
//        VanillaOption bermudanOption(payoff, bermudanExercise);
//        VanillaOption americanOption(payoff, americanExercise);




//        // Analytic formulas:

//        // Black-Scholes for European
//        method = "Black-Scholes";
//        europeanOption.setPricingEngine(boost::shared_ptr<PricingEngine>(
//                                     new AnalyticEuropeanEngine(bsmProcess)));
//        std::cout << std::setw(widths[0]) << std::left << method
//                  << std::fixed
//                  << std::setw(widths[1]) << std::left << europeanOption.NPV()
//                  << std::setw(widths[2]) << std::left << "N/A"
//                  << std::setw(widths[3]) << std::left << "N/A"
//                  << std::endl;






//        // Finite differences
//        Size timeSteps = 801;
//        method = "FD (Crank Nicolson)";
//        europeanOption.setPricingEngine(boost::shared_ptr<PricingEngine>(
//                 new FDEuropeanEngine<CrankNicolson>(bsmProcess,
//                                                     timeSteps,timeSteps-1)));
//        bermudanOption.setPricingEngine(boost::shared_ptr<PricingEngine>(
//                 new FDBermudanEngine<CrankNicolson>(bsmProcess,
//                                                     timeSteps,timeSteps-1)));
//        americanOption.setPricingEngine(boost::shared_ptr<PricingEngine>(
//                 new FDAmericanEngine<CrankNicolson>(bsmProcess,
//                                                     timeSteps,timeSteps-1)));
//        std::cout << std::setw(widths[0]) << std::left << method
//                  << std::fixed
//                  << std::setw(widths[1]) << std::left << europeanOption.NPV()
//                  << std::setw(widths[2]) << std::left << bermudanOption.NPV()
//                  << std::setw(widths[3]) << std::left << americanOption.NPV()
//                  << std::endl;

//        method = "FD (Explcit Euler)";
//        europeanOption.setPricingEngine(boost::shared_ptr<PricingEngine>(
//                 new FDEuropeanEngine<ExplicitEuler>(bsmProcess,
//                                                     timeSteps,timeSteps-1)));
//        bermudanOption.setPricingEngine(boost::shared_ptr<PricingEngine>(
//                 new FDBermudanEngine<ExplicitEuler>(bsmProcess,
//                                                     timeSteps,timeSteps-1)));
//        americanOption.setPricingEngine(boost::shared_ptr<PricingEngine>(
//                 new FDAmericanEngine<ExplicitEuler>(bsmProcess,
//                                                     timeSteps,timeSteps-1)));
//        std::cout << std::setw(widths[0]) << std::left << method
//                  << std::fixed
//                  << std::setw(widths[1]) << std::left << europeanOption.NPV()
//                  << std::setw(widths[2]) << std::left << bermudanOption.NPV()
//                  << std::setw(widths[3]) << std::left << americanOption.NPV()
//                  << std::endl;

//        method = "FD (Implicit Euler)";
//        europeanOption.setPricingEngine(boost::shared_ptr<PricingEngine>(
//                 new FDEuropeanEngine<ImplicitEuler>(bsmProcess,
//                                                     timeSteps,timeSteps-1)));
//        bermudanOption.setPricingEngine(boost::shared_ptr<PricingEngine>(
//                 new FDBermudanEngine<ImplicitEuler>(bsmProcess,
//                                                     timeSteps,timeSteps-1)));
//        americanOption.setPricingEngine(boost::shared_ptr<PricingEngine>(
//                 new FDAmericanEngine<ImplicitEuler>(bsmProcess,
//                                                     timeSteps,timeSteps-1)));
//        std::cout << std::setw(widths[0]) << std::left << method
//                  << std::fixed
//                  << std::setw(widths[1]) << std::left << europeanOption.NPV()
//                  << std::setw(widths[2]) << std::left << bermudanOption.NPV()
//                  << std::setw(widths[3]) << std::left << americanOption.NPV()
//                  << std::endl;




//        //---------------------------------------------------------------------------

//        // End test
//        Real seconds = timer.elapsed();
//        Integer hours = int(seconds/3600);
//        seconds -= hours * 3600;
//        Integer minutes = int(seconds/60);
//        seconds -= minutes * 60;
//        std::cout << " \nRun completed in ";
//        if (hours > 0)
//            std::cout << hours << " h ";
//        if (hours > 0 || minutes > 0)
//            std::cout << minutes << " m ";
//        std::cout << std::fixed << std::setprecision(0)
//                  << seconds << " s\n" << std::endl;
//        return 0;

//    } catch (std::exception& e) {
//        std::cerr << e.what() << std::endl;
//        return 1;
//    } catch (...) {
//        std::cerr << "unknown error" << std::endl;
//        return 1;
//    }
//}

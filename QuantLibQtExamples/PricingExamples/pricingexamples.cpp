/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

#include "cmstest.hpp"
#include "fdhestontest.hpp"
#include "../customutilities.hpp"
#include <ql/quantlib.hpp>
#include <map>
#include <iostream>

using namespace QuantLib;

//#define CMS_TEST
#define FDHESTON_TEST

namespace CMS {
    void testCMS();
}

namespace NSHestonModel {
    void testFdmHestonAmerican();
}

namespace NSMonteCarloPricing {
    void testMcEquityOption();
}

int main()
{
    try {

        //JumpDiffusionTest::testMerton76();
        //JumpDiffusionTest::testGreeks();

        //NSHestonModel::testFdmHestonAmerican();
        //NSMonteCarloPricing::testMcEquityOption();

#if defined(CMS_TEST)
        CmsTest::testFairRate();
        CmsTest::testCmsSwap();
        CmsTest::testParity();
#endif

#if defined(FDHESTON_TEST)
#include "fdhestontest.hpp"
        FdHestonTest::testFdmHestonBarrier();
        FdHestonTest::testFdmHestonBarrierVsBlackScholes();
        FdHestonTest::testFdmHestonAmerican();
        FdHestonTest::testFdmHestonIkonenToivanen();
        FdHestonTest::testFdmHestonBlackScholes();
        FdHestonTest::testFdmHestonEuropeanWithDividends();
        FdHestonTest::testFdmHestonConvergence();

        FdHestonTest::testBlackScholesFokkerPlanckFwdEquation();
            #if BOOST_VERSION >= 103900
        FdHestonTest::testSquareRootZeroFlowBC();
            #endif
        FdHestonTest::testTransformedZeroFlowBC();
        FdHestonTest::testSquareRootEvolveWithStationaryDensity();
            #if BOOST_VERSION >= 103900
        FdHestonTest::testSquareRootFokkerPlanckFwdEquation();
            #endif
        FdHestonTest::testHestonFokkerPlanckFwdEquation();
#endif


    } catch (std::exception& e) {
        std::cerr << e.what() << std::endl;
        return 1;
    } catch (...) {
        std::cerr << "unknown error" << std::endl;
        return 1;
    }
}

void CMS::testCMS() {
    //constantMaturityFromDiscountRatios();
    //Size spanningForwards;
    //std::vector<Time>& rateTimes;
    //CMSwapCurveState cms = CMSwapCurveState(rateTimes, spanningForwards);
}

void NSMonteCarloPricing::testMcEquityOption() {

    LARGE_TITLE("Monte Carlo Equity Option");

    // set up dates
    Calendar calendar = TARGET();
    Date todaysDate(15, May, 1998);
    Date settlementDate(17, May, 1998);
    Settings::instance().evaluationDate() = todaysDate;

    // our options
    Option::Type type(Option::Put);
    Real underlying = 36;
    Real strike = 40;
    Spread dividendYield = 0.00;
    Rate riskFreeRate = 0.06;
    Volatility volatility = 0.20;
    Date maturity(17, May, 1999);
    DayCounter dayCounter = Actual365Fixed();

    std::cout << "Option type = "  << type << std::endl;
    std::cout << "Maturity = "        << maturity << std::endl;
    std::cout << "Underlying price = "        << underlying << std::endl;
    std::cout << "Strike = "                  << strike << std::endl;
    std::cout << "Risk-free interest rate = " << io::rate(riskFreeRate)
              << std::endl;
    std::cout << "Dividend yield = " << io::rate(dividendYield)
              << std::endl;
    std::cout << "Volatility = " << io::volatility(volatility)
              << std::endl;
    std::cout << std::endl;
    std::string method;
    std::cout << std::endl ;

    // write column headings
    Size widths[] = { 35, 14, 14, 14 };
    std::cout << std::setw(widths[0]) << std::left << "Method"
              << std::setw(widths[1]) << std::left << "European"
              << std::setw(widths[2]) << std::left << "Bermudan"
              << std::setw(widths[3]) << std::left << "American"
              << std::endl;

    std::vector<Date> exerciseDates;
    for (Integer i=1; i<=4; i++)
        exerciseDates.push_back(settlementDate + 3*i*Months);

    boost::shared_ptr<Exercise> europeanExercise(
                                     new EuropeanExercise(maturity));

    boost::shared_ptr<Exercise> bermudanExercise(
                                     new BermudanExercise(exerciseDates));

    boost::shared_ptr<Exercise> americanExercise(
                                     new AmericanExercise(settlementDate,
                                                          maturity));

    Handle<Quote> underlyingH(
        boost::shared_ptr<Quote>(new SimpleQuote(underlying)));

    // bootstrap the yield/dividend/vol curves
    Handle<YieldTermStructure> flatTermStructure(
        boost::shared_ptr<YieldTermStructure>(
            new FlatForward(settlementDate, riskFreeRate, dayCounter)));
    Handle<YieldTermStructure> flatDividendTS(
        boost::shared_ptr<YieldTermStructure>(
            new FlatForward(settlementDate, dividendYield, dayCounter)));
    Handle<BlackVolTermStructure> flatVolTS(
        boost::shared_ptr<BlackVolTermStructure>(
            new BlackConstantVol(settlementDate, calendar, volatility,
                                 dayCounter)));
    boost::shared_ptr<StrikedTypePayoff> payoff(
                                    new PlainVanillaPayoff(type, strike));
    boost::shared_ptr<BlackScholesMertonProcess> bsmProcess(
             new BlackScholesMertonProcess(underlyingH, flatDividendTS,
                                           flatTermStructure, flatVolTS));

    // options
    VanillaOption europeanOption(payoff, europeanExercise);
    VanillaOption bermudanOption(payoff, bermudanExercise);
    VanillaOption americanOption(payoff, americanExercise);

    // Analytic formulas:

    // Black-Scholes for European
    method = "Black-Scholes";
    europeanOption.setPricingEngine(boost::shared_ptr<PricingEngine>(
                                 new AnalyticEuropeanEngine(bsmProcess)));
    std::cout << std::setw(widths[0]) << std::left << method
              << std::fixed
              << std::setw(widths[1]) << std::left << europeanOption.NPV()
              << std::setw(widths[2]) << std::left << "N/A"
              << std::setw(widths[3]) << std::left << "N/A"
              << std::endl;

    // Monte Carlo Method: MC (crude)
    Size timeSteps = 1;
    method = "MC (crude)";
    Size mcSeed = 42;
    boost::shared_ptr<PricingEngine> mcengine1;
    mcengine1 = MakeMCEuropeanEngine<PseudoRandom>(bsmProcess)
        .withSteps(timeSteps)
        .withAbsoluteTolerance(0.02)
        .withSeed(mcSeed);
    europeanOption.setPricingEngine(mcengine1);
    // Real errorEstimate = europeanOption.errorEstimate();
    std::cout << std::setw(widths[0]) << std::left << method
              << std::fixed
              << std::setw(widths[1]) << std::left << europeanOption.NPV()
              << std::setw(widths[2]) << std::left << "N/A"
              << std::setw(widths[3]) << std::left << "N/A"
              << std::endl;

    // Monte Carlo Method: QMC (Sobol)
    method = "QMC (Sobol)";
    Size nSamples = 32768;  // 2^15

    boost::shared_ptr<PricingEngine> mcengine2;
    mcengine2 = MakeMCEuropeanEngine<LowDiscrepancy>(bsmProcess)
        .withSteps(timeSteps)
        .withSamples(nSamples);
    europeanOption.setPricingEngine(mcengine2);
    std::cout << std::setw(widths[0]) << std::left << method
              << std::fixed
              << std::setw(widths[1]) << std::left << europeanOption.NPV()
              << std::setw(widths[2]) << std::left << "N/A"
              << std::setw(widths[3]) << std::left << "N/A"
              << std::endl;

    // Monte Carlo Method: MC (Longstaff Schwartz)
    method = "MC (Longstaff Schwartz)";
    boost::shared_ptr<PricingEngine> mcengine3;
    mcengine3 = MakeMCAmericanEngine<PseudoRandom>(bsmProcess)
        .withSteps(100)
        .withAntitheticVariate()
        .withCalibrationSamples(4096)
        .withAbsoluteTolerance(0.02)
        .withSeed(mcSeed);
    americanOption.setPricingEngine(mcengine3);
    std::cout << std::setw(widths[0]) << std::left << method
              << std::fixed
              << std::setw(widths[1]) << std::left << "N/A"
              << std::setw(widths[2]) << std::left << "N/A"
              << std::setw(widths[3]) << std::left << americanOption.NPV()
              << std::endl;
}

void NSHestonModel::testFdmHestonAmerican() {

    LARGE_TITLE("FD Heston");

    SavedSettings backup;

    Handle<Quote> s0(boost::shared_ptr<Quote>(new SimpleQuote(100.0)));
    Handle<YieldTermStructure> rTS(flatRate(0.05, Actual365Fixed()));
    Handle<YieldTermStructure> qTS(flatRate(0.00, Actual365Fixed()));

    Real v0 = 0.04;
    Real kappa = 2.5;
    Real theta = 0.04;
    Real sigma = 0.66;
    Real rho = -0.8;
    boost::shared_ptr<HestonProcess> hestonProcess(
                new HestonProcess(rTS, qTS, s0, v0, kappa, theta, sigma, rho));
    /*
     * dS(t,S) = mu*Sdt + sqrt(v)*SdW1
     * dv(t,S) = kappa(theta-v)dt + sigma*sqrt(v)dW2
     * dW1*dW2 = rho*dt
     * dS/S = mu*dt + sqrt(v)*dW1
     * dv   = 2.5(0.04-v)dt + 0.66*dW2
     * dW1*dW2/dt = -0.8
     **/
    Settings::instance().evaluationDate() = Date(28, March, 2004);
    Date exerciseDate(28, March, 2005);
    boost::shared_ptr<Exercise> exercise(new AmericanExercise(exerciseDate));
    boost::shared_ptr<StrikedTypePayoff> payoff(new PlainVanillaPayoff(Option::Put, 100));
    VanillaOption option(payoff, exercise);
    boost::shared_ptr<PricingEngine> engine(
                new FdHestonVanillaEngine(boost::shared_ptr<HestonModel>(
                                              new HestonModel(hestonProcess)),200, 100, 50));
    option.setPricingEngine(engine);

    std::cout << "NPV: \t" << option.NPV() << std::endl; //npvExpected=5.66032
    std::cout << "delta: \t" << option.delta() << std::endl; //deltaExpected=-0.30065
    std::cout << "gammga: \t" << option.gamma() << std::endl; //gammaExpected=0.02202

}

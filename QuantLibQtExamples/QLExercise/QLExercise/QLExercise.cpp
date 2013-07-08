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


// Detect memory leak in debug mode
// http://www.microsoft.com/japan/msdn/vs_previous/visualc/techmat/feature/MemLeaks/
#define _CRTDBG_MAP_ALLOC
#include <stdlib.h>
#include <crtdbg.h>

#include <boost/timer.hpp>
#include <boost/progress.hpp>
//#include <boost/timer/timer.hpp>
#include <iostream>
#include <iomanip>
//#include <windows.h>

// the only header you need to use QuantLib
#include <ql/quantlib.hpp>

#include "customutilities.hpp"
//#include <ql/termstructures/volatility/equityfx/localvolsurface.hpp>
#include "libormarketmodel.hpp"
#include "libormarketmodelprocess.hpp"
//#include "flatratevolhelperfnc.hpp"
//#include "cdo.hpp"
//#include "hestonmodeltest.hpp"
#include "inflationcapfloortest.hpp"
#include "inflationvolatilitytest.hpp"
//#include "margrabeoptiontest.hpp"
#include "inflationtest.hpp"
#include "inflationcpiswaptest.hpp"
#include "yearonyearinflationindexedswaptest.hpp"
#include "varianceswapstest.hpp"
#include "optimization.hpp"
#include "ffttest.hpp"
#include "variancegammatest.hpp"
#include "eurodollarfuturestest.hpp"
#include "volatilitycubetest.hpp"
#include "swaptionvolatilitycubetest.hpp"

#ifdef BOOST_MSVC
/* Uncomment the following lines to unmask floating-point
   exceptions. Warning: unpredictable results can arise...

   See http://www.wilmott.com/messageview.cfm?catid=10&threadid=9481
   Is there anyone with a definitive word about this?
*/
// #include <float.h>
// namespace { unsigned int u = _controlfp(_EM_INEXACT, _MCW_EM); }
#endif

using namespace QuantLib;

//#define QL_ENABLE_SESSIONS

#if defined(QL_ENABLE_SESSIONS)
namespace QuantLib {
    Integer sessionId() { return 0; }
}
#endif

//class my_numpunct: public std::numpunct<char> {
//	std::string do_grouping() const { return "\3"; }
//};


//TODO: currency swap
//TODO: inflation derivatives
//TODO: CMS, OIS
//TODO: swaption, variance swap
//TODO: commodity swap

int main(int, char* []) {

    boost::timer timer;

    #pragma region PrecisionSettings
    //std::locale nl(std::locale(), new my_numpunct);
    //std::cout.imbue(nl); // uses thousands' separators
    //std::cout << 1000000000 << "\n";

    //std::cout.imbue(std::locale("")); // uses thousand separators
    //std::cout << 1000000000 << "\n";
    //std::cout.imbue(std::locale()); // does not use thousands' separators
    //std::cout << 1000000000 << "\n";

    // std::fixed=_  std::ios::scientific=ȊwZpvZ\  std::ios::floatfield=\p
    // std::cout << std::fixed << "b1 = " << b1 << " b2 = " << b2 << " b3 = " << b3 << std::endl;
    // std::cout.setf(std::ios::scientific, std::ios::floatfield);
    // std::ios::fixed=Œ菬_\Astd::ios::floatfield=\p
    //std::ios_base::fmtflags original_flags = std::cout.flags(); // Store the current format flag setting, in order to restore it later on.
    std::cout.setf(std::ios::fixed, std::ios::floatfield);
    //std::cout.precision(10);
    //std::cout.width(10);
    //std::cout.imbue(std::locale("")); // uses thousand separators
    //std::cout.imbue(std::locale()); // not use thousand separators
    //std::cout << 109347501923709128 << std::endl;
    //std::cout.unsetf(std::ios::floatfield); //	Clear the adjustment flags.
    //std::cout.flags(original_flags); // Restore the original flags.

    // write column headings
    /*
    Size widths[] = { 35, 14, 14, 14 };
    std::cout << std::setw(widths[0]) << std::left << "Method"
        << std::setw(widths[1]) << std::left << "European"
        << std::setw(widths[2]) << std::left << "Bermudan"
        << std::setw(widths[3]) << std::left << "American"
        << std::endl;
    std::cout << std::setw(widths[0]) << std::left << method
        << std::fixed
        << std::setw(widths[1]) << std::left << europeanOption.NPV()
        << std::setw(widths[2]) << std::left << "N/A"
        << std::setw(widths[3]) << std::left << "N/A"
        << std::endl;
    */
    #pragma endregion PrecisionSettings

    try {

        //  [7/16/2012 AlgeFrontal] //-

        //  [7/17/2012 AlgeFrontal]
        LARGE_TITLE("Swaption Volatility Cube Test");
        SwaptionVolatilityCubeTest::testAtmVols();
        SwaptionVolatilityCubeTest::testSmile();
        SwaptionVolatilityCubeTest::testSabrVols();
        SwaptionVolatilityCubeTest::testSpreadedCube();
        SwaptionVolatilityCubeTest::testObservability();

        //  [7/29/2012 AlgeFrontal]
        LARGE_TITLE("Interest Rate Volatility Cube");
        VolatilityCubeTest::testVolatilityCube();



        LARGE_TITLE("Libor Market Model");
        LiborMarketModelTest::testSimpleCovarianceModels();
        LiborMarketModelTest::testCapletPricing();
        LiborMarketModelTest::testCalibration();
        LiborMarketModelTest::testSwaptionPricing();
        LiborMarketModelTest::mylmmtest();
        LiborMarketModelTest::testIshiyamaLMM();
        LiborMarketModelProcessTest::testInitialisation();
        LiborMarketModelProcessTest::testLambdaBootstrapping();
        LiborMarketModelProcessTest::testMonteCarloCapletPricing();

        /*

        LARGE_TITLE("Variance Swap");
        VarianceSwapTest::testReplicatingVarianceSwap();
        VarianceSwapTest::testMCVarianceSwap();

        LARGE_TITLE("Eurodollar Futures");
        EurodollarFuturesTest::testEurodollarFutures();

        LARGE_TITLE("FFT");
        FastFourierTransformTest::testFFT();
        FastFourierTransformTest::testSimple();
        FastFourierTransformTest::testInverse();


        LARGE_TITLE("Variance Gamma");
        VarianceGammaTest::testVarianceGamma();

        LARGE_TITLE("Optimization Test");
        OptimizationTest::optimization();
        OptimizationTest::LM();
        //OptimizationTest::RiskParity();


        LARGE_TITLE("CPI Swap (ZCIIS)");
        CPISwapTest::consistency();
        CPISwapTest::zciisconsistency();
        CPISwapTest::cpibondconsistency();
        CPISwapTest::myinflationcpiswap();


        LARGE_TITLE("Inflation Cap/Floor");
        InflationCapFloorTest::testConsistency();
        InflationCapFloorTest::testParity();
        InflationCapFloorTest::testCachedValue();


        LARGE_TITLE("Inflation Volatility");
        InflationVolTest::testYoYPriceSurfaceToVol();
        InflationVolTest::testYoYPriceSurfaceToATM();


        LARGE_TITLE("Inflation Test");
        InflationTest::testPeriod();
        InflationTest::testZeroIndex();
        InflationTest::testZeroTermStructure();
        InflationTest::testYYIndex();
        InflationTest::testYYTermStructure();
        InflationTest::myInflationTest();


        LARGE_TITLE("Year-on-Year Inflation Indexed Swap (YYIIS)");
        YYIISTest::testYYIIS();


        LARGE_TITLE("Libor Market Model");
        LiborMarketModelTest::testSimpleCovarianceModels();
        LiborMarketModelTest::testCapletPricing();
        LiborMarketModelTest::testCalibration();
        LiborMarketModelTest::testSwaptionPricing();
        LiborMarketModelProcessTest::testInitialisation();
        LiborMarketModelProcessTest::testLambdaBootstrapping();
        LiborMarketModelProcessTest::testMonteCarloCapletPricing();


        LARGE_TITLE("Margrabe Option");
        MargrabeOptionTest::testEuroExchangeTwoAssets();
        MargrabeOptionTest::testAmericanExchangeTwoAssets();
        MargrabeOptionTest::testGreeks();
        MargrabeOptionTest::simpleMargrabeOption();


        LARGE_TITLE("Heston Model");
        HestonModelTest::testBlackCalibration();
        HestonModelTest::testDAXCalibration();
        HestonModelTest::testAnalyticVsBlack();
        HestonModelTest::testAnalyticVsCached();
        HestonModelTest::testKahlJaeckelCase();
        HestonModelTest::testMcVsCached();
        HestonModelTest::testFdBarrierVsCached();
        HestonModelTest::testFdVanillaVsCached();
        HestonModelTest::testDifferentIntegrals();
        HestonModelTest::testMultipleStrikesEngine();
        HestonModelTest::testAnalyticPiecewiseTimeDependent();
        HestonModelTest::testDAXCalibrationOfTimeDependentModel();

        */


        //for (Size i=5; i < 10; ++i)
        //	InverseFloater(i/100.0);

        //Poison distribution
        //double lambda=0.1;
        /*
        BigInteger seed =12324;
        MersenneTwisterUniformRng unifMt(seed);
        // u is in (0,1)
        Real u = unifMt.next().value;
        std::cout << "Mersanne Twister Uniform Random Number u: " << u << std::endl;
        for(int cnt=0; cnt<10; cnt++){
            std::cout << cnt << ": " << unifMt.next().value << std::endl;
        }
        */
        //InverseCumulativePoisson f(lambda);
        //InverseCumulativeRng<MersenneTwisterUniformRng, InverseCumulativePoisson> rng(unifMt, f);




        //DebugBreak();


        //std::cout << "sizeof(unsigned long)=" << sizeof(unsigned long) << std::endl;
        //std::cout << 0.5/(1UL<<(32-1)) << std::endl;

        //std::cout << qltest( << std::endl;



        //std::cout << "=======================================================" << std::endl;

        // CDO
        //std::cout << "\n[8] "; CdoTest::testHW();

        //////////////////////////////////////////////////////////////////////////
    //! Local volatility surface derived from a Black vol surface
    /*! For details about this implementation refer to
        "Stochastic Volatility and Local Volatility," in
        "Case Studies and Financial Modeling Course Notes," by Jim Gatheral, Fall Term, 2003
        Reference:
        1. http://www.math.ku.dk/~rolf/teaching/ctff03/Gatheral.1.pdf
        2. Carlos Alexander III.4.3.2 (p.245) Dupire's Equation
    */
        // Volatility Surface
        /*

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

        std::vector<Date> exerciseDates;
        for (Integer i=1; i<=4; i++)
            exerciseDates.push_back(settlementDate + 3*i*Months);

        //std::cout << exerciseDates << std::endl;

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

        boost::shared_ptr<LocalVolSurface> lvolsurf(
            new LocalVolSurface(flatVolTS,
                                flatTermStructure,
                                flatDividendTS,
                                underlyingH));

        std::cout << "Reference Date: " << lvolsurf->referenceDate() << std::endl;
        std::cout << "Day Counter: " << lvolsurf->dayCounter() << std::endl;
        //std::cout << "Local Volatility: " << lvolsurf->localVolImpl(3,40 << std::endl;
        //Volatility localvolImpl(3,40);

        */

#pragma region ProgressBar
        /*
        boost::progress_display show_progress(1000000);
        for (long i = 0; i < 1000000; ++i)
        {
            std::sqrt(123.456L); // burn some time
            ++show_progress;
        }
        std::cout << std::endl << std::endl;
        */
#pragma endregion ProgressBar


        //////////////////////////////////////////////////////////////////////////
        /// variance swap
        /*
        #include <ql/instruments/varianceswap.hpp>
        #include <ql/termstructures/voltermstructure.hpp>
        Real varswpStrike = 0.5;
        Real varswpNotional = 1000000;
        Date varswpStart(1, May, 2012);
        Date varswpMaturity = calendar.advance(varswpStart,6,Months,ModifiedFollowing);

        boost::shared_ptr<VarianceSwap> varswp(new VarianceSwap(
                                Position::Type::Long,
                                varswpStrike,
                                varswpNotional,
                                varswpStart,
                                varswpMaturity));

        Calendar calendar = TARGET();
        Natural settlementDays = 2;

        boost::shared_ptr<VolatilityTermStructure> volTermStructure(
            new VolatilityTermStructure(
                settlementDays,
                calendar,
                BusinessDayConvention::ModifiedFollowing,
                Actual365Fixed()));

        varswp->setupArguments(boost::shared_ptr<PricingEngine>(
            new DiscountingSwapEngine(volTermStructure)));
        */

        //std::cout << "Variance: " << varswp->variance() << std::endl;

        //////////////////////////////////////////////////////////////////////////
        //////////////////////////////////////////////////////////////////////////
        ///// Finite Difference
        //std::cout << "<<<Finite Difference Method>>>" << std::endl;
        //// 1.set up dates
        //Calendar calendar = TARGET();
        //Date todaysDate(15, May, 1998);
        //Date settlementDate(17, May, 1998);
        //Settings::instance().evaluationDate() = todaysDate;

        //// 2.our options
        //Option::Type type(Option::Put);
        //Real underlying = 36;
        //Real strike = 40;
        //Spread dividendYield = 0.00;
        //Rate riskFreeRate = 0.06;
        //Volatility volatility = 0.20;
        //Date maturity(17, May, 1999);
        //DayCounter dayCounter = Actual365Fixed();

        //std::cout << "Option type = "  << type << std::endl;
        //std::cout << "Maturity = "        << maturity << std::endl;
        //std::cout << "Underlying price = "        << underlying << std::endl;
        //std::cout << "Strike = "                  << strike << std::endl;
        //std::cout << "Risk-free interest rate = " << io::rate(riskFreeRate)
        //	<< std::endl;
        //std::cout << "Dividend yield = " << io::rate(dividendYield)
        //	<< std::endl;
        //std::cout << "Volatility = " << io::volatility(volatility)
        //	<< std::endl;
        //std::cout << std::endl;
        //std::string method;
        //std::cout << std::endl ;

        //// 3.write column headings
        //Size widths[] = { 35, 14, 14, 14 };
        //std::cout << std::setw(widths[0]) << std::left << "Method"
        //	<< std::setw(widths[1]) << std::left << "European"
        //	<< std::setw(widths[2]) << std::left << "Bermudan"
        //	<< std::setw(widths[3]) << std::left << "American"
        //	<< std::endl;

        //std::vector<Date> exerciseDates;
        //for (Integer i=1; i<=4; i++)
        //	exerciseDates.push_back(settlementDate + 3*i*Months);

        //boost::shared_ptr<Exercise> europeanExercise(
        //	new EuropeanExercise(maturity));

        //boost::shared_ptr<Exercise> bermudanExercise(
        //	new BermudanExercise(exerciseDates));

        //boost::shared_ptr<Exercise> americanExercise(
        //	new AmericanExercise(settlementDate,
        //	maturity));

        //Handle<Quote> underlyingH(
        //	boost::shared_ptr<Quote>(new SimpleQuote(underlying)));

        //// 4.bootstrap the yield/dividend/vol curves
        //Handle<YieldTermStructure> flatTermStructure(
        //	boost::shared_ptr<YieldTermStructure>(
        //	new FlatForward(settlementDate, riskFreeRate, dayCounter)));
        //Handle<YieldTermStructure> flatDividendTS(
        //	boost::shared_ptr<YieldTermStructure>(
        //	new FlatForward(settlementDate, dividendYield, dayCounter)));
        //Handle<BlackVolTermStructure> flatVolTS(
        //	boost::shared_ptr<BlackVolTermStructure>(
        //	new BlackConstantVol(settlementDate, calendar, volatility,
        //	dayCounter)));
        //boost::shared_ptr<StrikedTypePayoff> payoff(
        //	new PlainVanillaPayoff(type, strike));
        //boost::shared_ptr<BlackScholesMertonProcess> bsmProcess(
        //	new BlackScholesMertonProcess(underlyingH, flatDividendTS,
        //	flatTermStructure, flatVolTS));

        //// 5.options
        //VanillaOption europeanOption(payoff, europeanExercise);
        //VanillaOption bermudanOption(payoff, bermudanExercise);
        //VanillaOption americanOption(payoff, americanExercise);

        //// 6.Finite differences pricing
        //Size timeSteps = 801;
        //method = "Finite differences";
        //europeanOption.setPricingEngine(boost::shared_ptr<PricingEngine>(
        //	new FDEuropeanEngine<CrankNicolson>(bsmProcess,
        //	timeSteps,timeSteps-1)));
        //bermudanOption.setPricingEngine(boost::shared_ptr<PricingEngine>(
        //	new FDBermudanEngine<CrankNicolson>(bsmProcess,
        //	timeSteps,timeSteps-1)));
        ///*americanOption.setPricingEngine(boost::shared_ptr<PricingEngine>(
        //	new FDAmericanEngine<CrankNicolson>(bsmProcess,
        //	timeSteps,timeSteps-1)));*/
        //americanOption.setPricingEngine(boost::shared_ptr<PricingEngine>(
        //	new FDAmericanEngine<CrankNicolson>(bsmProcess,
        //	timeSteps,timeSteps-1)));
        //std::cout << std::setw(widths[0]) << std::left << method
        //	<< std::fixed
        //	<< std::setw(widths[1]) << std::left << europeanOption.NPV()
        //	<< std::setw(widths[2]) << std::left << bermudanOption.NPV()
        //	<< std::setw(widths[3]) << std::left << americanOption.NPV()
        //	<< std::endl;

        //// Binomial method: Jarrow-Rudd
        //method = "Binomial Jarrow-Rudd";
        //europeanOption.setPricingEngine(boost::shared_ptr<PricingEngine>(
        //	new BinomialVanillaEngine<JarrowRudd>(bsmProcess,timeSteps)));
        //bermudanOption.setPricingEngine(boost::shared_ptr<PricingEngine>(
        //	new BinomialVanillaEngine<JarrowRudd>(bsmProcess,timeSteps)));
        //americanOption.setPricingEngine(boost::shared_ptr<PricingEngine>(
        //	new BinomialVanillaEngine<JarrowRudd>(bsmProcess,timeSteps)));
        //std::cout << std::setw(widths[0]) << std::left << method
        //	<< std::fixed
        //	<< std::setw(widths[1]) << std::left << europeanOption.NPV()
        //	<< std::setw(widths[2]) << std::left << bermudanOption.NPV()
        //	<< std::setw(widths[3]) << std::left << americanOption.NPV()
        //	<< std::endl;
        //method = "Binomial Cox-Ross-Rubinstein";
        //europeanOption.setPricingEngine(boost::shared_ptr<PricingEngine>(
        //	new BinomialVanillaEngine<CoxRossRubinstein>(bsmProcess,
        //	timeSteps)));
        //bermudanOption.setPricingEngine(boost::shared_ptr<PricingEngine>(
        //	new BinomialVanillaEngine<CoxRossRubinstein>(bsmProcess,
        //	timeSteps)));
        //americanOption.setPricingEngine(boost::shared_ptr<PricingEngine>(
        //	new BinomialVanillaEngine<CoxRossRubinstein>(bsmProcess,
        //	timeSteps)));
        //std::cout << std::setw(widths[0]) << std::left << method
        //	<< std::fixed
        //	<< std::setw(widths[1]) << std::left << europeanOption.NPV()
        //	<< std::setw(widths[2]) << std::left << bermudanOption.NPV()
        //	<< std::setw(widths[3]) << std::left << americanOption.NPV()
        //	<< std::endl;

        //// Binomial method: Binomial Leisen-Reimer
        //method = "Binomial Leisen-Reimer";
        //europeanOption.setPricingEngine(boost::shared_ptr<PricingEngine>(
        //	new BinomialVanillaEngine<LeisenReimer>(bsmProcess,timeSteps)));
        //bermudanOption.setPricingEngine(boost::shared_ptr<PricingEngine>(
        //	new BinomialVanillaEngine<LeisenReimer>(bsmProcess,timeSteps)));
        //americanOption.setPricingEngine(boost::shared_ptr<PricingEngine>(
        //	new BinomialVanillaEngine<LeisenReimer>(bsmProcess,timeSteps)));
        //std::cout << std::setw(widths[0]) << std::left << method
        //	<< std::fixed
        //	<< std::setw(widths[1]) << std::left << europeanOption.NPV()
        //	<< std::setw(widths[2]) << std::left << bermudanOption.NPV()
        //	<< std::setw(widths[3]) << std::left << americanOption.NPV()
        //	<< std::endl;



        //////////////////////////////////////////////////////////////////////////

        std::cout << "\nTime Elapsed: " << timer.elapsed() << " seconds" << std::endl;

        // dump memory leak information
        _CrtDumpMemoryLeaks();

        char tmp;
        std::cin >> tmp;
        return 0;
    } catch (std::exception& e) {
        std::cerr << e.what() << std::endl;
        return 1;
    } catch (...) {
        std::cerr << "unknown error" << std::endl;
        return 1;
    }
}

/*
double qltest()
{
    boost::timer timer;

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

    std::vector<Date> exerciseDates;
    for (Integer i=1; i<=4; i++)
        exerciseDates.push_back(settlementDate + 3*i*Months);

    //std::cout << exerciseDates << std::endl;

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
    std::string method = "Black-Scholes";
    europeanOption.setPricingEngine(boost::shared_ptr<PricingEngine>(
        new AnalyticEuropeanEngine(bsmProcess)));
    double returnValue = europeanOption.NPV();

    return returnValue;
}


std::vector<std::vector<Matrix> > theVegaBumps(bool factorwiseBumping,
    boost::shared_ptr<MarketModel> marketModel,
    bool doCaps)
{
    Real multiplierCutOff = 50.0;
    Real projectionTolerance = 1E-4;
    Size numberRates= marketModel->numberOfRates();

    std::vector<VolatilityBumpInstrumentJacobian::Cap> caps;

    if (doCaps)
    {

        Rate capStrike = marketModel->initialRates()[0];

        for (Size i=0; i< numberRates-1; i=i+1)
        {
            VolatilityBumpInstrumentJacobian::Cap nextCap;
            nextCap.startIndex_ = i;
            nextCap.endIndex_ = i+1;
            nextCap.strike_ = capStrike;
            caps.push_back(nextCap);
        }


    }



    std::vector<VolatilityBumpInstrumentJacobian::Swaption> swaptions(numberRates);

    for (Size i=0; i < numberRates; ++i)
    {
        swaptions[i].startIndex_ = i;
        swaptions[i].endIndex_ = numberRates;

    }

    VegaBumpCollection possibleBumps(marketModel,
        factorwiseBumping);

    OrthogonalizedBumpFinder  bumpFinder(possibleBumps,
        swaptions,
        caps,
        multiplierCutOff, // if vector length grows by more than this discard
        projectionTolerance);      // if vector projection before scaling less than this discard

    std::vector<std::vector<Matrix> > theBumps;

    bumpFinder.GetVegaBumps(theBumps);

    return theBumps;

}

int InverseFloater(Real rateLevel)
{

    Size numberRates =20;
    Real accrual = 0.5;
    Real firstTime = 0.5;

    Real strike =0.15;
    Real fixedMultiplier = 2.0;
    Real floatingSpread =0.0;
    bool payer = true;


    std::vector<Real> rateTimes(numberRates+1);
    for (Size i=0; i < rateTimes.size(); ++i)
        rateTimes[i] = firstTime + i*accrual;

    std::vector<Real> paymentTimes(numberRates);
    std::vector<Real> accruals(numberRates,accrual);
    std::vector<Real> fixedStrikes(numberRates,strike);
    std::vector<Real> floatingSpreads(numberRates,floatingSpread);
    std::vector<Real> fixedMultipliers(numberRates,fixedMultiplier);

    for (Size i=0; i < paymentTimes.size(); ++i)
        paymentTimes[i] = firstTime + (i+1)*accrual;

    MultiStepInverseFloater inverseFloater(
        rateTimes,
        accruals,
        accruals,
        fixedStrikes,
        fixedMultipliers,
        floatingSpreads,
        paymentTimes,
        payer);




    //exercise schedule, we can exercise on any rate time except the last one
    std::vector<Rate> exerciseTimes(rateTimes);
    exerciseTimes.pop_back();

    // naive exercise strategy, exercise above a trigger level
    Real trigger =0.05;
    std::vector<Rate> swapTriggers(exerciseTimes.size(), trigger);
    SwapRateTrigger naifStrategy(rateTimes, swapTriggers, exerciseTimes);

    // Longstaff-Schwartz exercise strategy
    std::vector<std::vector<NodeData> > collectedData;
    std::vector<std::vector<Real> > basisCoefficients;

    // control that does nothing, need it because some control is expected
    NothingExerciseValue control(rateTimes);

    SwapForwardBasisSystem basisSystem(rateTimes,exerciseTimes);
    //    SwapBasisSystem basisSystem(rateTimes,exerciseTimes);



    // rebate that does nothing, need it because some rebate is expected
    // when you break a swap nothing happens.
    NothingExerciseValue nullRebate(rateTimes);

    CallSpecifiedMultiProduct dummyProduct =
        CallSpecifiedMultiProduct(inverseFloater, naifStrategy,
        ExerciseAdapter(nullRebate));

    EvolutionDescription evolution = dummyProduct.evolution();


    // parameters for models


    Size seed = 12332; // for Sobol generator
    Size trainingPaths = 65536;
    Size paths = 65536;
    Size vegaPaths =16384;

#ifdef _DEBUG
    trainingPaths = 8192;
    paths = 8192;
    vegaPaths = 1024;
#endif


    std::cout <<  " inverse floater \n";
    std::cout << " fixed strikes :  "  << strike << "\n";
    std::cout << " number rates :  " << numberRates << "\n";

    std::cout << "training paths, " << trainingPaths << "\n";
    std::cout << "paths, " << paths << "\n";
    std::cout << "vega Paths, " << vegaPaths << "\n";


    // set up a calibration, this would typically be done by using a calibrator



    //Real rateLevel =0.08;

    std::cout << " rate level " <<  rateLevel << "\n";

    Real initialNumeraireValue = 0.95;

    Real volLevel = 0.11;
    Real beta = 0.2;
    Real gamma = 1.0;
    Size numberOfFactors = std::min<Size>(5,numberRates);

    Spread displacementLevel =0.02;

    // set up vectors
    std::vector<Rate> initialRates(numberRates,rateLevel);
    std::vector<Volatility> volatilities(numberRates, volLevel);
    std::vector<Spread> displacements(numberRates, displacementLevel);

    ExponentialForwardCorrelation correlations(
        rateTimes,volLevel, beta,gamma);




    FlatVol  calibration(
        volatilities,
        boost::shared_ptr<PiecewiseConstantCorrelation>(new  ExponentialForwardCorrelation(correlations)),
        evolution,
        numberOfFactors,
        initialRates,
        displacements);

    boost::shared_ptr<MarketModel> marketModel(new FlatVol(calibration));

    // we use a factory since there is data that will only be known later
    SobolBrownianGeneratorFactory generatorFactory(
        SobolBrownianGenerator::Diagonal, seed);

    std::vector<Size> numeraires( moneyMarketMeasure(evolution));

    // the evolver will actually evolve the rates
    LogNormalFwdRatePc  evolver(marketModel,
        generatorFactory,
        numeraires   // numeraires for each step
        );

    boost::shared_ptr<MarketModelEvolver> evolverPtr(new LogNormalFwdRatePc(evolver));

    int t1= clock();

    // gather data before computing exercise strategy
    collectNodeData(evolver,
        inverseFloater,
        basisSystem,
        nullRebate,
        control,
        trainingPaths,
        collectedData);

    int t2 = clock();


    // calculate the exercise strategy's coefficients
    genericLongstaffSchwartzRegression(collectedData,
        basisCoefficients);


    // turn the coefficients into an exercise strategy
    LongstaffSchwartzExerciseStrategy exerciseStrategy(
        basisSystem, basisCoefficients,
        evolution, numeraires,
        nullRebate, control);


    //  callable receiver swap
    CallSpecifiedMultiProduct callableProduct =
        CallSpecifiedMultiProduct(
        inverseFloater, exerciseStrategy,
        ExerciseAdapter(nullRebate));

    MultiProductComposite allProducts;
    allProducts.add(inverseFloater);
    allProducts.add(callableProduct);
    allProducts.finalize();


    AccountingEngine accounter(evolverPtr,
        Clone<MarketModelMultiProduct>(allProducts),
        initialNumeraireValue);

    SequenceStatisticsInc stats;

    accounter.multiplePathValues (stats,paths);

    int t3 = clock();

    std::vector<Real> means(stats.mean());

    for (Size i=0; i < means.size(); ++i)
        std::cout << means[i] << "\n";

    std::cout << " time to build strategy, " << (t2-t1)/static_cast<Real>(CLOCKS_PER_SEC)<< ", seconds.\n";
    std::cout << " time to price, " << (t3-t2)/static_cast<Real>(CLOCKS_PER_SEC)<< ", seconds.\n";

    // vegas

    // do it twice once with factorwise bumping, once without
    Size pathsToDoVegas = vegaPaths;

    for (Size i=0; i < 4; ++i)
    {

        bool allowFactorwiseBumping = i % 2 > 0 ;

        bool doCaps = i / 2 > 0 ;


        LogNormalFwdRateEuler evolverEuler(marketModel,
            generatorFactory,
            numeraires
            ) ;

        MarketModelPathwiseInverseFloater pathwiseInverseFloater(
            rateTimes,
            accruals,
            accruals,
            fixedStrikes,
            fixedMultipliers,
            floatingSpreads,
            paymentTimes,
            payer);

        Clone<MarketModelPathwiseMultiProduct> pathwiseInverseFloaterPtr(pathwiseInverseFloater.clone());

        //  callable inverse floater
        CallSpecifiedPathwiseMultiProduct callableProductPathwise(pathwiseInverseFloaterPtr,
            exerciseStrategy);

        Clone<MarketModelPathwiseMultiProduct> callableProductPathwisePtr(callableProductPathwise.clone());


        std::vector<std::vector<Matrix> > theBumps(theVegaBumps(allowFactorwiseBumping,
            marketModel,
            doCaps));

        PathwiseVegasOuterAccountingEngine
            accountingEngineVegas(boost::shared_ptr<LogNormalFwdRateEuler>(new LogNormalFwdRateEuler(evolverEuler)),
            //         pathwiseInverseFloaterPtr,
            callableProductPathwisePtr,
            marketModel,
            theBumps,
            initialNumeraireValue);

        std::vector<Real> values,errors;

        accountingEngineVegas.multiplePathValues(values,errors,pathsToDoVegas);


        std::cout << "vega output \n";
        std::cout << " factorwise bumping " << allowFactorwiseBumping << "\n";
        std::cout << " doCaps " << doCaps << "\n";



        Size r=0;

        std::cout << " price estimate, " << values[r++] << "\n";

        for (Size i=0; i < numberRates; ++i, ++r)
            std::cout << " Delta, " << i << ", " << values[r] << ", " << errors[r] << "\n";

        Real totalVega = 0.0;

        for (; r < values.size(); ++r)
        {
            std::cout << " vega, " << r - 1 -  numberRates<< ", " << values[r] << " ," << errors[r] << "\n";
            totalVega +=  values[r];
        }

        std::cout << " total Vega, " << totalVega << "\n";
    }

    bool doUpperBound = true;

    if (doUpperBound)
    {

        // upper bound

        MTBrownianGeneratorFactory uFactory(seed+142);


        boost::shared_ptr<MarketModelEvolver> upperEvolver(new LogNormalFwdRatePc( boost::shared_ptr<MarketModel>(new FlatVol(calibration)),
            uFactory,
            numeraires   // numeraires for each step
            ));

        std::vector<boost::shared_ptr<MarketModelEvolver> > innerEvolvers;

        std::valarray<bool> isExerciseTime =   isInSubset(evolution.evolutionTimes(),    exerciseStrategy.exerciseTimes());

        for (Size s=0; s < isExerciseTime.size(); ++s)
        {
            if (isExerciseTime[s])
            {
                MTBrownianGeneratorFactory iFactory(seed+s);
                boost::shared_ptr<MarketModelEvolver> e =boost::shared_ptr<MarketModelEvolver> (static_cast<MarketModelEvolver*>(new   LogNormalFwdRatePc(boost::shared_ptr<MarketModel>(new FlatVol(calibration)),
                    uFactory,
                    numeraires ,  // numeraires for each step
                    s)));

                innerEvolvers.push_back(e);
            }
        }

        //#region OneClick Update
        UpperBoundEngine uEngine(upperEvolver,  // does outer paths
            innerEvolvers, // for sub-simulations that do continuation values
            inverseFloater,
            nullRebate,
            inverseFloater,
            nullRebate,
            exerciseStrategy,
            initialNumeraireValue);

        Statistics uStats;
        Size innerPaths = 255;
        Size outerPaths =256;

        int t4 = clock();

        uEngine.multiplePathValues(uStats,outerPaths,innerPaths);
        Real upperBound = uStats.mean();
        Real upperSE = uStats.errorEstimate();

        int t5=clock();

        std::cout << " Upper - lower is, " << upperBound << ", with standard error " << upperSE << "\n";
        std::cout << " time to compute upper bound is,  " << (t5-t4)/static_cast<Real>(CLOCKS_PER_SEC) << ", seconds.\n";
        //#region
    }



    return 0;

}
*/


/*
void readFromFile()
{
    // t@C琔lǂݍŌvZʂo
    FILE* fp_in = freopen("data.txt", "r", stdin);
    FILE* fp_out = freopen("result.txt", "w", stdout);
    int n;
    while(std::cin >> n){
        std::cout << n << " * " << n << " = " << (n*n) << std::endl;
    }
    fclose(fp_in);
    fclose(fp_out);
}
*/

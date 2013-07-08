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


int main(int, char* []) {

    try {

        boost::timer timer;
        std::cout << std::endl;

        //-------------------------------------------------------------------
        LARGE_TITLE("Variance Swap");

        /* References:
          http://cfe.cboe.com/education/finaleuromoneyvarpaper.pdf
          Demeterfi, Derman, Kamal & Zou, "A Guide to Volatility and Variance Swaps", 1999
          http://math.uchicago.edu/~sbossu/VarSwaps.pdf
        */

        // setup dates
        Calendar calendar = TARGET();
        Date todaysDate(3, September, 2012);
        Date settlementDate = calendar.advance(todaysDate, 2*Days, ModifiedFollowing, false);
        Settings::instance().evaluationDate() = todaysDate;
        DayCounter dayCounter = Actual365Fixed();

        // contract specifications
        Volatility strikeVol = 19.0;
        Real strikeVariance = std::pow(strikeVol, 2);
        Volatility realizedVol = 25.0;
        Real realizedVariance = std::pow(realizedVol, 2);
        Position::Type position(Position::Long);
        Real vegaNotional = 100000.0;
        Real varianceNotional = vegaNotional / (2 * strikeVol); // market convention
        Date startDate(settlementDate);
        Date maturityDate = calendar.advance(startDate, 1*Years, ModifiedFollowing, false);
        Spread dividendYield = 0.000;
        Rate riskFreeRate = 0.013;

        VarianceSwap varianceSwap(
                    position, strikeVariance, varianceNotional, startDate, maturityDate);

        Handle<Quote> underlyingH(
            boost::shared_ptr<Quote>(new SimpleQuote(realizedVariance)));
        // bootstrap the yield/dividend/vol curves
        Handle<YieldTermStructure> flatTermStructure(
            boost::shared_ptr<YieldTermStructure>(
                new FlatForward(settlementDate, riskFreeRate, dayCounter)));
        Handle<YieldTermStructure> flatDividendTS(
            boost::shared_ptr<YieldTermStructure>(
                new FlatForward(settlementDate, dividendYield, dayCounter)));
        Handle<BlackVolTermStructure> flatVolTS(
            boost::shared_ptr<BlackVolTermStructure>(
                new BlackConstantVol(settlementDate, calendar, realizedVol,
                                     dayCounter)));
        boost::shared_ptr<GeneralizedBlackScholesProcess> process(
                 new BlackScholesMertonProcess(underlyingH, flatDividendTS,
                                               flatTermStructure, flatVolTS));

        Size timeSteps = 100;
        Size timeStepsPerYear = 1000;
        bool brownianBridge = true;
        bool antitheticVariate = true;
        Real requiredSamples = 50;
        Real requiredTolerance = 0.1;
        Size maxSamples = 300;
        BigNatural seed = 12286;

        boost::shared_ptr<PricingEngine> MCVarianceSwapEngine =
                MakeMCVarianceSwapEngine<PseudoRandom,Statistics>(process)
                .withStepsPerYear(timeStepsPerYear)
                .withAbsoluteTolerance(requiredTolerance)
                .withSeed(seed);
                //.withSteps(timeSteps)
                //.withSamples(requiredSamples)
                //.withMaxSamples(maxSamples)
                //.withBrownianBridge(brownianBridge) // default true
                //.withAntitheticVariate(antitheticVariate) // default true

        varianceSwap.setPricingEngine(MCVarianceSwapEngine);

        std::cout << "Realized Volatility: " << std::sqrt(varianceSwap.variance()) << std::endl;

        std::cout << "Variance risk premium (= realized variance - variance swap rate): "
                  << (realizedVariance - varianceSwap.variance()) << std::endl;

        Real varSwapMCNPV = varianceSwap.NPV();
        std::cout << "Variance Swap NPV:\t" << varSwapMCNPV << std::endl;

        // \note The payoff of a variance swap is convex in volatility.
        Real varSwapApproxNPV = varianceNotional*(realizedVariance - strikeVariance);
        std::cout << "Approximation:\t" << varSwapApproxNPV << std::endl;

        Real CvxAdj = varSwapMCNPV - varSwapApproxNPV;
        std::cout << "Convexity Adjustment:\t" << CvxAdj
                  << " (" << io::percent(CvxAdj/varSwapMCNPV) << ")" << std::endl;





        //-------------------------------------------------------------------

        // End test
        Real seconds = timer.elapsed();
        Integer hours = int(seconds/3600);
        seconds -= hours * 3600;
        Integer minutes = int(seconds/60);
        seconds -= minutes * 60;
        std::cout << " \nRun completed in ";
        if (hours > 0)
            std::cout << hours << " h ";
        if (hours > 0 || minutes > 0)
            std::cout << minutes << " m ";
        std::cout << std::fixed << std::setprecision(0)
                  << seconds << " s\n" << std::endl;
        return 0;

    } catch (std::exception& e) {
        std::cerr << e.what() << std::endl;
        return 1;
    } catch (...) {
        std::cerr << "unknown error" << std::endl;
        return 1;
    }
}

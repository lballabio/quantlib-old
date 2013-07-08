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

// the only header you need to use QuantLib
#include <ql/quantlib.hpp>

#ifdef BOOST_MSVC
/* Uncomment the following lines to unmask floating-point
   exceptions. Warning: unpredictable results can arise...

   See http://www.wilmott.com/messageview.cfm?catid=10&threadid=9481
   Is there anyone with a definitive word about this?
*/
// #include <float.h>
// namespace { unsigned int u = _controlfp(_EM_INEXACT, _MCW_EM); }
#endif

#include "customutilities.hpp"
#include <boost/timer.hpp>
#include <iostream>
#include <iomanip>

using namespace QuantLib;

#if defined(QL_ENABLE_SESSIONS)
namespace QuantLib {

    Integer sessionId() { return 0; }

}
#endif

/**
 * Alternating Direction Implicit (ADI) finite difference method
 * Ref:
 * - http://www.quantcode.com/modules/mydownloads/viewcat.php?cid=10&min=30&orderby=hitsD&show=5
 * - Efficient Pricing of an Asian Put Option Using Stiff ODE Methods
 *
 */

int main(int, char* []) {

    try {

        boost::timer timer;
        std::cout << std::endl;


        LARGE_TITLE("Alternating Direction Implicit (ADI)");



        if(false)
        {
        LARGE_TITLE("Equity Option with Finite Difference Method");

        // set up dates
        Calendar calendar = TARGET();
        Date todaysDate(3, September, 2012);
        Date settlementDate(5, September, 2012);
        Settings::instance().evaluationDate() = todaysDate;

        // our options
        Option::Type type(Option::Put);
        Real underlying = 45;
        Real strike = 50;
        Spread dividendYield = 0.015;
        Rate riskFreeRate = 0.009;
        Volatility volatility = 0.25;
        Date maturity(3, September, 2013);
        DayCounter dayCounter = Actual365Fixed();

        std::cout << std::setw(30) << "Valuation date = " << todaysDate << std::endl;
        std::cout << std::setw(30) << "Maturity = " << maturity << std::endl;
        std::cout << std::setw(30) << "Option type = " << type << std::endl;
        std::cout << std::setw(30) << "Underlying price = " << underlying << std::endl;
        std::cout << std::setw(30) << "Strike = " << strike << std::endl;
        std::cout << std::setw(30) << "Risk-free interest rate = " << io::rate(riskFreeRate)
                  << std::endl;
        std::cout  << std::setw(30)<< "Dividend yield = " << io::rate(dividendYield)
                  << std::endl;
        std::cout << std::setw(30) << "Volatility = " << io::volatility(volatility)
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
//        Handle<SimpleQuote> underlyingH(
//            boost::shared_ptr<SimpleQuote>(new SimpleQuote(underlying)));

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






        // Finite differences
        Size timeSteps = 801;
        method = "FD (Crank Nicolson)";
        europeanOption.setPricingEngine(boost::shared_ptr<PricingEngine>(
                 new FDEuropeanEngine<CrankNicolson>(bsmProcess,
                                                     timeSteps,timeSteps-1)));
        bermudanOption.setPricingEngine(boost::shared_ptr<PricingEngine>(
                 new FDBermudanEngine<CrankNicolson>(bsmProcess,
                                                     timeSteps,timeSteps-1)));
        americanOption.setPricingEngine(boost::shared_ptr<PricingEngine>(
                 new FDAmericanEngine<CrankNicolson>(bsmProcess,
                                                     timeSteps,timeSteps-1)));
        std::cout << std::setw(widths[0]) << std::left << method
                  << std::fixed
                  << std::setw(widths[1]) << std::left << europeanOption.NPV()
                  << std::setw(widths[2]) << std::left << bermudanOption.NPV()
                  << std::setw(widths[3]) << std::left << americanOption.NPV()
                  << std::endl;

        method = "FD (Explcit Euler)";
        europeanOption.setPricingEngine(boost::shared_ptr<PricingEngine>(
                 new FDEuropeanEngine<ExplicitEuler>(bsmProcess,
                                                     timeSteps,timeSteps-1)));
        bermudanOption.setPricingEngine(boost::shared_ptr<PricingEngine>(
                 new FDBermudanEngine<ExplicitEuler>(bsmProcess,
                                                     timeSteps,timeSteps-1)));
        americanOption.setPricingEngine(boost::shared_ptr<PricingEngine>(
                 new FDAmericanEngine<ExplicitEuler>(bsmProcess,
                                                     timeSteps,timeSteps-1)));
        std::cout << std::setw(widths[0]) << std::left << method
                  << std::fixed
                  << std::setw(widths[1]) << std::left << europeanOption.NPV()
                  << std::setw(widths[2]) << std::left << bermudanOption.NPV()
                  << std::setw(widths[3]) << std::left << americanOption.NPV()
                  << std::endl;

        method = "FD (Implicit Euler)";
        europeanOption.setPricingEngine(boost::shared_ptr<PricingEngine>(
                 new FDEuropeanEngine<ImplicitEuler>(bsmProcess,
                                                     timeSteps,timeSteps-1)));
        bermudanOption.setPricingEngine(boost::shared_ptr<PricingEngine>(
                 new FDBermudanEngine<ImplicitEuler>(bsmProcess,
                                                     timeSteps,timeSteps-1)));
        americanOption.setPricingEngine(boost::shared_ptr<PricingEngine>(
                 new FDAmericanEngine<ImplicitEuler>(bsmProcess,
                                                     timeSteps,timeSteps-1)));
        std::cout << std::setw(widths[0]) << std::left << method
                  << std::fixed
                  << std::setw(widths[1]) << std::left << europeanOption.NPV()
                  << std::setw(widths[2]) << std::left << bermudanOption.NPV()
                  << std::setw(widths[3]) << std::left << americanOption.NPV()
                  << std::endl;


        LARGE_TITLE("Finite Difference Greeks (SensitivityAnalysis)");

        //std::cout << Centered << std::endl;
        boost::shared_ptr<Instrument> europeanOption_(
                    new VanillaOption(payoff, europeanExercise));
        boost::shared_ptr<Instrument> bermudanOption_(
                    new VanillaOption(payoff, bermudanExercise));
        boost::shared_ptr<Instrument> americanOption_(
                    new VanillaOption(payoff, americanExercise));
        method = "FD (Crank Nicolson)";
        europeanOption_->setPricingEngine(boost::shared_ptr<PricingEngine>(
                 new FDEuropeanEngine<CrankNicolson>(bsmProcess,
                                                     timeSteps,timeSteps-1)));
        bermudanOption_->setPricingEngine(boost::shared_ptr<PricingEngine>(
                 new FDBermudanEngine<CrankNicolson>(bsmProcess,
                                                     timeSteps,timeSteps-1)));
        americanOption_->setPricingEngine(boost::shared_ptr<PricingEngine>(
                 new FDAmericanEngine<CrankNicolson>(bsmProcess,
                                                     timeSteps,timeSteps-1)));
        std::cout << std::setw(widths[0]) << std::left << method
                  << std::fixed
                  << std::setw(widths[1]) << std::left << europeanOption_->NPV()
                  << std::setw(widths[2]) << std::left << bermudanOption_->NPV()
                  << std::setw(widths[3]) << std::left << americanOption_->NPV()
                  << std::endl;

        std::vector<boost::shared_ptr<Instrument> > instruments;
        instruments.push_back(europeanOption_);
        instruments.push_back(bermudanOption_);
        instruments.push_back(americanOption_);
        std::vector<Real> weights;
        Real sumNPV(aggregateNPV(instruments, weights));
        std::cout << "Aggregated NPV: " << sumNPV << std::endl;

        // TODO: rectify quotes
        std::vector<Handle<SimpleQuote> > quotes;
        //Handle<Quote> underlyingH(
        //    boost::shared_ptr<Quote>(new SimpleQuote(underlying)));
        Handle<SimpleQuote> quote(
            boost::shared_ptr<SimpleQuote>(new SimpleQuote(underlying)));
        //Handle<SimpleQuote> quote(boost::dynamic_pointer_cast<SimpleQuote>(underlyingH));
        quotes.push_back(quote);
        quotes.push_back(quote);
        quotes.push_back(quote);

        Real shift(10.0);

        std::pair<Real, Real> sensitivities =
                parallelAnalysis(
                    quotes,
                    instruments,
                    weights,
                    shift,
                    Centered,
                    Null<Real>());

        std::cout << "Onesided sensitivity: " << sensitivities.first << std::endl;
        std::cout << "Centered sensitivity: " << sensitivities.second << std::endl;
        }





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

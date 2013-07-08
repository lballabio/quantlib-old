/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*!
 Copyright (C) 2005, 2006, 2007 StatPro Italia srl
 Copyright (C) 2008 Bojan Nikolic

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

#include "stdafx.h"

#ifndef EQUITY_OPTION_REWORK_HPP
#define EQUITY_OPTION_REWORK_HPP

// the only header you need to use QuantLib
#include <ql/quantlib.hpp>

//#include <boost/timer.hpp>
#include <boost/variant.hpp>
#include <iostream>
#include <iomanip>

using namespace QuantLib;


//-------------------------------//
//       EquityOptionEx();       //
//-------------------------------//


// http://www.bnikolic.co.uk/blog/ql-equity-option-ex.html

struct OptionInputs {
    Option::Type type;
    Real underlying;
    Real strike;
    Spread dividendYield;
    Rate riskFreeRate;
    Volatility volatility;
    Date maturity;
    DayCounter dayCounter;
};


void PrintInputs(std::ostream & os,
                 const OptionInputs &in)
{
    os << "Option type = "  << in.type << std::endl;
    os << "Maturity = "        << in.maturity << std::endl;
    os << "Underlying price = "        << in.underlying << std::endl;
    os << "Strike = "                  << in.strike << std::endl;
    os << "Risk-free interest rate = " << io::rate(in.riskFreeRate)
              << std::endl;
    os << "Dividend yield = " << io::rate(in.dividendYield)
              << std::endl;
    os << "Volatility = " << io::volatility(in.volatility)
              << std::endl;
    os << std::endl;
}

typedef boost::variant<const std::string &, double, const char * > OutputEl;

/** A simple helper class to print the
    output variant
*/
class OutputElPrinter:
  public boost::static_visitor<>
{

  std::ostream & os;

public:

  OutputElPrinter (std::ostream & os) :
    os(os)
  {
  }

  void operator()(const std::string & str) const
  {
      os << str;
  }

  void operator()(const char * str) const
  {
      os << str;
  }

  void operator()(double d) const
  {
      os << d;
  }

};


void PrintResRow( const std::string & method,
                  OutputEl Euro,
                  OutputEl Berm,
                  OutputEl Amer)
{
    Size widths[] = { 35, 14, 14, 14 };
    OutputElPrinter op(std::cout);

    std::cout << std::setw(widths[0]) << std::left << method
              << std::setw(widths[1]) << std::left;
    boost::apply_visitor(op,Euro);
    std::cout << std::setw(widths[2]) << std::left;
    boost::apply_visitor(op,Berm);
    std::cout << std::setw(widths[3]) << std::left;
    boost::apply_visitor(op,Amer);
    std::cout << std::endl;
}

void BlackScholesEx( VanillaOption & euro,
                     boost::shared_ptr<BlackScholesMertonProcess> bsmProcess)
{

    boost::shared_ptr<PricingEngine> pe ( new AnalyticEuropeanEngine(bsmProcess)) ;

    euro.setPricingEngine(pe);

    PrintResRow("Black-Scholes",
                euro.NPV(),
                "N/A",
                "N/A");
}

void BaroneAdesiWhaleyEx( VanillaOption & amer,
                          boost::shared_ptr<BlackScholesMertonProcess> bsmProcess)
{
    boost::shared_ptr<PricingEngine> pe ( new BaroneAdesiWhaleyApproximationEngine(bsmProcess) );

    amer.setPricingEngine(pe);

    PrintResRow("Barone-Adesi/Whaley",
                "N/A",
                "N/A",
                amer.NPV());

}

void BjerksundStenslandEx( VanillaOption & amer,
                         boost::shared_ptr<BlackScholesMertonProcess> bsmProcess)
{
    boost::shared_ptr<PricingEngine> pe ( new BjerksundStenslandApproximationEngine(bsmProcess) );

    amer.setPricingEngine(pe);

    PrintResRow("Bjerksund/Stensland",
                "N/A",
                "N/A",
                amer.NPV());

}

void IntegralEx( VanillaOption & euro,
                 boost::shared_ptr<BlackScholesMertonProcess> bsmProcess)
{
    boost::shared_ptr<PricingEngine> pe(new IntegralEngine(bsmProcess));

    euro.setPricingEngine(pe);

    PrintResRow("Integral",
                euro.NPV(),
                "N/A",
                "N/A");
}

/*
void FiniteDifferencesEx( VanillaOption & euro,
                          VanillaOption & berm,
                          VanillaOption & amer,
                          boost::shared_ptr<BlackScholesMertonProcess> bsmProcess,
                          Size timeSteps)
{

    boost::shared_ptr<PricingEngine> euro_pe(new FDEuropeanEngine(bsmProcess,
                                                                   timeSteps,
                                                                   timeSteps-1));

    euro.setPricingEngine(euro_pe);

    boost::shared_ptr<PricingEngine> berm_pe(new FDBermudanEngine(bsmProcess,
                                                                  timeSteps,
                                                                  timeSteps-1));

    berm.setPricingEngine(berm_pe);


    boost::shared_ptr<PricingEngine> amer_pe(new FDAmericanEngine(bsmProcess,
                                                                  timeSteps,
                                                                  timeSteps-1));

    amer.setPricingEngine(amer_pe);

    PrintResRow("Finite differences",
                euro.NPV(),
                berm.NPV(),
                amer.NPV());


}
*/

void BinomialJarrowRuddEx( VanillaOption & euro,
                           VanillaOption & berm,
                           VanillaOption & amer,
                           boost::shared_ptr<BlackScholesMertonProcess> bsmProcess,
                           Size timeSteps)
{

    boost::shared_ptr<PricingEngine>  euro_pe(new BinomialVanillaEngine<JarrowRudd>(bsmProcess,
                                                                                    timeSteps));
    euro.setPricingEngine(euro_pe);


    boost::shared_ptr<PricingEngine> berm_pe(new BinomialVanillaEngine<JarrowRudd>(bsmProcess,
                                                                                   timeSteps));

    berm.setPricingEngine(berm_pe);


    boost::shared_ptr<PricingEngine> amer_pe(new BinomialVanillaEngine<JarrowRudd>(bsmProcess,
                                                                                   timeSteps));

    amer.setPricingEngine(amer_pe);

    PrintResRow("Binomial Jarrow-Rudd",
                euro.NPV(),
                berm.NPV(),
                amer.NPV());

}

void BinomialCoxRossRubinstein( VanillaOption & euro,
                                VanillaOption & berm,
                                VanillaOption & amer,
                                boost::shared_ptr<BlackScholesMertonProcess> bsmProcess,
                                Size timeSteps)
{

    boost::shared_ptr<PricingEngine>  euro_pe(new BinomialVanillaEngine<CoxRossRubinstein>(bsmProcess,
                                                                                           timeSteps));
    euro.setPricingEngine(euro_pe);


    boost::shared_ptr<PricingEngine> berm_pe(new BinomialVanillaEngine<CoxRossRubinstein>(bsmProcess,
                                                                                          timeSteps));
    berm.setPricingEngine(berm_pe);


    boost::shared_ptr<PricingEngine> amer_pe(new BinomialVanillaEngine<CoxRossRubinstein>(bsmProcess,
                                                                                          timeSteps));

    amer.setPricingEngine(amer_pe);

    PrintResRow("Binomial Cox-Ross-Rubinstein",
                euro.NPV(),
                berm.NPV(),
                amer.NPV());

}

void AdditiveEquiprobabilitiesEx( VanillaOption & euro,
                                  VanillaOption & berm,
                                  VanillaOption & amer,
                                  boost::shared_ptr<BlackScholesMertonProcess> bsmProcess,
                                  Size timeSteps)
{

    boost::shared_ptr<PricingEngine>  euro_pe(new BinomialVanillaEngine<AdditiveEQPBinomialTree>(bsmProcess,
                                                                                                 timeSteps));
    euro.setPricingEngine(euro_pe);


    boost::shared_ptr<PricingEngine> berm_pe(new BinomialVanillaEngine<AdditiveEQPBinomialTree>(bsmProcess,
                                                                                                timeSteps));
    berm.setPricingEngine(berm_pe);


    boost::shared_ptr<PricingEngine> amer_pe(new BinomialVanillaEngine<AdditiveEQPBinomialTree>(bsmProcess,
                                                                                                timeSteps));
    amer.setPricingEngine(amer_pe);

    PrintResRow("Additive equiprobabilities",
                euro.NPV(),
                berm.NPV(),
                amer.NPV());

}

void BinomialTrigeorgisEx( VanillaOption & euro,
                           VanillaOption & berm,
                           VanillaOption & amer,
                           boost::shared_ptr<BlackScholesMertonProcess> bsmProcess,
                           Size timeSteps)
{

    boost::shared_ptr<PricingEngine>  euro_pe(new BinomialVanillaEngine<Trigeorgis>(bsmProcess,
                                                                                    timeSteps));
    euro.setPricingEngine(euro_pe);


    boost::shared_ptr<PricingEngine> berm_pe(new BinomialVanillaEngine<Trigeorgis>(bsmProcess,
                                                                                   timeSteps));
    berm.setPricingEngine(berm_pe);


    boost::shared_ptr<PricingEngine> amer_pe(new BinomialVanillaEngine<Trigeorgis>(bsmProcess,
                                                                                   timeSteps));
    amer.setPricingEngine(amer_pe);

    PrintResRow("Binomial Trigeorgis",
                euro.NPV(),
                berm.NPV(),
                amer.NPV());

}

void BinomialTianEx( VanillaOption & euro,
                     VanillaOption & berm,
                     VanillaOption & amer,
                     boost::shared_ptr<BlackScholesMertonProcess> bsmProcess,
                     Size timeSteps)
{

    boost::shared_ptr<PricingEngine>  euro_pe(new BinomialVanillaEngine<Tian>(bsmProcess,
                                                                              timeSteps));
    euro.setPricingEngine(euro_pe);


    boost::shared_ptr<PricingEngine> berm_pe(new BinomialVanillaEngine<Tian>(bsmProcess,
                                                                             timeSteps));
    berm.setPricingEngine(berm_pe);


    boost::shared_ptr<PricingEngine> amer_pe(new BinomialVanillaEngine<Tian>(bsmProcess,
                                                                             timeSteps));
    amer.setPricingEngine(amer_pe);

    PrintResRow("Binomial Tian",
                euro.NPV(),
                berm.NPV(),
                amer.NPV());

}

void BinomialLeisenReimerEx( VanillaOption & euro,
                             VanillaOption & berm,
                             VanillaOption & amer,
                             boost::shared_ptr<BlackScholesMertonProcess> bsmProcess,
                             Size timeSteps)
{

    boost::shared_ptr<PricingEngine>  euro_pe(new BinomialVanillaEngine<LeisenReimer>(bsmProcess,
                                                                                      timeSteps));
    euro.setPricingEngine(euro_pe);


    boost::shared_ptr<PricingEngine> berm_pe(new BinomialVanillaEngine<LeisenReimer>(bsmProcess,
                                                                                     timeSteps));
    berm.setPricingEngine(berm_pe);


    boost::shared_ptr<PricingEngine> amer_pe(new BinomialVanillaEngine<LeisenReimer>(bsmProcess,
                                                                                     timeSteps));
    amer.setPricingEngine(amer_pe);

    PrintResRow("Binomial Leisen-Reimer",
                euro.NPV(),
                berm.NPV(),
                amer.NPV());

}

void BinomialJoshiEx( VanillaOption & euro,
                      VanillaOption & berm,
                      VanillaOption & amer,
                      boost::shared_ptr<BlackScholesMertonProcess> bsmProcess,
                      Size timeSteps)
{

    boost::shared_ptr<PricingEngine>  euro_pe(new BinomialVanillaEngine<Joshi4>(bsmProcess,
                                                                                timeSteps));
    euro.setPricingEngine(euro_pe);


    boost::shared_ptr<PricingEngine> berm_pe(new BinomialVanillaEngine<Joshi4>(bsmProcess,
                                                                               timeSteps));
    berm.setPricingEngine(berm_pe);


    boost::shared_ptr<PricingEngine> amer_pe(new BinomialVanillaEngine<Joshi4>(bsmProcess,
                                                                               timeSteps));
    amer.setPricingEngine(amer_pe);

    PrintResRow("Binomial Joshi",
                euro.NPV(),
                berm.NPV(),
                amer.NPV());

}

void MCCrudeEx( VanillaOption & euro,
                boost::shared_ptr<BlackScholesMertonProcess> bsmProcess)
{
    Size timeSteps = 1;
    Size mcSeed = 42;

    boost::shared_ptr<PricingEngine> mcengine1;
    mcengine1 = MakeMCEuropeanEngine<PseudoRandom>(bsmProcess)
        .withSteps(timeSteps)
        .withAbsoluteTolerance(0.02)
        .withSeed(mcSeed);

    euro.setPricingEngine(mcengine1);

    PrintResRow("MC (crude)",
                euro.NPV(),
                "N/A",
                "N/A");

}

void QMCSobolEx( VanillaOption & euro,
                boost::shared_ptr<BlackScholesMertonProcess> bsmProcess)
{
    Size timeSteps = 1;
    Size nSamples = 32768;  // 2^15

    boost::shared_ptr<PricingEngine> mcengine2;
    mcengine2 = MakeMCEuropeanEngine<LowDiscrepancy>(bsmProcess)
        .withSteps(timeSteps)
        .withSamples(nSamples);

    euro.setPricingEngine(mcengine2);

    PrintResRow("QMC (Sobol)",
                euro.NPV(),
                "N/A",
                "N/A");

}

/*void MCLongstaffSchwartzEx( VanillaOption & amer,
                            boost::shared_ptr<BlackScholesMertonProcess> bsmProcess)
{
    Size mcSeed = 42;

    boost::shared_ptr<PricingEngine> mcengine3;
    mcengine3 = MakeMCAmericanEngine<PseudoRandom>(bsmProcess)
        .withSteps(100)
        .withAntitheticVariate()
        .withCalibrationSamples(4096)
        .withTolerance(0.02)
        .withSeed(mcSeed);

    amer.setPricingEngine(mcengine3);

    PrintResRow("MC (Longstaff Schwartz)",
                "N/A",
                "N/A",
                amer.NPV());
}*/




void EquityOptionEx(void)
{

    std::cout << std::endl;

    // set up dates
    Calendar calendar = TARGET();
    Date todaysDate(15, May, 1998);
    Date settlementDate(17, May, 1998);
    Settings::instance().evaluationDate() = todaysDate;

    // our options
    OptionInputs in;
    in.type = Option::Put;
    in.underlying = 36;
    in.strike = 40;
    in.dividendYield = 0.00;
    in.riskFreeRate = 0.06;
    in.volatility = 0.20;
    in.maturity= Date(17, May, 1999);
    in.dayCounter = Actual365Fixed();

    PrintInputs(std::cout, in);
    std::cout << std::endl ;




    // write column headings
    PrintResRow("Method",
                "European",
                "Bermudan",
                "American");


    std::vector<Date> exerciseDates;
    for (Integer i=1; i<=4; i++)
        exerciseDates.push_back(settlementDate + 3*i*Months);

    boost::shared_ptr<Exercise> europeanExercise(
                                                 new EuropeanExercise(in.maturity));

    boost::shared_ptr<Exercise> bermudanExercise(
                                                 new BermudanExercise(exerciseDates));

    boost::shared_ptr<Exercise> americanExercise(
                                                 new AmericanExercise(settlementDate,
                                                                      in.maturity));

    Handle<Quote> underlyingH(
                              boost::shared_ptr<Quote>(new SimpleQuote(in.underlying)));

    // bootstrap the yield/dividend/vol curves
    Handle<YieldTermStructure> flatTermStructure(
                                                 boost::shared_ptr<YieldTermStructure>(
                                                                                       new FlatForward(settlementDate,
                                                                                                       in.riskFreeRate,
                                                                                                       in.dayCounter)));
    Handle<YieldTermStructure> flatDividendTS(
                                              boost::shared_ptr<YieldTermStructure>(
                                                                                    new FlatForward(settlementDate,
                                                                                                    in.dividendYield,
                                                                                                    in.dayCounter)));
    Handle<BlackVolTermStructure> flatVolTS(
                                            boost::shared_ptr<BlackVolTermStructure>(
                                                                                     new BlackConstantVol(settlementDate,
                                                                                                          calendar,
                                                                                                          in.volatility,
                                                                                                          in.dayCounter)));
    boost::shared_ptr<StrikedTypePayoff> payoff(
                                                new PlainVanillaPayoff(in.type,
                                                                       in.strike));

    boost::shared_ptr<BlackScholesMertonProcess> bsmProcess(
                                                            new BlackScholesMertonProcess(underlyingH,
                                                                                          flatDividendTS,
                                                                                          flatTermStructure,
                                                                                          flatVolTS));

    // options
    VanillaOption europeanOption(payoff, europeanExercise);
    VanillaOption bermudanOption(payoff, bermudanExercise);
    VanillaOption americanOption(payoff, americanExercise);

    // Analytic formulas:

    // Black-Scholes for European
    BlackScholesEx(europeanOption,
                   bsmProcess);

    // Barone-Adesi and Whaley approximation for American
    BaroneAdesiWhaleyEx(americanOption,
                        bsmProcess);

    // Bjerksund and Stensland approximation for American
    BjerksundStenslandEx(americanOption,
                         bsmProcess);

    // Integral
    IntegralEx(europeanOption,
               bsmProcess);

    std::string method;
    Size timeSteps = 801;

    // Finite differences	
    /*FiniteDifferencesEx(europeanOption,
                        bermudanOption,
                        americanOption,
                        bsmProcess,
                        timeSteps);*/

    // Binomial method: Jarrow-Rudd
    BinomialJarrowRuddEx(europeanOption,
                         bermudanOption,
                         americanOption,
                         bsmProcess,
                         timeSteps);


    BinomialCoxRossRubinstein(europeanOption,
                              bermudanOption,
                              americanOption,
                              bsmProcess,
                              timeSteps);

    // Binomial method: Additive equiprobabilities
    AdditiveEquiprobabilitiesEx(europeanOption,
                                bermudanOption,
                                americanOption,
                                bsmProcess,
                                timeSteps);

    // Binomial method: Binomial Trigeorgis
    BinomialTrigeorgisEx(europeanOption,
                         bermudanOption,
                         americanOption,
                         bsmProcess,
                         timeSteps);


    // Binomial method: Binomial Tian
    BinomialTianEx(europeanOption,
                   bermudanOption,
                   americanOption,
                   bsmProcess,
                   timeSteps);

    // Binomial method: Binomial Leisen-Reimer
    BinomialLeisenReimerEx(europeanOption,
                           bermudanOption,
                           americanOption,
                           bsmProcess,
                           timeSteps);

    // Binomial method: Binomial Joshi
    BinomialJoshiEx(europeanOption,
                    bermudanOption,
                    americanOption,
                    bsmProcess,
                    timeSteps);

    // Monte Carlo Method: MC (crude)
    MCCrudeEx(europeanOption,
              bsmProcess);

    // Monte Carlo Method: QMC (Sobol)
    QMCSobolEx(europeanOption,
               bsmProcess);


    // Monte Carlo Method: MC (Longstaff Schwartz)
    /*MCLongstaffSchwartzEx(americanOption,
                          bsmProcess);*/

}

#endif EQUITY_OPTION_REWORK_HPP
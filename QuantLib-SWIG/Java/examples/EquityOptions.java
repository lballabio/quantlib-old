
/*
 Copyright (C) 2007 Richard Gomes
 Copyright (C) 2007 Tito Ingargiola

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

package examples;

import org.quantlib.QuantLib;
import org.quantlib.Actual365Fixed;
import org.quantlib.AmericanExercise;
import org.quantlib.AnalyticEuropeanEngine;
import org.quantlib.BaroneAdesiWhaleyEngine;
import org.quantlib.BermudanExercise;
import org.quantlib.BinomialVanillaEngine;
import org.quantlib.BjerksundStenslandEngine;
import org.quantlib.BlackConstantVol;
import org.quantlib.BlackScholesMertonProcess;
import org.quantlib.BlackVolTermStructure;
import org.quantlib.BlackVolTermStructureHandle;
import org.quantlib.Calendar;
import org.quantlib.Date;
import org.quantlib.DateVector;
import org.quantlib.DayCounter;
import org.quantlib.EuropeanExercise;
import org.quantlib.Exercise;
import org.quantlib.FDAmericanEngine;
import org.quantlib.FDBermudanEngine;
import org.quantlib.FDEuropeanEngine;
import org.quantlib.FlatForward;
import org.quantlib.IntegralEngine;
import org.quantlib.MCEuropeanEngine;
import org.quantlib.Month;
import org.quantlib.Option;
import org.quantlib.Payoff;
import org.quantlib.Period;
import org.quantlib.PlainVanillaPayoff;
import org.quantlib.Quote;
import org.quantlib.QuoteHandle;
import org.quantlib.Settings;
import org.quantlib.SimpleQuote;
import org.quantlib.StochasticProcess;
import org.quantlib.TARGET;
import org.quantlib.TimeUnit;
import org.quantlib.VanillaOption;
import org.quantlib.YieldTermStructure;
import org.quantlib.YieldTermStructureHandle;

/**
 * EquityOption Test app - java version of QuantLib/Examples/EquityOption
 * to illustrate use of Quantlib through supplied SWIG interfaces.
 *
 * You need to run this with a correctly set library path and something like:
 *
 * -Djava.library.path=/usr/local/lib
 *
 * @author Richard Gomes
 * @author Tito Ingargiola
 */
public class EquityOptions {

    static {
        try {
            System.loadLibrary("QuantLibJNI");
        } catch (RuntimeException e) {
            e.printStackTrace();
        }
    }

    public static void main(String[] args) throws Exception {
        long beginTime = System.currentTimeMillis();

        // our option
        Option.Type type = Option.Type.Put;
        double strike = 40.0;
        double underlying = 36.0;
        double riskFreeRate = 0.06;
        double dividendYield = 0.00;
        double volatility = 0.2;

        Date todaysDate = new Date(15, Month.May, 1998);
        Date settlementDate = new Date(17, Month.May, 1998);
        Settings.instance().setEvaluationDate(todaysDate);

        Date maturity = new Date(17, Month.May, 1999);
        DayCounter dayCounter = new Actual365Fixed();
        Calendar calendar = new TARGET();

        // write column headings
        String fmt = "\n%-35s %-14s %-14s %-14s\n";
        System.out.printf(fmt, "Method", "European", "Bermudan", "American");
        System.out.println("============================================================================");


        // define European, Bermudan, and American exercises
        DateVector exerciseDates = new DateVector();
        for (int i = 1; i <= 4; i++) {
            Date forward = settlementDate.add(new Period(3*i, TimeUnit.Months));
            exerciseDates.add(forward);
        }
        Exercise europeanExercise = new EuropeanExercise(maturity);
        Exercise bermudanExercise = new BermudanExercise(exerciseDates);
        Exercise americanExercise = new AmericanExercise(settlementDate,
                                                         maturity);


        // define the underlying asset and the yield/dividend/volatility curves
        QuoteHandle underlyingH = new QuoteHandle(new SimpleQuote(underlying));
        YieldTermStructureHandle flatTermStructure =
            new YieldTermStructureHandle(new FlatForward(
                                  settlementDate, riskFreeRate, dayCounter));
        YieldTermStructureHandle flatDividendYield =
            new YieldTermStructureHandle(new FlatForward(
                                  settlementDate, dividendYield, dayCounter));
        BlackVolTermStructureHandle flatVolatility =
            new BlackVolTermStructureHandle(new BlackConstantVol(
                           settlementDate, calendar, volatility, dayCounter));

        BlackScholesMertonProcess stochasticProcess =
            new BlackScholesMertonProcess(underlyingH,
                                          flatDividendYield,
                                          flatTermStructure,
                                          flatVolatility);

        // options
        Payoff payoff = new PlainVanillaPayoff(type, strike);

        VanillaOption europeanOption =
            new VanillaOption(payoff, europeanExercise);
        VanillaOption bermudanOption =
            new VanillaOption(payoff, bermudanExercise);
        VanillaOption americanOption =
            new VanillaOption(payoff, americanExercise);

        fmt = "%34s %13.9f %13.9f %13.9f\n";

        // Analytic formulas:

        // Black-Scholes for European
        String method = "Black-Scholes";
        europeanOption.setPricingEngine(
                               new AnalyticEuropeanEngine(stochasticProcess));
        System.out.printf(fmt, new Object[] { method,
                                              europeanOption.NPV(),
                                              Double.NaN,
                                              Double.NaN } );

        // Barone-Adesi and Whaley approximation for American
        method = "Barone-Adesi/Whaley";
        americanOption.setPricingEngine(
                              new BaroneAdesiWhaleyEngine(stochasticProcess));
        System.out.printf(fmt, new Object[] { method,
                                              Double.NaN,
                                              Double.NaN,
                                              americanOption.NPV() } );

        // Bjerksund and Stensland approximation for American
        method = "Bjerksund/Stensland";
        americanOption.setPricingEngine(
                             new BjerksundStenslandEngine(stochasticProcess));
        System.out.printf(fmt, new Object[] { method,
                                              Double.NaN,
                                              Double.NaN,
                                              americanOption.NPV() } );

        // Integral
        method = "Integral";
        europeanOption.setPricingEngine(new IntegralEngine(stochasticProcess));
        System.out.printf(fmt, new Object[] { method, europeanOption.NPV(),
                                              Double.NaN, Double.NaN } );

        // Finite differences
        int timeSteps = 801;
        method = "Finite differences";
        europeanOption.setPricingEngine(new FDEuropeanEngine(stochasticProcess,
                                                             timeSteps,
                                                             timeSteps-1));
        bermudanOption.setPricingEngine(new FDBermudanEngine(stochasticProcess,
                                                             timeSteps,
                                                             timeSteps-1));
        americanOption.setPricingEngine(new FDAmericanEngine(stochasticProcess,
                                                             timeSteps,
                                                             timeSteps-1));
        System.out.printf(fmt, new Object[] { method,
                                              europeanOption.NPV(),
                                              bermudanOption.NPV(),
                                              americanOption.NPV() });

        // Binomial method
        method = "Binomial Jarrow-Rudd";
        europeanOption.setPricingEngine(
                          new BinomialVanillaEngine(stochasticProcess,
                                                    "JarrowRudd", timeSteps));
        bermudanOption.setPricingEngine(new BinomialVanillaEngine(
                                                    stochasticProcess,
                                                    "JarrowRudd", timeSteps));
        americanOption.setPricingEngine(new BinomialVanillaEngine(
                                                    stochasticProcess,
                                                    "JarrowRudd", timeSteps));
        System.out.printf(fmt, new Object[] { method,
                                              europeanOption.NPV(),
                                              bermudanOption.NPV(),
                                              americanOption.NPV() } );

        method = "Binomial Cox-Ross-Rubinstein";
        europeanOption.setPricingEngine(
                   new BinomialVanillaEngine(stochasticProcess,
                                             "CoxRossRubinstein", timeSteps));
        bermudanOption.setPricingEngine(
                   new BinomialVanillaEngine(stochasticProcess,
                                             "CoxRossRubinstein", timeSteps));
        americanOption.setPricingEngine(
                   new BinomialVanillaEngine(stochasticProcess,
                                             "CoxRossRubinstein", timeSteps));
        System.out.printf(fmt, new Object[] { method,
                                              europeanOption.NPV(),
                                              bermudanOption.NPV(),
                                              americanOption.NPV() } );

        method = "Additive equiprobabilities";
        europeanOption.setPricingEngine(
             new BinomialVanillaEngine(stochasticProcess,
                                       "AdditiveEQPBinomialTree", timeSteps));
        bermudanOption.setPricingEngine(
             new BinomialVanillaEngine(stochasticProcess,
                                       "AdditiveEQPBinomialTree", timeSteps));
        americanOption.setPricingEngine(
             new BinomialVanillaEngine(stochasticProcess,
                                       "AdditiveEQPBinomialTree", timeSteps));
        System.out.printf(fmt, new Object[] { method,
                                              europeanOption.NPV(),
                                              bermudanOption.NPV(),
                                              americanOption.NPV() } );

        method = "Binomial Trigeorgis";
        europeanOption.setPricingEngine(
                          new BinomialVanillaEngine(stochasticProcess,
                                                    "Trigeorgis", timeSteps));
        bermudanOption.setPricingEngine(
                          new BinomialVanillaEngine(stochasticProcess,
                                                    "Trigeorgis", timeSteps));
        americanOption.setPricingEngine(
                          new BinomialVanillaEngine(stochasticProcess,
                                                    "Trigeorgis", timeSteps));
        System.out.printf(fmt, new Object[] { method,
                                              europeanOption.NPV(),
                                              bermudanOption.NPV(),
                                              americanOption.NPV() } );

        method = "Binomial Tian";
        europeanOption.setPricingEngine(
                                new BinomialVanillaEngine(stochasticProcess,
                                                          "Tian", timeSteps));
        bermudanOption.setPricingEngine(
                                new BinomialVanillaEngine(stochasticProcess,
                                                          "Tian", timeSteps));
        americanOption.setPricingEngine(
                                new BinomialVanillaEngine(stochasticProcess,
                                                          "Tian", timeSteps));
        System.out.printf(fmt, new Object[] { method,
                                              europeanOption.NPV(),
                                              bermudanOption.NPV(),
                                              americanOption.NPV() } );

        method = "Binomial Leisen-Reimer";
        europeanOption.setPricingEngine(
                        new BinomialVanillaEngine(stochasticProcess,
                                                  "LeisenReimer", timeSteps));
        bermudanOption.setPricingEngine(
                        new BinomialVanillaEngine(stochasticProcess,
                                                  "LeisenReimer", timeSteps));
        americanOption.setPricingEngine(
                        new BinomialVanillaEngine(stochasticProcess,
                                                  "LeisenReimer", timeSteps));
        System.out.printf(fmt, new Object[] { method,
                                              europeanOption.NPV(),
                                              bermudanOption.NPV(),
                                              americanOption.NPV() } );

        method = "Binomial Joshi";
        europeanOption.setPricingEngine(
                              new BinomialVanillaEngine(stochasticProcess,
                                                        "Joshi4", timeSteps));
        bermudanOption.setPricingEngine(
                              new BinomialVanillaEngine(stochasticProcess,
                                                        "Joshi4", timeSteps));
        americanOption.setPricingEngine(
                              new BinomialVanillaEngine(stochasticProcess,
                                                        "Joshi4", timeSteps));
        System.out.printf(fmt, new Object[] { method,
                                              europeanOption.NPV(),
                                              bermudanOption.NPV(),
                                              americanOption.NPV() } );


        // Monte Carlo Method
        timeSteps = 1;
        int mcSeed = 42;
        int nSamples = 32768; // 2^15
        int maxSamples = 1048576; // 2^20

        method = "MC (crude)";
        europeanOption.setPricingEngine(
                    new MCEuropeanEngine(stochasticProcess,
                                         "PseudoRandom", timeSteps,
                                         QuantLib.nullInt(),
                                         false, false,
                                         nSamples, 0.02, maxSamples, mcSeed));
        System.out.printf(fmt, new Object[] { method,
                                              europeanOption.NPV(),
                                              Double.NaN,
                                              Double.NaN } );

        method = "MC (Sobol)";
        europeanOption.setPricingEngine(
                    new MCEuropeanEngine(stochasticProcess,
                                         "LowDiscrepancy", timeSteps,
                                         QuantLib.nullInt(),
                                         false, false,
                                         nSamples, 0.02, maxSamples, mcSeed));
        System.out.printf(fmt, new Object[] { method,
                                              europeanOption.NPV(),
                                              Double.NaN,
                                              Double.NaN } );


        /*
        method = "MC (Longstaff Schwartz)";
        // This is the original C++ code:
        //        MakeMCAmericanEngine<PseudoRandom>().withSteps(100)
        //        .withAntitheticVariate()
        //        .withCalibrationSamples(4096)
        //        .withTolerance(0.02)
        //        .withSeed(mcSeed);

        System.out.printf(fmt, new Object[] { method,
                                              Double.NaN,
                                              Double.NaN,
                                              americanOption.NPV() });
        */

        long msecs = (System.currentTimeMillis()-beginTime);
        System.out.println("Run completed in "+msecs+" ms.");

    }
}


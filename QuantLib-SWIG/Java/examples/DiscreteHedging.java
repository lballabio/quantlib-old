/*
 Copyright (C) 2014 Felix Lee

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

/*  This example computes profit and loss of a discrete interval hedging
    strategy and compares with the results of Derman & Kamal's (Goldman Sachs
    Equity Derivatives Research) Research Note: "When You Cannot Hedge
    Continuously: The Corrections to Black-Scholes"
    http://www.ederman.com/emanuelderman/GSQSpapers/when_you_cannot_hedge.pdf

    Suppose an option hedger sells an European option and receives the
    Black-Scholes value as the options premium.
    Then he follows a Black-Scholes hedging strategy, rehedging at discrete,
    evenly spaced time intervals as the underlying stock changes. At
    expiration, the hedger delivers the option payoff to the option holder,
    and unwinds the hedge. We are interested in understanding the final
    profit or loss of this strategy.

    If the hedger had followed the exact Black-Scholes replication strategy,
    re-hedging continuously as the underlying stock evolved towards its final
    value at expiration, then, no matter what path the stock took, the final
    P&L would be exactly zero. When the replication strategy deviates from
    the exact Black-Scholes method, the final P&L may deviate from zero. This
    deviation is called the replication error. When the hedger rebalances at
    discrete rather than continuous intervals, the hedge is imperfect and the
    replication is inexact. The more often hedging occurs, the smaller the
    replication error.

    We examine the range of possibilities, computing the replication error.
*/

package examples;

import org.quantlib.Option;
import org.quantlib.PlainVanillaPayoff;
import org.quantlib.BlackCalculator;
import org.quantlib.Calendar;
import org.quantlib.TARGET;
import org.quantlib.Date;
import org.quantlib.DayCounter;
import org.quantlib.Actual365Fixed;
import org.quantlib.QuoteHandle;
import org.quantlib.SimpleQuote;
import org.quantlib.YieldTermStructureHandle;
import org.quantlib.FlatForward;
import org.quantlib.BlackVolTermStructureHandle;
import org.quantlib.BlackConstantVol;
import org.quantlib.BlackScholesMertonProcess;
import org.quantlib.PseudoRandomMT;
import org.quantlib.InvCumulativeMersenneTwisterGaussianRsg;
import org.quantlib.PathGeneratorPseudoRandom;
import org.quantlib.Path;
import org.quantlib.PathPricerPath;
import org.quantlib.Statistics;
import org.quantlib.MonteCarloModelSingleVariatePseudoRandom;

public class DiscreteHedging {
    // Link up the quantlib
    static {
        try {
            System.loadLibrary("QuantLibJNI");
        } catch (RuntimeException e) {
            e.printStackTrace();
        }
    }

    public static void main(String[] args) {
        final long startTime = System.currentTimeMillis();

        double maturity = 1.0/12.0; // 1 month
        double strike = 100.0;
        double underlying = 100.0;
        double volatility = 0.20; // 20%
        double riskFreeRate = 0.05; // 5%
        ReplicationError rp = new ReplicationError(Option.Type.Call, maturity, strike,
                                                   underlying, volatility, riskFreeRate);

        long scenarios = 50000;
        long hedgesNum;

        hedgesNum = 21;
        rp.compute(hedgesNum, scenarios);

        hedgesNum = 84;
        rp.compute(hedgesNum, scenarios);

        final long endTime = System.currentTimeMillis();
        System.out.println("\nRun completed in " + (endTime - startTime)/1000 + " s\n");
    }
}

class ReplicationError {
    private double maturity;
    private PlainVanillaPayoff payoff;
    private double s0;
    private double sigma;
    private double r;
    private double vega;

    public static final double M_PI = 3.141592653589793238462643383280;

    ReplicationError(Option.Type type,
                     double maturity,
                     double strike,
                     double s0,
                     double sigma,
                     double r) {
        this.maturity = maturity;
        this.payoff = new PlainVanillaPayoff(type, strike);
        this.s0 = s0;
        this.sigma = sigma;
        this.r = r;

        // value of the option
        double rDiscount = Math.exp(-r*maturity);
        double qDiscount = 1.0;
        double forward = s0*qDiscount/rDiscount;
        double stdDev = Math.sqrt(sigma*sigma*maturity);
        BlackCalculator black = new BlackCalculator(payoff,forward,stdDev,rDiscount);
        System.out.printf("\nOption value: %1$7.5f\n", black.value());

        // store option's vega, since Derman and Kamal's formula needs it
        vega = black.vega(maturity);

        System.out.println("");
        System.out.print("         |");
        System.out.print("          |");
        System.out.print("      P&L |");
        System.out.print("      P&L |");
        System.out.print(" Derman&Kamal |");
        System.out.println("      P&L |      P&L");

        System.out.print(" samples |");
        System.out.print("   trades |");
        System.out.print("     mean |");
        System.out.print(" std.dev. |");
        System.out.print("      formula |");
        System.out.println(" skewness | kurtosis");

        System.out.print("-------------------------");
        System.out.print("-------------------------");
        System.out.println("----------------------------");

    }

    // the actual replication error computation
    // The computation over nSamples paths of the P&L distribution
    void compute(long nTimeSteps, long nSamples) {
        // hedging interval
        // Time tau = maturity_ / nTimeSteps;

        // Black-Scholes framework: the underlying stock price evolves
        // lognormally with a fixed known volatility that stays constant
        // throughout time.
        Calendar calendar = new TARGET();
        Date today = Date.todaysDate();
        DayCounter dayCount = new Actual365Fixed();
        QuoteHandle stateVariable = new QuoteHandle(new SimpleQuote(s0));
        YieldTermStructureHandle riskFreeRate =
            new YieldTermStructureHandle(new FlatForward(today, r, dayCount));
        YieldTermStructureHandle dividendYield =
            new YieldTermStructureHandle(new FlatForward(today, 0.0, dayCount));
        BlackVolTermStructureHandle volatility =
            new BlackVolTermStructureHandle(new BlackConstantVol(today, calendar,
                                                                 sigma, dayCount));
        BlackScholesMertonProcess diffusion =
            new BlackScholesMertonProcess(stateVariable, dividendYield, riskFreeRate,
                                          volatility);

        // Black Scholes equation rules the path generator:
        // at each step the log of the stock
        // will have drift and sigma^2 variance
        InvCumulativeMersenneTwisterGaussianRsg rsg =
            PseudoRandomMT.make_sequence_generator(nTimeSteps, 0);

        boolean brownianBridge = false;

        PathGeneratorPseudoRandom myPathGenerator =
            new PathGeneratorPseudoRandom(diffusion, maturity, nTimeSteps, rsg, brownianBridge);

        // The replication strategy's Profit&Loss is computed for each path
        // of the stock. The path pricer knows how to price a path using its
        // value() method
        PathPricerPath myPathPricer =
            new ReplicationPathPricer(payoff.optionType(), payoff.strike(), r, maturity, sigma);

        // a statistics accumulator for the path-dependant Profit&Loss values
        Statistics statisticsAccumulator = new Statistics();

        MonteCarloModelSingleVariatePseudoRandom MCSimulation =
            new MonteCarloModelSingleVariatePseudoRandom(myPathGenerator,
                                                         myPathPricer,
                                                         statisticsAccumulator,
                                                         false);

        // the model simulates nSamples paths
        MCSimulation.addSamples(nSamples);

        // the sampleAccumulator method
        // gives access to all the methods of statisticsAccumulator
        double PLMean  = MCSimulation.sampleAccumulator().mean();
        double PLStDev = MCSimulation.sampleAccumulator().standardDeviation();
        double PLSkew  = MCSimulation.sampleAccumulator().skewness();
        double PLKurt  = MCSimulation.sampleAccumulator().kurtosis();

        // Derman and Kamal's formula
        double theorStD = Math.sqrt(M_PI/4/nTimeSteps)*vega*sigma;

        System.out.printf("%1$8d |", nSamples);
        System.out.printf("%1$9d |", nTimeSteps);
        System.out.printf("%1$9.3f |", PLMean);
        System.out.printf("%1$9.2f |", PLStDev);
        System.out.printf("%1$13.2f |", theorStD);
        System.out.printf("%1$9.2f |", PLSkew);
        System.out.printf("%1$9.2f\n", PLKurt);
    }
}

// The key for the MonteCarlo simulation is to have a PathPricer that
// implements a value(const Path& path) method.
// This method prices the portfolio for each Path of the random variable
class ReplicationPathPricer extends PathPricerPath {
    private Option.Type type;
    private double strike;
    private double r;
    private double maturity;
    private double sigma;

    // real constructor
    public ReplicationPathPricer(Option.Type type, double strike, double r, double maturity,
                                 double sigma) {
        this.type = type;
        this.strike = strike;
        this.r = r;
        this.maturity = maturity;
        this.sigma = sigma;
    }

    public double getValue(Path path) {

        long n = path.length()-1;

        // discrete hedging interval
        double dt = maturity/n;

        // For simplicity, we assume the stock pays no dividends.
        double stockDividendYield = 0.0;

        // let's start
        double t = 0.0;

        // stock value at t=0
        double stock = path.front();

        // money account at t=0
        double money_account = 0.0;

        /************************/
        /*** the initial deal ***/
        /************************/
        // option fair price (Black-Scholes) at t=0
        double rDiscount = Math.exp(-r*maturity);
        double qDiscount = Math.exp(-stockDividendYield*maturity);
        double forward = stock*qDiscount/rDiscount;
        double stdDev = Math.sqrt(sigma*sigma*maturity);
        PlainVanillaPayoff payoff = new PlainVanillaPayoff(type, strike);
        BlackCalculator black = new BlackCalculator(payoff, forward, stdDev, rDiscount);
        // sell the option, cash in its premium
        money_account += black.value();
        // compute delta
        double delta = black.delta(stock);
        // delta-hedge the option buying stock
        double stockAmount = delta;
        money_account -= stockAmount*stock;

        /**********************************/
        /*** hedging during option life ***/
        /**********************************/
        for (long step = 0; step < n-1; step++) {

            // time flows
            t += dt;

            // accruing on the money account
            money_account *= Math.exp(r*dt);

            // stock growth
            stock = path.value(step+1);

            // recalculate option value at the current stock value,
            // and the current time to maturity
            rDiscount = Math.exp(-r*(maturity-t));
            qDiscount = Math.exp(-stockDividendYield*(maturity-t));
            forward = stock*qDiscount/rDiscount;
            stdDev = Math.sqrt(sigma*sigma*(maturity-t));
            BlackCalculator black_recalc = new BlackCalculator(payoff, forward, stdDev, rDiscount);

            // recalculate delta
            delta = black_recalc.delta(stock);
 
            // re-hedging
            money_account -= (delta - stockAmount)*stock;
            stockAmount = delta;
        }

        /*************************/
        /*** option expiration ***/
        /*************************/
        // last accrual on my money account
        money_account *= Math.exp(r*dt);
        // last stock growth
        stock = path.value(n);

        // the hedger delivers the option payoff to the option holder
        double optionPayoff = new PlainVanillaPayoff(type, strike).getValue(stock);
        money_account -= optionPayoff;

        // and unwinds the hedge selling his stock position
        money_account += stockAmount*stock;

        // final Profit&Loss
        return money_account;
    }
}

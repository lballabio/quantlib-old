
/*
 Copyright (C) 2008 Tito Ingargiola

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

import org.quantlib.Actual365Fixed;
import org.quantlib.BlackCalculator;
import org.quantlib.BlackConstantVol;
import org.quantlib.BlackScholesMertonProcess;
import org.quantlib.BlackVolTermStructureHandle;
import org.quantlib.Calendar;
import org.quantlib.Date;
import org.quantlib.DayCounter;
import org.quantlib.FlatForward;
import org.quantlib.GaussianLowDiscrepancySequenceGenerator;
import org.quantlib.GaussianPathGenerator;
import org.quantlib.GaussianRandomSequenceGenerator;
import org.quantlib.GaussianSobolPathGenerator;
import org.quantlib.Option;
import org.quantlib.Path;
import org.quantlib.PlainVanillaPayoff;
import org.quantlib.QuoteHandle;
import org.quantlib.SamplePath;
import org.quantlib.SimpleQuote;
import org.quantlib.Statistics;
import org.quantlib.TARGET;
import org.quantlib.UniformLowDiscrepancySequenceGenerator;
import org.quantlib.UniformRandomGenerator;
import org.quantlib.UniformRandomSequenceGenerator;
import org.quantlib.YieldTermStructureHandle;

/**
 * DiscreteHedging Test app - java version of QuantLib/Examples/DiscreteHedging
 * to illustrate use of Quantlib's MonteCarlo functionality through supplied
 * SWIG interfaces.
 *
 * You need to run this with a correctly set library path and something like:
 *
 * -Djava.library.path=/usr/local/lib
 *
 * @author Tito Ingargiola
 **/
public class DiscreteHedging {

    static {  // Load QuantLib
        try { System.loadLibrary("QuantLibJNI"); }
        catch (RuntimeException e) { e.printStackTrace(); }
    }

    public static void main(String[] args) throws Exception {

        long begin = System.currentTimeMillis();

        double maturity = 1.0/12.0;   // 1 month
        double strike = 100;
        double underlying = 100;
        double volatility = 0.20; // 20%
        double riskFreeRate = 0.05; // 5%
        ReplicationError rp = new ReplicationError(Option.Type.Call, maturity,
            strike, underlying, volatility, riskFreeRate);

        long scenarios = 50000;
        long hedgesNum = 21;
        rp.compute(hedgesNum, scenarios);

        hedgesNum = 84;
        rp.compute(hedgesNum, scenarios);

        long msecs = (System.currentTimeMillis()-begin);
        System.out.println("\nRun completed in "+msecs+" ms.");
    }

    /**
     * The ReplicationError class carries out Monte Carlo simulations to
     * evaluate the outcome (the replication error) of the discrete hedging
     * strategy over different, randomly generated scenarios of future stock
     * price evolution.
     **/
    public static class ReplicationError {

        public ReplicationError(Option.Type type, double maturity,
            double strike, double s0, double sigma, double r ) {

            type_ = type;
            maturity_ = maturity;
            strike_ = strike;
            s0_ = s0;
            sigma_ = sigma;
            r_ = r;

            // value of the option
            double rDiscount = Math.exp(-r_ * maturity_);
            double qDiscount = 1.0;
            double forward = s0_ * qDiscount/rDiscount;
            double stdDev = Math.sqrt(sigma_*sigma_*maturity);
            BlackCalculator black = new BlackCalculator
                (new PlainVanillaPayoff(type,strike),forward,stdDev,rDiscount);

            System.out.printf("Option value: %2.5f \n\n",black.value());

            // store option's vega, since Derman and Kamal's formula needs it
            vega_ = black.vega(maturity_);

            String fmt ="%-8s | %-8s | %-8s | %-8s | %-12s | %-8s | %-8s \n";
            System.out.printf
                (fmt, " ", " ", "P&L", "P&L", "Derman&Kamal", "P&L","P&L" );
            System.out.printf(fmt, " samples", "trades", "mean", "std.dev",
                "formula", "skewness","kurtosis" );
            for (int i = 0; i < 78; i++) System.out.print("-");
            System.out.println("-");
        }

        void compute(long nTimeSteps, long nSamples) {
            assert nTimeSteps>0 : "the number of steps must be > 0";

            /* Black-Scholes framework: the underlying stock price evolves
               lognormally with a fixed known volatility that stays constant
               throughout time. */
            Calendar calendar = new TARGET();
            Date today = Date.todaysDate();
            DayCounter dayCounter = new Actual365Fixed();
            QuoteHandle stateVariable = new QuoteHandle(new SimpleQuote(s0_));

            YieldTermStructureHandle riskFreeRate =
                new YieldTermStructureHandle
                    (new FlatForward(today, r_, dayCounter));
            YieldTermStructureHandle dividendYield =
                new YieldTermStructureHandle
                    (new FlatForward(today, 0.0, dayCounter));
            BlackVolTermStructureHandle volatility =
                new BlackVolTermStructureHandle(
                    new BlackConstantVol(today, calendar, sigma_, dayCounter));
            BlackScholesMertonProcess diffusion =
                new BlackScholesMertonProcess
                    (stateVariable,dividendYield, riskFreeRate, volatility);

            // Black Scholes equation rules the path generator:
            // at each step the log of the stock
            // will have drift and sigma^2 variance
             boolean brownianBridge = false;
            GaussianRandomSequenceGenerator rsg =
                new GaussianRandomSequenceGenerator
                    (new UniformRandomSequenceGenerator
                        (nTimeSteps,new UniformRandomGenerator(0)));
            GaussianPathGenerator myPathGenerator =
                new GaussianPathGenerator
                    (diffusion,maturity_,nTimeSteps,rsg, brownianBridge);

            /* Alternately you can modify the MonteCarloModel to take a
             * GaussianSobolPathGenerator and uncomment these lines and
             * comment those just above
             *
            GaussianLowDiscrepancySequenceGenerator rsg =
                new GaussianLowDiscrepancySequenceGenerator
                    (new UniformLowDiscrepancySequenceGenerator
                            (nTimeSteps));
            GaussianSobolPathGenerator myPathGenerator =
                new GaussianSobolPathGenerator
                    (diffusion,maturity_,nTimeSteps,rsg, brownianBridge);*/

            ReplicationPathPricer myPathPricer = new ReplicationPathPricer
                (type_,strike_, r_, maturity_, sigma_);

            MonteCarloModel mcSimulation = new MonteCarloModel
                (myPathGenerator, myPathPricer);

            mcSimulation.addSamples(nSamples);

            // the sampleAccumulator method
            // gives access to all the methods of statisticsAccumulator
            double PLMean  = mcSimulation.sampleAccumulator().mean();
            double PLStDev =
                mcSimulation.sampleAccumulator().standardDeviation();
            double PLSkew  = mcSimulation.sampleAccumulator().skewness();
            double PLKurt  = mcSimulation.sampleAccumulator().kurtosis();

            // Derman and Kamal's formula
            double theorStD = Math.sqrt(Math.PI/4/nTimeSteps)*vega_*sigma_;

            String fmt =
                "%-8d | %-8d | %-8.3f | %-8.2f | %-12.2f | %-8.2f | %-8.2f \n";
            System.out.printf(fmt, nSamples, nTimeSteps, PLMean, PLStDev,
                theorStD, PLSkew, PLKurt );
        }

        double maturity_;
        Option.Type type_;
        double strike_;
        double s0_;
        double sigma_;
        double r_;
        double vega_;
    }

    /**
     * We pull the interface for a PathPricer into Java so we can
     * support its implementation in Java while still relying upon QuantLib's
     * powerful RNGs.
     */
    public static interface JPathPricer {

        public double price(Path path);

    }

    // The key for the MonteCarlo simulation is to have a PathPricer that
    // implements a value(const Path& path) method.
    // This method prices the portfolio for each Path of the random variable
    public static class ReplicationPathPricer implements JPathPricer {

        public ReplicationPathPricer(Option.Type type,  double strike,
            double r, double maturity, double sigma) {
            assert strike > 0     : "Strike must be positive!";
            assert maturity > 0 : "Risk free rate must be positive!";
            assert r >= 0         : "Risk free rate must be positive or Zero!";
            assert sigma >= 0     : "Volatility must be positive or Zero!";

            type_         = type;
            strike_     = strike;
            r_             = r;
            maturity_     = maturity;
            sigma_         = sigma;
        }

        public double price(Path path) {

            long n = path.length() - 1;
            assert n > 0 : "The path can't be empty!";

            // discrete hedging interval
            double dt = maturity_ / ((double)n);

            // For simplicity, we assume the stock pays no dividends.
            double stockDividendYield = 0.0;

            // let's start
            double t = 0;

            // stock value at t=0
            double stock = path.front();

            // money account at t=0
            double money_account = 0.0;

            /************************/
            /*** the initial deal ***/
            /************************/
            // option fair price (Black-Scholes) at t=0
            double rDiscount = Math.exp(-r_*maturity_);
            double qDiscount = Math.exp(-stockDividendYield*maturity_);
            double forward = stock*qDiscount/rDiscount;
            double stdDev = Math.sqrt(sigma_*sigma_*maturity_);
            PlainVanillaPayoff payoff = new PlainVanillaPayoff(type_,strike_);
            BlackCalculator black = new BlackCalculator
                (payoff,forward,stdDev,rDiscount);

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
            for (long step = 0; step < n-1; step++){

                // time flows
                t += dt;

                // accruing on the money account
                money_account *= Math.exp( r_*dt );

                // stock growth:
                stock = path.value(step+1);

                // recalculate option value at the current stock value,
                // and the current time to maturity
                rDiscount = Math.exp(-r_*(maturity_-t));
                qDiscount = Math.exp(-stockDividendYield*(maturity_-t));

                forward = stock*(qDiscount/rDiscount);

                stdDev = Math.sqrt(sigma_*sigma_*(maturity_-t));
                black = new BlackCalculator
                    (new PlainVanillaPayoff(type_,strike_),forward,stdDev,rDiscount);

                // recalculate delta
                delta = black.delta(stock);

                // re-hedging
                money_account -= (delta - stockAmount)*stock;
                stockAmount = delta;
            }

            /*************************/
            /*** option expiration ***/
            /*************************/
            // last accrual on my money account
            money_account *= Math.exp( r_*dt );
            // last stock growth
            stock = path.value(n);

            // the hedger delivers the option payoff to the option holder
            double optionPayoff =
                (new PlainVanillaPayoff(type_, strike_)).getValue(stock);
            money_account -= optionPayoff;

            // and unwinds the hedge selling his stock position
            money_account += stockAmount*stock;

            // final Profit&Loss
            return money_account;
        }

        double maturity_;
        Option.Type type_;
        double strike_;
        double sigma_;
        double r_;
    }


    /**
     * We pull the MonteCarloModel into Java so that we can enable the
     * implementation in java of our PathPricer
     */
    public static class MonteCarloModel {

        /** convenience ctor **/
        public MonteCarloModel
            (GaussianPathGenerator gpg, JPathPricer pathpricer) {
            this(gpg,pathpricer,false, null);
        }

        /** complete ctor **/
           public MonteCarloModel(GaussianPathGenerator gpg,
                JPathPricer pathpricer, boolean antitheticVariate,
                Statistics stats ) {
            assert gpg != null : "PathGenerator must not be null!";
            assert pathpricer != null : "PathPricer must not be null!";
            gpg_ = gpg;
            ppricer_ = pathpricer;
            stats_ = (stats==null) ? new Statistics() : stats;
            av_ = antitheticVariate;
        }

        public Statistics sampleAccumulator () { return stats_; }


        public void addSamples( long samples ) {
            for(long j = 0; j < samples; j++) {

                SamplePath path = gpg_.next();
                double price = ppricer_.price(path.value());
                if ( av_ ) {
                    path = gpg_.antithetic();
                    double price2 = ppricer_.price(path.value());
                    stats_.add((price+price2)/2.0, path.weight());

                } else {
                    stats_.add(price, path.weight());
                }
           }

        }
        final boolean av_;
        final GaussianPathGenerator gpg_;
        final JPathPricer ppricer_;
        final Statistics stats_;
    }
}


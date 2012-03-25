/*
 Copyright (C) 2012 Klaus Spanderen


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

import org.quantlib.{Array => QArray, _}

object HestonMonteCarlo {
    def main(args: Array[String]) : Unit = {

        try {
            System.loadLibrary("QuantLibJNI")
        } 
        catch {
            case ex: UnsatisfiedLinkError => {
                  println("please check your LD_LIBRARY_PATH variable")
                throw ex
            }
        }

        val dayCounter     = new ActualActual();
        val settlementDate = new Date(27, Month.December, 2004)
        Settings.instance setEvaluationDate settlementDate

        val exerciseDate = new Date(28, Month.March, 2005)
        val maturity = dayCounter.yearFraction(settlementDate, exerciseDate)

        val payoff = new PlainVanillaPayoff(Option.Type.Call, 1.05)
        val exercise = new EuropeanExercise(exerciseDate)
        val vanillaOption = new VanillaOption(payoff, exercise)

        val rTS = new YieldTermStructureHandle(
            new FlatForward(settlementDate, 0.0225, dayCounter))

        val divTS = new YieldTermStructureHandle(
            new FlatForward(settlementDate, 0.02, dayCounter))
       
        val s0 = new QuoteHandle(new SimpleQuote(1.0))

        val v0    =  0.1
        val kappa =  3.16
        val theta =  0.09
        val sigma =  0.4
        val rho   = -0.2

        val hestonProcess = new HestonProcess(rTS, divTS, s0, v0, 
                                              kappa, theta, sigma, rho)

        val hestonModel = new HestonModel(hestonProcess)
        val analyticEngine = new AnalyticHestonEngine(hestonModel)
        vanillaOption.setPricingEngine(analyticEngine)

        val timeSteps = 10
        val grsg = new GaussianRandomSequenceGenerator(
            new UniformRandomSequenceGenerator(
                2*timeSteps, new UniformRandomGenerator(1234)))
        
        val grid = new TimeGrid(maturity, timeSteps)
        val times = new DoubleVector()
        (0L until grid.getSize).foreach(i => times add(grid elementAt i))
        val gen = new GaussianMultiPathGenerator(hestonProcess, times, grsg) 

        val stat = new IncrementalStatistics()
        for (i <- (0 until 50000)) {
            stat.add(0.5*( payoff.getValue(gen.next.value at 0 back)
                          +payoff.getValue(gen.antithetic.value at 0 back())))
        }

        printf("Semi-Analytic: %f\n", vanillaOption.NPV())
        printf("Monte-Carlo  : %f +/-%f\n", stat.mean(), stat.errorEstimate()) 
    }
}
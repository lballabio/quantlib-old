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

object HestonModelCalibration {
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

        val calendar       = new TARGET();
        val dayCounter     = new Actual365Fixed();
        val settlementDate = new Date(5, Month.July, 2002);
        Settings.instance setEvaluationDate settlementDate
     
        val rates = new DoubleVector()
        List(0.0357,0.0357,0.0349,0.0341,0.0355,
             0.0359,0.0368,0.0386,0.0401).foreach(i => rates.add(i))
             
        val dates = new DateVector()
        val maturityInDays = Vector(0, 13, 41, 75, 165, 256, 345, 524, 703)     
        maturityInDays.foreach(i => dates.add(settlementDate.add(i)))
        
        val rTS = new YieldTermStructureHandle(
            new ZeroCurve(dates, rates, dayCounter))
       
        val divTS = new YieldTermStructureHandle(
            new FlatForward(settlementDate, 0.0, dayCounter))
       
        val s0 = new QuoteHandle(new SimpleQuote(4468.17))
        
        val vols = Vector(
            0.6625,0.4875,0.4204,0.3667,0.3431,0.3267,0.3121,0.3121,
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
            0.3976,0.2860,0.2607,0.2356,0.2297,0.2268,0.2241,0.2320 )
            
        val strikes = Vector( 
            3400,3600,3800,4000,4200,4400,4500,4600,4800,5000,5200,5400,5600)
            
        val calibrationHelpers = new CalibrationHelperVector()    
            
        for (s <- (0 to 12)) {
            for (m <- (1 to 8)) {
                val vol = new QuoteHandle(new SimpleQuote(vols(s*8+m-1)))
                val maturityInWeeks = ((maturityInDays(m)+3)/7.).toInt
                val maturity = new Period(maturityInWeeks, TimeUnit.Weeks)

                calibrationHelpers.add(
                    new HestonModelHelper(maturity, calendar, s0.value, 
                                          strikes(s), vol, rTS, divTS, 
                                          CalibrationHelper.ImpliedVolError))
            }
        }
        
        val helpers = for (i <- 0 until calibrationHelpers.size().toInt)
            yield(calibrationHelpers.get(i))
        
        val v0   =  0.1
        val kappa=  1.0
        val theta=  0.1
        val sigma=  0.5
        val rho  = -0.5

        val hestonProcess = new HestonProcess(rTS, divTS, s0, v0, 
                                              kappa, theta, sigma, rho)
        
        val hestonModel = new HestonModel(hestonProcess) 
        val analyticHestonEngine = new AnalyticHestonEngine(hestonModel, 64);
        
        helpers.foreach(h => h setPricingEngine analyticHestonEngine)
        
        val optimizerMethod = new LevenbergMarquardt(1e-8, 1e-8, 1e-8)
        hestonModel.calibrate(calibrationHelpers, optimizerMethod, 
                              new EndCriteria(400, 40, 1.0e-8, 1.0e-8, 1.0e-8))
                              
        val sse = helpers map {h => 100*h.calibrationError} map {e => e*e} sum;
        printf("error=  %f\n",sse)
        
        val params = hestonModel.params
        printf("v0   =  %f\n",params.get(4))
        printf("kappa=  %f\n",params.get(1))
        printf("theta=  %f\n",params.get(0))
        printf("sigma=  %f\n",params.get(2))
        printf("rho  = %f\n", params.get(3))
    }
}
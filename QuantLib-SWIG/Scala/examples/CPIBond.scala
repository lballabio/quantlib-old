/*
 Copyright (C) 2014 Klaus Spanderen


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

import org.quantlib.Month._
import org.quantlib.{Array => QArray, _}

case class Datum(date: Date, rate: Double)

object CPIBond {
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
        
        doIt
     }

     def doIt() = {
        val startTime = System.currentTimeMillis

        val calendar = new UnitedKingdom()
        val dayCounter = new ActualActual();
        val convention = BusinessDayConvention.ModifiedFollowing
        
        val today = new Date(25, November, 2009)
        val evaluationDate = calendar.adjust(today)
        Settings.instance().setEvaluationDate(evaluationDate)        
        
        val yTS = new YieldTermStructureHandle(
                    new FlatForward(evaluationDate, 0.05, dayCounter))

        val from = new Date(20, July, 2007);
        val to   = new Date(20, November, 2009);
        val tenor = new Period(1, TimeUnit.Months)
        val rpiSchedule = new Schedule(from, to, tenor, calendar, 
                                       convention, convention,
                                       DateGeneration.Rule.Backward, false)
        
        val cpiTS = new RelinkableZeroInflationTermStructureHandle
        val inflationIndex = new UKRPI(false, cpiTS)
        val fixData = Array(206.1, 207.3, 208.0, 208.9, 209.7, 210.9,
                            209.8, 211.4, 212.1, 214.0, 215.1, 216.8,
                            216.5, 217.2, 218.4, 217.7, 216,
                            212.9, 210.1, 211.4, 211.3, 211.5,
                            212.8, 213.4, 213.4, 213.4, 214.4)
        
        for (i <- 0 until fixData.size) {
          inflationIndex.addFixing(rpiSchedule.date(i), fixData(i)) 
        }
        
        val observationLag = new Period(2, TimeUnit.Months)
        val zciisData = Array(
          Datum(new Date(25, November, 2010), 3.0495 ),
          Datum(new Date(25, November, 2011), 2.93 ),
          Datum(new Date(26, November, 2012), 2.9795 ),
          Datum(new Date(25, November, 2013), 3.029 ),
          Datum(new Date(25, November, 2014), 3.1425 ),
          Datum(new Date(25, November, 2015), 3.211 ),
          Datum(new Date(25, November, 2016), 3.2675 ),
          Datum(new Date(25, November, 2017), 3.3625 ),
          Datum(new Date(25, November, 2018), 3.405 ),
          Datum(new Date(25, November, 2019), 3.48 ),
          Datum(new Date(25, November, 2021), 3.576 ),
          Datum(new Date(25, November, 2024), 3.649 ),
          Datum(new Date(26, November, 2029), 3.751 ),
          Datum(new Date(27, November, 2034), 3.77225),
          Datum(new Date(25, November, 2039), 3.77 ),
          Datum(new Date(25, November, 2049), 3.734 ),
          Datum(new Date(25, November, 2059), 3.714 )
        )
        
        val zeroSwapHelpers = new ZeroHelperVector
        zciisData map { datum => zeroSwapHelpers add 
          new ZeroCouponInflationSwapHelper(
            datum.rate/100d, observationLag,
            datum.date, calendar, convention, dayCounter, inflationIndex) }

        cpiTS linkTo new PiecewiseZeroInflation(          
          evaluationDate, calendar, dayCounter, observationLag, 
          inflationIndex.frequency, inflationIndex.interpolated, 
          zciisData(0).rate/100d,
          yTS, zeroSwapHelpers, 1.0e-12, new Linear)
          
        val notional = 1000000d;
        
        val fixedRates = new DoubleVector()
        fixedRates add 0.1    
        
        val fixedDayCounter = new Actual365Fixed
        val fixedPaymentConvention = BusinessDayConvention.ModifiedFollowing
        val fixedPaymentCalendar = new UnitedKingdom
	val contractObservationLag = new Period(3, TimeUnit.Months)
        val observationInterpolation = CPI.InterpolationType.Flat
        val settlementDays = 3
        val growthOnly = true
        
        val baseCPI = 206.1
        val startDate = new Date(2, October, 2007)
        val endDate = new Date(2, October, 2052)
        
        val fixedSchedule = new Schedule(startDate, endDate, 
          new Period(6, TimeUnit.Months), fixedPaymentCalendar,
          BusinessDayConvention.Unadjusted,
          BusinessDayConvention.Unadjusted,
          DateGeneration.Rule.Backward, false)
          
        val bond = new CPIBond(settlementDays, notional, growthOnly,
                               baseCPI, contractObservationLag,
                               inflationIndex, observationInterpolation,
                               fixedSchedule, fixedRates, fixedDayCounter, 
                               fixedPaymentConvention, new Date)
                               
        bond setPricingEngine new DiscountingBondEngine(yTS)
        println("clean price: " + bond.cleanPrice + " \t exec time: " + 
          (System.currentTimeMillis - startTime)/1000d)
          
        cpiTS linkTo new ZeroInflationTermStructure  // break cylic reference between
                                                     // cpiTS, zeroSwapHelpers and inflationIndex
    }
}
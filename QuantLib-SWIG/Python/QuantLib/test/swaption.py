"""
 Copyright (C) 2000, 2001, 2002 RiskMap srl

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it under the
 terms of the QuantLib license.  You should have received a copy of the
 license along with this program; if not, please email ferdinando@ametrano.net
 The license is also available online at http://quantlib.org/html/license.html

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
"""

# $Id$


from QuantLib import *
import unittest

class SwaptionTest(unittest.TestCase):
    def setUp(self):
        self.today = Date_todaysDate()
        self.termStructure = TermStructureHandle()
        self.nominal = 100.0
        self.rollingConvention = 'modifiedFollowing'
        self.fixedFrequency = 1; self.floatingFrequency = 2
        self.fixedDayCount = DayCounter('30/360'); self.fixedAdj = 0
        self.index = Xibor('Euribor',12/self.floatingFrequency,'Months',
                           self.termStructure)
        self.calendar = self.index.calendar()
        self.settlementDays = 2
        self.settlement = self.calendar.advance(self.today,
                                                self.settlementDays,"days",
                                               'following')
        self.fixingDays = 2
        self.termStructure.linkTo(FlatForward(self.settlement,
                                              0.05,DayCounter('Act/365')))
        self.cases = [ (exercise,length,payFixed)
                        for exercise in [1, 2, 3, 5, 7, 10]
                        for length in [1, 2, 3, 5, 7, 10, 15, 20]
                        for payFixed in [0,1] ]
    def makeSwap(self,startDate,length,fixedRate,spread,payFixed):
        return SimpleSwap(payFixed, startDate, length, 'years',
            self.calendar, self.rollingConvention, self.nominal,
            self.fixedFrequency, fixedRate, self.fixedAdj, self.fixedDayCount,
            self.floatingFrequency, self.index, self.fixingDays, spread,
            self.termStructure)
    def makeSwaption(self,swap,exerciseDate,volatility):
        return Swaption(swap,
                        EuropeanExercise(exerciseDate),
                        self.termStructure,
                        BlackSwaptionEngine(
                            BlackModel(
                                MarketElementHandle(
                                    SimpleMarketElement(volatility)),
                                self.termStructure)))
    # check 1
    def testStrikeDependency(self):
        "Testing swaption dependency on strike"
        for (exercise,length,payFixed) in self.cases:
            exerciseDate = self.calendar.roll(self.today.plusYears(exercise))
            startDate = self.calendar.advance(exerciseDate,
                                              self.settlementDays,'days')
            strikes = [0.03, 0.04, 0.05, 0.06, 0.07]
            values = []
            for s in strikes:
                swap = self.makeSwap(startDate,length,s,0.0,payFixed)
                swaption = self.makeSwaption(swap,exerciseDate,0.20)
                values.append(swaption.NPV())

            if payFixed:
                # NPV must decrease with strike
                for i in range(1,len(values)):
                    if values[i] > values[i-1]:
                        self.fail("""
NPV is increasing with the strike in a payer swaption:
    exercise date: %s
    length: %d years
    value: %8.4f at strike: %8.4f
    value: %8.4f at strike: %8.4f
                                  """ % (exercise,length,
                                         values[i-1],strikes[i-1]*100,
                                         values[i],  strikes[i]*100))
            else:
                # NPV must increase with strike
                for i in range(1,len(values)):
                    if values[i] < values[i-1]:
                        self.fail("""
NPV is decreasing with the strike in a receiver swaption:
    exercise date: %s
    length: %d years
    value: %8.4f at strike: %8.4f
    value: %8.4f at strike: %8.4f
                                  """ % (exercise,length,
                                         values[i-1],strikes[i-1]*100,
                                         values[i],  strikes[i]*100))

    # check 2
    def testSpreadDependency(self):
        "Testing swaption dependency on spread"
        for (exercise,length,payFixed) in self.cases:
            exerciseDate = self.calendar.roll(self.today.plusYears(exercise))
            startDate = self.calendar.advance(exerciseDate,
                                              self.settlementDays,'days')
            spreads = [-0.002, -0.001, 0.0, 0.001, 0.002]
            values = []
            for s in spreads:
                swap = self.makeSwap(startDate,length,0.06,s,payFixed)
                swaption = self.makeSwaption(swap,exerciseDate,0.20)
                values.append(swaption.NPV())

            if payFixed:
                # NPV must increase with spread
                for i in range(1,len(values)):
                    if values[i] < values[i-1]:
                        self.fail("""
NPV is decreasing with the spread in a payer swaption:
    exercise date: %s
    length: %d years
    value: %8.4f for spread: %8.4f
    value: %8.4f for spread: %8.4f
                                  """ % (exercise,length,
                                         values[i-1],spreads[i-1]*100,
                                         values[i],  spreads[i]*100))
            else:
                # NPV must decrease with spread
                for i in range(1,len(values)):
                    if values[i] > values[i-1]:
                        self.fail("""
NPV is increasing with the spread in a receiver swaption:
    exercise date: %s
    length: %d years
    value: %8.4f for spread: %8.4f
    value: %8.4f for spread: %8.4f
                                  """ % (exercise,length,
                                         values[i-1],spreads[i-1]*100,
                                         values[i],  spreads[i]*100))

    # check 3
    def testSpreadTreatment(self):
        "Testing swaption treatment of spread"
        for (exercise,length,payFixed) in self.cases:
            exerciseDate = self.calendar.roll(self.today.plusYears(exercise))
            startDate = self.calendar.advance(exerciseDate,
                                              self.settlementDays,'days')
            spreads = [-0.002, -0.001, 0.0, 0.001, 0.002]
            for s in spreads:
                swap = self.makeSwap(startDate,length,0.06,s,payFixed)
                correction = s*swap.floatingLegBPS()/swap.fixedLegBPS()
                equivalentSwap = self.makeSwap(startDate,length,
                                               0.06+correction,0.0,payFixed)
                swaption1 = self.makeSwaption(swap,
                                              exerciseDate,
                                              0.20)
                swaption2 = self.makeSwaption(equivalentSwap,
                                              exerciseDate,
                                              0.20)
                if abs(swaption1.NPV()-swaption2.NPV()) > 1.0e-10:
                    self.fail("""
wrong spread treatment:
    exercise date: %s
    length: %d years
    pay fixed: %d
    spread: %8.4f
    value:                        %8.4f
    value of equivalent swaption: %8.4f
                              """ % (exercise,length,payFixed,
                                     s*100,
                                     swaption1.NPV(),
                                     swaption2.NPV()))

    # check 4
    def testCachedValue(self):
        "Testing swaption value against cached value"
        cachedSettlement = Date(15,3,2002)
        self.termStructure.linkTo(FlatForward(cachedSettlement,
                                              0.05,DayCounter('Act/365')))
        exerciseDate = self.calendar.roll(cachedSettlement.plusYears(5))
        startDate = self.calendar.advance(exerciseDate,
                                          self.settlementDays,'days')
        swap = self.makeSwap(startDate,10,0.06,0.0,1)
        swaption = self.makeSwaption(swap,exerciseDate,0.20)
        cachedNPV = 3.645305998559
        if abs(swaption.NPV()-cachedNPV) > 1.0e-11:
            self.fail("""
failed to reproduce cached value:
    calculated: %18.12f
    expected:   %18.12f
                      """ % (swaption.NPV(),cachedNPV))


if __name__ == '__main__':
    import QuantLib
    print 'testing QuantLib', QuantLib.__version__
    suite = unittest.TestSuite()
    suite.addTest(unittest.makeSuite(SwaptionTest,'test'))
    unittest.TextTestRunner(verbosity=2).run(suite)
    raw_input('press return to continue')

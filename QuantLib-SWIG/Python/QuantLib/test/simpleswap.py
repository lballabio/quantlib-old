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

__version__ = "$Revision$"
# $Source$

from QuantLib import *
import unittest

class SimpleSwapTest(unittest.TestCase):
    def setUp(self):
        self.payFixed = 1
        today = Date_todaysDate()
        self.settlementDays = 2
        self.fixingDays = 2
        self.nominal = 100
        self.rollingConvention = 'modifiedFollowing'
        self.fixedFrequency = 1; self.floatingFrequency = 2
        self.fixedDayCount = DayCounter('30/360'); self.fixedAdj = 0
        self.euriborHandle = TermStructureHandle()
        self.index = Xibor("Euribor",
                           12/self.floatingFrequency,'Months',
                           self.euriborHandle)
        self.calendar = self.index.calendar()
        self.settlement = self.calendar.advance(today,
                                                self.settlementDays, "days",
                                                "following")
        termStructure = FlatForward(self.settlement, 0.05,
                                    DayCounter('Act/365'))
        self.euriborHandle.linkTo(termStructure)
    def makeSwap(self,length,fixedRate,floatingSpread):
        return SimpleSwap(self.payFixed, self.settlement, length, 'years',
                          self.calendar, self.rollingConvention, self.nominal,
                          self.fixedFrequency, fixedRate, self.fixedAdj,
                          self.fixedDayCount, self.floatingFrequency,
                          self.index, self.fixingDays, floatingSpread,
                          self.euriborHandle)
    def testFairRate(self):
        "Testing simple swap calculation of fair fixed rate"
        fixedRate = 0.0
        cases = [ (length, spread)
                  for length in [1, 2, 5, 10, 20]
                  for spread in [-0.001, -0.01, 0, 0.01, 0.001] ]

        for (length, spread) in cases:
            swap = self.makeSwap(length, fixedRate, spread)
            swap = self.makeSwap(length, swap.fairRate(), spread)
            if not (abs(swap.NPV()) <= 1e-10):
                self.fail("""
recalculating with implied rate:
    calculated value: %(NPV)g
    expected value:   0.0
                          """ % swap.NPV())
    def testFairSpread(self):
        "Testing simple swap calculation of fair floating spread"
        spread = 0.0
        cases = [ (length, rate)
                  for length in [1, 2, 5, 10, 20]
                  for rate in [0.04, 0.05, 0.06, 0.07] ]

        for (length,rate) in cases:
            swap = self.makeSwap(length, rate, spread)
            swap = self.makeSwap(length, rate, swap.fairSpread())
            if not (abs(swap.NPV()) <= 1e-10):
                self.fail("""
recalculating with implied spread:
    calculated value: %(NPV)g
    expected value:   0.0
                          """ % swap.NPV())
    def testRateDependency(self):
        "Testing simple swap dependency on fixed rate"
        cases = [ (length, spread)
                  for length in [1, 2, 5, 10, 20]
                  for spread in [-0.001, -0.01, 0, 0.01, 0.001] ]
        rates = [0.03, 0.04, 0.05, 0.06, 0.07]
        
        for (length, spread) in cases:
            values = []
            for r in rates:
                swap = self.makeSwap(length,r,spread)
                values.append(swap.NPV())
            # We're paying fixed - NPV must decrease with rate
            for i in range(1,len(values)):
                if values[i] >= values[i-1]:
                    self.fail("""
NPV is increasing with the fixed rate in a simple swap paying fixed:
    length: %d years
    value: %8.4f paying rate: %8.4f
    value: %8.4f paying rate: %8.4f
                              """ % (length,
                                     values[i-1],rates[i-1]*100,
                                     values[i],  rates[i]*100))
    def testSpreadDependency(self):
        "Testing simple swap dependency on floating spread"
        cases = [ (length, rate)
                  for length in [1, 2, 5, 10, 20]
                  for rate in [0.04, 0.05, 0.06, 0.07] ]
        spreads = [-0.01, -0.002, -0.001, 0, 0.001, 0.002, 0.01]

        for (length, rate) in cases:
            values = []
            for s in spreads:
                swap = self.makeSwap(length,rate,s)
                values.append(swap.NPV())
            # We're paying fixed - NPV must increase with spread
            for i in range(1,len(values)):
                if values[i] <= values[i-1]:
                    self.fail("""
NPV is decreasing with the spread in a simple swap paying fixed:
    length: %d years
    value: %8.4f receiving spread: %8.4f
    value: %8.4f receiving spread: %8.4f
                              """ % (length,
                                     values[i-1],spreads[i-1]*100,
                                     values[i],  spreads[i]*100))
    def testCachedValue(self):
        "Testing simple swap calculation against cached value"
        today = Date(17,6,2002)
        self.settlement = self.calendar.advance(today,
                                                self.settlementDays, "days",
                                                "following")
        termStructure = FlatForward(self.settlement, 0.05,
                                    DayCounter('Act/365'))
        self.euriborHandle.linkTo(termStructure)

        swap = self.makeSwap(10,0.06,0.001)
        cachedNPV = -5.883663676727
        if abs(swap.NPV()-cachedNPV) > 1.0e-11:
            self.fail("""
failed to reproduce cached simple swap value:
    calculated: %18.12f
    expected:   %18.12f
                      """ % (swap.NPV(),cachedNPV))



if __name__ == '__main__':
    import QuantLib
    print 'testing QuantLib', QuantLib.__version__
    suite = unittest.TestSuite()
    suite.addTest(unittest.makeSuite(SimpleSwapTest,'test'))
    unittest.TextTestRunner(verbosity=2).run(suite)
    raw_input('press return to continue')

"""
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl

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
# $Id$

from QuantLib import *
import unittest

class PiecewiseFlatForwardTest(unittest.TestCase):
    def runTest(self):
        "Testing piecewise flat forward curve"
        euriborHandle = TermStructureHandle()
        calendar = Calendar('TARGET')
        today = Date_todaysDate()
        settlementDays = 2
        settlement = calendar.advance(today,
                                      settlementDays,"days",
                                      'following')
        fixingDays = 2
        # deposits
        rollingConvention = 'ModifiedFollowing'
        dayCounter = DayCounter('Act/360')
        depositData = [
            (1,   'week', 4.559),
            (1,  'month', 4.581),
            (2, 'months', 4.573),
            (3, 'months', 4.557),
            (6, 'months', 4.496),
            (9, 'months', 4.490)
        ]
        deposits = [
            DepositRateHelper(
                MarketElementHandle(SimpleMarketElement(rate/100)),
                n, units, settlementDays,
                calendar, rollingConvention, dayCounter)
            for (n,units,rate) in depositData
        ]
        # swaps
        swapRollingConvention = 'modifiedFollowing'
        fixedFrequency = 1
        fixedIsAdjusted = 0
        fixedDayCount = DayCounter('30/360')
        floatingFrequency = 2
        swapData = [
            ( 1, 4.54),
            ( 2, 4.63),
            ( 3, 4.75),
            ( 4, 4.86),
            ( 5, 4.99),
            ( 6, 5.11),
            ( 7, 5.23),
            ( 8, 5.33),
            ( 9, 5.41),
            (10, 5.47),
            (12, 5.60),
            (15, 5.75),
            (20, 5.89),
            (25, 5.95),
            (30, 5.96)
        ]
        swaps = [
            SwapRateHelper(
                MarketElementHandle(SimpleMarketElement(rate/100)),
                years, "years", settlementDays,
                calendar, swapRollingConvention, fixedFrequency,
                fixedIsAdjusted, fixedDayCount, floatingFrequency)
            for (years,rate) in swapData
        ]
        # all instruments
        instruments = deposits + swaps
        # instantiate curve
        termStructure = PiecewiseFlatForward(today,settlement,instruments,
                                             DayCounter('Act/360'))
        euriborHandle.linkTo(termStructure)
        # check deposits
        for (n,units,expectedRate) in depositData:
            expectedRate /= 100
            index = Xibor("Euribor",n,units, euriborHandle)
            estimatedRate = index.fixing(today)
            if not (abs(estimatedRate - expectedRate) <= 1.0e-9):
                self.fail("""
%(n)d %(units)s deposit:
    estimated rate: %(estimatedRate)12.10f
    input rate:     %(expectedRate)12.10f
    tolerance exceeded
                          """ % locals())

        # check swaps
        index = Xibor("Euribor",12/floatingFrequency,'Months', euriborHandle)
        for (years,expectedRate) in swapData:
            expectedRate /= 100
            swap = SimpleSwap(1,settlement,years,'years',
                              calendar,swapRollingConvention,100.0,
                              fixedFrequency,0.0,fixedIsAdjusted,
                              fixedDayCount,floatingFrequency,index,
                              fixingDays,0.0, euriborHandle)
            estimatedRate = swap.fairRate()
            if not (abs(estimatedRate - expectedRate) <= 1.0e-9):
                self.fail("""
%(years)d years swap:
    estimated rate: %(estimatedRate)12.10f
    input rate:     %(expectedRate)12.10f
    tolerance exceeded
	                      """ % locals())

if __name__ == '__main__':
    import QuantLib
    print 'testing QuantLib', QuantLib.__version__
    suite = unittest.TestSuite()
    suite.addTest(PiecewiseFlatForwardTest())
    unittest.TextTestRunner(verbosity=2).run(suite)
    raw_input('press return to continue')

"""
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it under the
 terms of the QuantLib license.  You should have received a copy of the
 license along with this program; if not, please email quantlib-dev@lists.sf.net
 The license is also available online at http://quantlib.org/html/license.html

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
"""

from QuantLib import *
import unittest

flag = None
def raiseFlag():
    global flag
    flag = 1

class TermStructureTest(unittest.TestCase):
    def setUp(self):
        self.calendar = TARGET()
        today = self.calendar.adjust(Date_todaysDate())
        self.settlementDays = 2
        settlement = self.calendar.advance(today,self.settlementDays,'days')
        deposits = [
            DepositRateHelper(
                QuoteHandle(SimpleQuote(rate/100)),
                n, units, self.settlementDays,
                self.calendar, 'mf', Actual360())
            for (n,units,rate) in [ (1,  'month', 4.581),
                                    (2, 'months', 4.573),
                                    (3, 'months', 4.557),
                                    (6, 'months', 4.496),
                                    (9, 'months', 4.490) ]
        ]
        swaps = [
            SwapRateHelper(
                QuoteHandle(SimpleQuote(rate/100)),
                years, "years", self.settlementDays,
                self.calendar, 1, 'unadjusted', Thirty360(),
                2, 'mf')
            for (years,rate) in [ ( 1, 4.54),
                                  ( 5, 4.99),
                                  (10, 5.47),
                                  (20, 5.89),
                                  (30, 5.96) ]
        ]

        self.termStructure = PiecewiseFlatForward(settlement,
                                                  deposits+swaps,
                                                  Actual360())

    def testImpliedObs(self):
        "Testing observability of implied term structure"
        global flag
        flag = None
        h = YieldTermStructureHandle()
        settlement = self.termStructure.referenceDate()
        new_settlement = self.calendar.advance(settlement,3,'years')
        implied = ImpliedTermStructure(h,new_settlement)
        obs = Observer(raiseFlag)
        obs.registerWith(implied)
        h.linkTo(self.termStructure)
        if not flag:
            self.fail("Observer was not notified of term structure change")
    def testFSpreadedObs(self):
        "Testing observability of forward-spreaded term structure"
        global flag
        flag = None
        me = SimpleQuote(0.01)
        mh = QuoteHandle(me)
        h = YieldTermStructureHandle()
        spreaded = ForwardSpreadedTermStructure(h,mh)
        obs = Observer(raiseFlag)
        obs.registerWith(spreaded)
        h.linkTo(self.termStructure)
        if not flag:
            self.fail("Observer was not notified of term structure change")
        flag = None
        me.setValue(0.005)
        if not flag:
            self.fail("Observer was not notified of spread change")
    def testZSpreadedObs(self):
        "Testing observability of zero-spreaded term structure"
        global flag
        flag = None
        me = SimpleQuote(0.01)
        mh = QuoteHandle(me)
        h = YieldTermStructureHandle()
        spreaded = ZeroSpreadedTermStructure(h,mh)
        obs = Observer(raiseFlag)
        obs.registerWith(spreaded)
        h.linkTo(self.termStructure)
        if not flag:
            self.fail("Observer was not notified of term structure change")
        flag = None
        me.setValue(0.005)
        if not flag:
            self.fail("Observer was not notified of spread change")


if __name__ == '__main__':
    import QuantLib
    print 'testing QuantLib', QuantLib.__version__
    suite = unittest.TestSuite()
    suite.addTest(unittest.makeSuite(TermStructureTest,'test'))
    unittest.TextTestRunner(verbosity=2).run(suite)
    raw_input('press return to continue')


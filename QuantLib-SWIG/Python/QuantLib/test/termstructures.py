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
# $Id$

from QuantLib import *
import unittest

flag = None
def raiseFlag():
    global flag
    flag = 1

class TermStructureTest(unittest.TestCase):
    def setUp(self):
        today = Date_todaysDate()
        calendar = Calendar('TARGET')
        settlementDays = 2
        settlement = calendar.advance(today,settlementDays,'days')
        deposits = [
            DepositRateHelper(
                MarketElementHandle(SimpleMarketElement(rate/100)),
                settlementDays, n, units,
                calendar, 'mf', DayCounter('act/360'))
            for (n,units,rate) in [ (1,  'month', 4.581),
                                    (2, 'months', 4.573),
                                    (3, 'months', 4.557),
                                    (6, 'months', 4.496),
                                    (9, 'months', 4.490) ]
        ]
        swaps = [
            SwapRateHelper(
                MarketElementHandle(SimpleMarketElement(rate/100)),
                settlementDays, years, calendar,
                'mf', 1, 0, DayCounter('30/360'), 2)
            for (years,rate) in [ ( 1, 4.54),
                                  ( 5, 4.99),
                                  (10, 5.47),
                                  (20, 5.89),
                                  (30, 5.96) ]
        ]        
        
        self.termStructure = PiecewiseFlatForward(today,settlement,
                                                  deposits+swaps,
                                                  DayCounter('Act/360'))
    def testImplied(self):
        "Testing consistency of implied term structure"
        tolerance = 1.0e-10
        h = TermStructureHandle(self.termStructure)
        new_today = self.termStructure.todaysDate().plusYears(3)
        new_settlement = Calendar('TARGET').advance(new_today,2,'days')
        test_date = new_settlement.plusYears(5)
        implied = ImpliedTermStructure(h,new_today,new_settlement)
        base_discount = self.termStructure.discount(new_settlement)
        discount = self.termStructure.discount(test_date)
        implied_discount = implied.discount(test_date)
        if abs(discount - base_discount*implied_discount) > tolerance:
            self.fail("""
unable to reproduce discount from implied curve
    calculated: %.10f
    expected:   %.10f
                      """ % (base_discount*implied_discount, discount))
    def testImpliedObs(self):
        "Testing observability of implied term structure"
        global flag
        flag = None
        h = TermStructureHandle()
        new_today = self.termStructure.todaysDate().plusYears(3)
        new_settlement = Calendar('TARGET').advance(new_today,2,'days')
        implied = ImpliedTermStructure(h,new_today,new_settlement)
        obs = Observer(raiseFlag)
        obs.registerWith(implied)
        h.linkTo(self.termStructure)
        if not flag:
            self.fail("Observer was not notified of term structure change")
    def testFSpreaded(self):
        "Testing consistency of forward-spreaded term structure"
        tolerance = 1.0e-10
        me = SimpleMarketElement(0.01)
        mh = MarketElementHandle(me)
        h = TermStructureHandle(self.termStructure)
        spreaded = ForwardSpreadedTermStructure(h,mh)
        test_date = self.termStructure.todaysDate().plusYears(5)
        forward = self.termStructure.instantaneousForward(test_date)
        spreaded_forward = spreaded.instantaneousForward(test_date)
        if abs((forward+me.value())-spreaded_forward) > tolerance:
            self.fail("""
unable to reproduce forward from spreaded curve
    calculated: %.10f
    expected:   %.10f
                      """ % (spreaded_forward-me.value(), forward))
    def testFSpreadedObs(self):
        "Testing observability of forward-spreaded term structure"
        global flag
        flag = None
        me = SimpleMarketElement(0.01)
        mh = MarketElementHandle(me)
        h = TermStructureHandle()
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
    def testZSpreaded(self):
        "Testing consistency of zero-spreaded term structure"
        tolerance = 1.0e-10
        me = SimpleMarketElement(0.01)
        mh = MarketElementHandle(me)
        h = TermStructureHandle(self.termStructure)
        spreaded = ZeroSpreadedTermStructure(h,mh)
        test_date = self.termStructure.todaysDate().plusYears(5)
        zero = self.termStructure.zeroYield(test_date)
        spreaded_zero = spreaded.zeroYield(test_date)
        if abs((zero+me.value())-spreaded_zero) > tolerance:
            self.fail("""
unable to reproduce zero yield from spreaded curve
    calculated: %.10f
    expected:   %.10f
                      """ % (spreaded_zero-me.value(), zero))
    def testZSpreadedObs(self):
        "Testing observability of zero-spreaded term structure"
        global flag
        flag = None
        me = SimpleMarketElement(0.01)
        mh = MarketElementHandle(me)
        h = TermStructureHandle()
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

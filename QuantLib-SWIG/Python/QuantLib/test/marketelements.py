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

import QuantLib
import unittest

flag = None
def raiseFlag():
    global flag
    flag = 1

class MarketElementTest(unittest.TestCase):
    def testObservable(self):
        "Testing observability of market elements"
        global flag
        flag = None
        me = QuantLib.SimpleMarketElement(0.0)
        obs = QuantLib.Observer(raiseFlag)
        obs.registerWith(me)
        me.setValue(3.14)
        if not flag:
            self.fail("Observer was not notified of market element change")
    def testObservableHandle(self):
        "Testing observability of market element handles"
        global flag
        flag = None
        me1 = QuantLib.SimpleMarketElement(0.0)
        h = QuantLib.MarketElementHandle(me1)
        obs = QuantLib.Observer(raiseFlag)
        obs.registerWith(h)
        me1.setValue(3.14)
        if not flag:
            self.fail("Observer was not notified of market element change")
        flag = None
        me2 = QuantLib.SimpleMarketElement(0.0)
        h.linkTo(me2)
        if not flag:
            self.fail("Observer was not notified of market element change")
    def testDerived(self):
        "Testing derived market elements"
        for f in [lambda x: x + 10,
                  lambda x: x * 10,
                  lambda x: x - 10]:
            me = QuantLib.SimpleMarketElement(17.0)
            h = QuantLib.MarketElementHandle(me)
            derived_me = QuantLib.DerivedMarketElement(h,f)
            derived_result = derived_me.value()
            function_result = f(me.value())
            if not (abs(derived_result - function_result) < 1e-10):
                self.fail("""
derived market element yields %(derived_result)f
function result is %(function_result)f
                """ % locals())
    def testComposite(self):
        "Testing composite market element"
        me1 = QuantLib.SimpleMarketElement(12.0)
        me2 = QuantLib.SimpleMarketElement(13.0)
        h1 = QuantLib.MarketElementHandle(me1)
        h2 = QuantLib.MarketElementHandle(me2)
        for f in [lambda x,y: x + y,
                  lambda x,y: x * y,
                  lambda x,y: x - y]:
            composite_me = QuantLib.CompositeMarketElement(h1,h2,f)
            composite_result = composite_me.value()
            function_result = f(me1.value(),me2.value())
            if not (abs(composite_result - function_result) < 1e-10):
                self.fail("""
composite market element yields %(composite_result)f
function result is %(function_result)f
                """ % locals())


if __name__ == '__main__':
    print 'testing QuantLib', QuantLib.__version__
    suite = unittest.TestSuite()
    suite.addTest(unittest.makeSuite(MarketElementTest,'test'))
    unittest.TextTestRunner(verbosity=2).run(suite)
    raw_input('press return to continue')

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

class InstrumentTest(unittest.TestCase):
    def testObservable(self):
        "Testing observability of stocks"
        global flag
        flag = None
        me = QuantLib.SimpleMarketElement(0.0)
        h = QuantLib.MarketElementHandle(me)
        s = QuantLib.Stock(h)
        obs = QuantLib.Observer(raiseFlag)
        obs.registerWith(s)
        me.setValue(3.14)
        if not flag:
            self.fail("Observer was not notified of stock value change")


if __name__ == '__main__':
    print 'testing QuantLib', QuantLib.__version__
    suite = unittest.TestSuite()
    suite.addTest(unittest.makeSuite(InstrumentTest,'test'))
    unittest.TextTestRunner(verbosity=2).run(suite)
    raw_input('press return to continue')

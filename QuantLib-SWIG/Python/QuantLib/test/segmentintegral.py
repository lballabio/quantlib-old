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

import QuantLib
import unittest
import math

class SegmentIntegralTest(unittest.TestCase):
    def Gauss(self,x):
        return math.exp(-x*x/2.0)/math.sqrt(2*math.pi)
    def runTest(self):
        "Testing segment integral"
        tolerance = 1e-4
        integrate = QuantLib.SegmentIntegral(10000)
        cases = [["f(x) = 1",        lambda x: 1,    0.0,     1.0, 1.0],
                 ["f(x) = x",        lambda x: x,    0.0,     1.0, 0.5],
                 ["f(x) = x^2",      lambda x: x*x,  0.0,     1.0, 1.0/3.0],
                 ["f(x) = sin(x)",   math.sin,       0.0, math.pi, 2.0],
                 ["f(x) = cos(x)",   math.cos,       0.0, math.pi, 0.0],
                 ["f(x) = Gauss(x)", self.Gauss,   -10.0,    10.0, 1.0]]
        
        for tag,f,a,b,expected in cases:
            calculated = integrate(f,a,b)
            if not (abs(calculated-expected) <= tolerance):
                self.fail("""
integrating %(tag)s
    calculated: %(calculated)f
    expected  : %(expected)f
                      """ % locals())


if __name__ == '__main__':
    print 'testing QuantLib', QuantLib.__version__
    suite = unittest.TestSuite()
    suite.addTest(SegmentIntegralTest())
    unittest.TextTestRunner(verbosity=2).run(suite)
    raw_input('press return to continue')


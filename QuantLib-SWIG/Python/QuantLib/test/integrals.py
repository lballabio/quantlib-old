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

import QuantLib
import unittest
import math

class IntegralTest(unittest.TestCase):
    def Gauss(self,x):
        return math.exp(-x*x/2.0)/math.sqrt(2*math.pi)
    def singleTest(self,I):
        tolerance = 1e-4
        cases = [["f(x) = 1",        lambda x: 1,    0.0,     1.0, 1.0],
                 ["f(x) = x",        lambda x: x,    0.0,     1.0, 0.5],
                 ["f(x) = x^2",      lambda x: x*x,  0.0,     1.0, 1.0/3.0],
                 ["f(x) = sin(x)",   math.sin,       0.0, math.pi, 2.0],
                 ["f(x) = cos(x)",   math.cos,       0.0, math.pi, 0.0],
                 ["f(x) = Gauss(x)", self.Gauss,   -10.0,    10.0, 1.0]]
        
        for tag,f,a,b,expected in cases:
            calculated = I(f,a,b)
            if not (abs(calculated-expected) <= tolerance):
                self.fail("""
integrating %(tag)s
    calculated: %(calculated)f
    expected  : %(expected)f
                      """ % locals())

    def testSegment(self):
        "Testing segment integration"
        self.singleTest(QuantLib.SegmentIntegral(10000))
    def testTrapezoid(self):
        "Testing trapezoid integration"
        self.singleTest(QuantLib.TrapezoidIntegral(1.0e-4))
    def testSimpson(self):
        "Testing Simpson integration"
        self.singleTest(QuantLib.SimpsonIntegral(1.0e-4))
    def testKronrod(self):
        "Testing Gauss-Kronrod integration"
        self.singleTest(QuantLib.KronrodIntegral(1.0e-4))
    


if __name__ == '__main__':
    print 'testing QuantLib', QuantLib.__version__
    suite = unittest.TestSuite()
    suite.addTest(unittest.makeSuite(IntegralTest,'test'))
    unittest.TextTestRunner(verbosity=2).run(suite)
    raw_input('press return to continue')


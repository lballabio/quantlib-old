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

import unittest
from QuantLib import *

class Foo:
    def __call__(self,x):
        return x*x-1.0
    def derivative(self,x):
        return 2.0*x

class Solver1DTest(unittest.TestCase):
    def runTest(self):
        "Testing 1-D solvers"
        for factory in [Brent,Bisection,FalsePosition,Ridder,Secant]:
            solver = factory()
            for accuracy in [1.0e-4, 1.0e-6, 1.0e-8]:
                root = solver.solve(lambda x:x*x-1.0,
                                    accuracy,1.5,0.1)
                if not (abs(root-1.0)<accuracy):
                    self.fail("""
%(factory)s
    solve():
    expected:         1.0
    calculated root:  %(root)g
    accuracy:         %(accuracy)s
                          """ % locals())
                root = solver.solve(lambda x:x*x-1.0,
                                    accuracy,1.5,0.0,1.0)
                if not (abs(root-1.0)<accuracy):
                    self.fail("""
%(factory)s
    bracketed solve():
    expected:         1.0
    calculated root:  %(root)g
    accuracy:         %(accuracy)s
                          """ % locals())
        for factory in [Newton,NewtonSafe]:
            solver = factory()
            for accuracy in [1.0e-4, 1.0e-6, 1.0e-8]:
                root = solver.solve(Foo(),accuracy,1.5,0.1)
                if not (abs(root-1.0)<accuracy):
                    self.fail("""
%(factory)s
    solve():
    expected:         1.0
    calculated root:  %(root)g
    accuracy:         %(accuracy)s
                          """ % locals())
                root = solver.solve(Foo(),accuracy,1.5,0.0,1.0)
                if not (abs(root-1.0)<accuracy):
                    self.fail("""
%(factory)s
    bracketed solve():
    expected:         1.0
    calculated root:  %(root)g
    accuracy:         %(accuracy)s
                          """ % locals())
            

if __name__ == '__main__':
    import QuantLib
    print 'testing QuantLib', QuantLib.__version__
    suite = unittest.TestSuite()
    suite.addTest(Solver1DTest())
    unittest.TextTestRunner(verbosity=2).run(suite)
    raw_input('press return to continue')


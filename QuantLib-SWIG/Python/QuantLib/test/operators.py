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

from __future__ import nested_scopes

__version__ = "$Revision$"
# $Id$

import QuantLib
import unittest
from math import sqrt

# define the norm of a discretized function
def norm(f,h):
    # squared values
    f2 = map(lambda x: x*x, f)
    # numeric integral of f^2
    I = h*(reduce(lambda x,y: x+y, f2)-0.5*f2[0]-0.5*f2[-1])
    return sqrt(I)


class OperatorTest(unittest.TestCase):
    def runTest(self):
        "Testing differential operators"
        average = 0.0
        sigma = 1.0
        normal = QuantLib.NormalDistribution(average, sigma)
        cum = QuantLib.CumulativeNormalDistribution(average, sigma)

        xMin = average - 4*sigma
        xMax = average + 4*sigma
        N = 10001
        h = (xMax-xMin)/(N-1)

        x = [ xMin+h*i for i in range(N) ]
        y = map(normal,x)
        yIntegrated = map(cum, x)
        yDerivative = map(normal.derivative, x)

        # define the differential operators...
        D = QuantLib.DZero(N,h)
        D2 = QuantLib.DPlusDMinus(N,h)
        # ...and calculate the derivatives
        yTemp  = D.applyTo(yIntegrated)
        ydTemp = D2.applyTo(yIntegrated)

        #check that first order derivative operator = gaussian
        e = norm(map(lambda x,y:x-y,yTemp,y),h)
        if not (e <= 1.0e-6):
            self.fail("""
norm of FD 1st deriv. of cum minus analytic gaussian: %(e)5.2e
tolerance exceeded
                      """ % locals())

        # check that second order derivative operator = normal.derivative
        e = norm(map(lambda x,y:x-y,ydTemp,yDerivative),h)
        if not (e <= 1.0e-4):
            self.fail("""
norm of FD 2nd deriv. of cum minus analytic gaussian.derivative: %(e)5.2e
tolerance exceeded
                      """ % locals())


if __name__ == '__main__':
    print 'testing QuantLib', QuantLib.__version__
    suite = unittest.TestSuite()
    suite.addTest(OperatorTest())
    unittest.TextTestRunner(verbosity=2).run(suite)
    raw_input('press return to continue')


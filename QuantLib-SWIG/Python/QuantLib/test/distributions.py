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

import QuantLib
import unittest
from math import exp, sqrt, pi

# define a Gaussian
def gaussian(x, average, sigma):
    normFact = sigma * sqrt(2*pi)
    dx = x-average
    return exp(-dx*dx/(2.0*sigma*sigma))/normFact

def gaussianDerivative(x, average, sigma):
    normFact = sigma*sigma*sigma*sqrt(2*pi)
    dx = x-average
    return -dx*exp(-dx*dx/(2.0*sigma*sigma))/normFact

# define the norm of a discretized function
def norm(f,h):
    # squared values
    f2 = map(lambda x: x*x, f)
    # numeric integral of f^2
    I = h*(reduce(lambda x,y: x+y, f2)-0.5*f2[0]-0.5*f2[-1])
    return sqrt(I)


class DistributionTest(unittest.TestCase):
    def runTest(self):
        "Testing distributions"
        average = 1.0
        sigma = 2.0
        normal = QuantLib.NormalDistribution(average, sigma)
        cum = QuantLib.CumulativeNormalDistribution(average, sigma)
        invCum = QuantLib.InverseCumulativeNormal(average, sigma)

        numberOfStandardDeviation = 6
        xMin = average - numberOfStandardDeviation*sigma
        xMax = average + numberOfStandardDeviation*sigma
        # odd in include average
        N = 100001
        h = (xMax-xMin)/(N-1)

        x = [ xMin+h*i for i in range(N) ]
        y = map(lambda x: gaussian(x,average,sigma), x)

        yIntegrated = map(cum, x)
        yTemp = map(normal, x)
        y2Temp = map(cum.derivative, x)
        xTemp = map(invCum, yIntegrated)
        yd = map(normal.derivative, x)
        ydTemp = map(lambda x: gaussianDerivative(x,average,sigma), x)

        #check norm=gaussian
        e = norm(map(lambda x,y:x-y,yTemp,y),h)
        if not (e <= 1.0e-16):
            self.fail("""
norm of C++ NormalDistribution minus analytic gaussian: %(e)5.2e
tolerance exceeded
                      """ % locals())

        #check invCum(cum) = Identity
        e = norm(map(lambda x,y:x-y,xTemp,x),h)
        if not (e <= 1.0e-3):
            self.fail("""
norm of C++ invCum(cum(.)) minus identity: %(e)5.2e
tolerance exceeded
                      """ % locals())

        #check cum.derivative=normal
        e = norm(map(lambda x,y:x-y,y2Temp,y),h)
        if not (e <= 1.0e-16):
            self.fail("""
norm of C++ Cumulative.derivative minus analytic gaussian: %(e)5.2e
tolerance exceeded
                      """ % locals())

        #check normal.derivative=gaussianDerivative
        e = norm(map(lambda x,y:x-y,ydTemp,yd),h)
        if not (e <= 1.0e-16):
            self.fail("""
norm of C++ NormalDist.derivative minus analytic gaussian.derivative: %(e)5.2e
tolerance exceeded
                      """ % locals())



if __name__ == '__main__':
    print 'testing QuantLib', QuantLib.__version__
    suite = unittest.TestSuite()
    suite.addTest(DistributionTest())
    unittest.TextTestRunner(verbosity=2).run(suite)
    raw_input('press return to continue')


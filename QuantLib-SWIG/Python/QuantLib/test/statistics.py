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

class StatisticsTest(unittest.TestCase):
    def runTest(self):
        "Testing statistics"
        tol = 1e-9

        data =    [3.0, 4.0, 5.0, 2.0, 3.0, 4.0, 5.0, 6.0, 4.0, 7.0]
        weights = [1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]

        s = QuantLib.Statistics()
        s.add(data, weights)

        samples = s.samples()
        N = len(data)
        if not (samples == N):
            self.fail("""
wrong number of samples
    calculated: %(samples)d
    expected  : %(N)d
                      """ % locals())

        weightSum = s.weightSum()
        rightWeightSum = reduce(lambda x,y:x+y, weights)
        if not (weightSum == rightWeightSum):
            self.fail("""
wrong sum of weights\n' + \
    calculated: %(weightSum)f
    expected  : %(rightWeightSum)f
                      """ % locals())

        minDatum = s.min()
        maxDatum = s.max()
        rightMin = min(data)
        rightMax = max(data)
        if not (minDatum == rightMin):
            self.fail("""
wrong minimum value
    calculated: %(minDatum)f
    expected  : %(rightMin)f
                      """ % locals())

        if not (maxDatum == rightMax):
            self.fail("""
wrong maximum value
    calculated: %(maxDatum)f
    expected  : %(rightMax)f
                      """ % locals())

        mean = s.mean()
        rightMean = reduce(lambda x,y:x+y,
                           map(lambda x,y:x*y, data, weights)) \
            / reduce(lambda x,y:x+y, weights)
        if not (abs(mean-rightMean) <= tol):
            self.fail("""
wrong mean value\n' + \
    calculated: %(mean)f
    expected  : %(rightMean)f
                      """ % locals())

        variance = s.variance()
        if not (abs(variance-2.23333333333) <= tol):
            self.fail("""
wrong variance
    calculated: %(variance)f
    expected  : 2.23333333333
                      """ % locals())

        stdDev = s.standardDeviation()
        if not (abs(stdDev-1.4944341181) <= tol):
            self.fail("""
wrong standard deviation
    calculated: %(stdDev)f
    expected  : 1.4944341181
                      """ % locals())

        skewness = s.skewness()
        if not (abs(skewness-0.359543071407) <= tol):
            self.fail("""
wrong skewness
    calculated: %(skewness)f
    expected  : 0.359543071407
                      """ % locals())

        kurtosis = s.kurtosis()
        if not (abs(kurtosis+0.151799637209) <= tol):
            self.fail("""
wrong kurtosis
    calculated: %(kurtosis)f
    expected  : -0.151799637209
                      """ % locals())

        s.reset()

        data = [ x-3 for x in data ]
        s.add(data, weights)
        downDev = s.downsideDeviation()
        if not (abs(downDev-0.333333333) <= tol):
            self.fail("""
wrong downside deviation
    calculated: %(downDev)f
    expected  : 0.333333333
                      """ % locals())


if __name__ == '__main__':
    print 'testing QuantLib', QuantLib.__version__
    suite = unittest.TestSuite()
    suite.addTest(StatisticsTest())
    unittest.TextTestRunner(verbosity=2).run(suite)
    raw_input('press return to continue')


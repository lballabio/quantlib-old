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
from math import exp, sqrt, pi

# define a Gaussian
def gaussian(x, average, sigma):
    normFact = sigma*sqrt(2*pi)
    dx = x-average
    return exp(-dx*dx/(2.0*sigma*sigma))/normFact

class RiskStatisticsTest(unittest.TestCase):
    def runTest(self):
        "Testing statistics risk measures"
        s = QuantLib.RiskStatistics()
        testCases = [(average,sigma) for average in [-100.0, 0.0, 100.0]
                                     for sigma in [0.1, 1.0, 10]]
        N = 25000
        numberOfSigma = 15

        for average,sigma in testCases:
            #target cannot be changed:
            #it is a strong assumption to compute values to be checked
            target = average
            normal = QuantLib.NormalDistribution(average, sigma)

            dataMin = average - numberOfSigma*sigma
            dataMax = average + numberOfSigma*sigma
            # even NOT to include average
            h = (dataMax-dataMin)/(N-1)

            data = [ dataMin+h*i for i in range(N)]

            weights = [ gaussian(x,average,sigma) for x in data ]
            s.add(data, weights)

            samples = s.samples()
            if not (samples == N):
                self.fail("""
wrong number of samples
    calculated: %(samples)d
    expected  : %(N)d
                          """ % locals())

            rightWeightSum = reduce(lambda x,y: x+y, weights)
            weightSum = s.weightSum()
            if not (abs(weightSum-rightWeightSum)/weightSum <= 1e-13):
                self.fail("""
wrong sum of weights
    calculated: %(weightSum)f
    expected  : %(rightWeightSum)f
                          """ % locals())

            minDatum = s.min()
            maxDatum = s.max()
            if not (minDatum == dataMin):
                self.fail("""
wrong minimum value
    calculated: %(minDatum)f
    expected  : %(dataMin)f
                          """ % locals())

            if not (abs(s.max()-dataMax) <= 1e-13):
                self.fail("""
wrong maximum value
    calculated: %(maxDatum)f
    expected  : %(dataMax)f
                          """ % locals())

            mean = s.mean()
            if average == 0.0:
                check = abs(mean-average)
            else:
                check = abs((mean-average)/average)
            if not (check <= 1e-13):
                self.fail("""
wrong mean value
    calculated: %(mean)f
    expected  : %(average)f
                          """ % locals())

            variance = s.variance()
            sigma2 = sigma*sigma
            if not (abs(variance-sigma2)/sigma2 <= 1e-4):
                self.fail("""
wrong variance
    calculated: %(variance)f
    expected  : %(sigma2)f
                          """ % locals())

            stdDev = s.standardDeviation()
            if not (abs(stdDev-sigma)/sigma <= 1e-4):
                self.fail("""
wrong standard deviation
    calculated: %(stdDev)f
    expected  : %(sigma)f
                          """ % locals())

            skewness = s.skewness()
            if not (abs(skewness) <= 1e-4):
                self.fail("""
wrong skewness
    calculated: %(skewness)f
    expected  : 0.0
                          """ % locals())

            kurtosis = s.kurtosis()
            if not (abs(kurtosis) <= 1e-1):
                self.fail("""
wrong kurtosis
    calculated: %(kurtosis)f
    expected  : 0.0
                          """ % locals())

            twoStdDev = QuantLib.CumulativeNormalDistribution(average, sigma)(
                average+2.0*sigma)
            rightPotentialUpside = max(average+2.0*sigma, 0.0)
            potentialUpside = s.potentialUpside(twoStdDev)
            if rightPotentialUpside == 0.0:
                check = abs(potentialUpside-rightPotentialUpside)
            else:
                check = abs(potentialUpside-rightPotentialUpside)/\
                        rightPotentialUpside
            if not (check <= 1e-3):
                self.fail("""
wrong potential upside
    calculated: %(potentialUpside)f
    expected:   %(rightPotentialUpside)f
                          """ % locals())

            rightVAR = -min(average-2.0*sigma, 0.0)
            VAR = s.valueAtRisk(twoStdDev)
            if rightVAR == 0.0:
                check = abs(VAR-rightVAR)
            else:
                check = abs(VAR-rightVAR)/rightVAR
            if not (check <= 1e-3):
                self.fail("""
wrong value at risk
    calculated: %(VAR)f
    expected:   %(rightVAR)f
                          """ % locals())

            if average > 0 and sigma < average:
                s.reset()
                continue

            tempVAR = average-2.0*sigma
            rightExShortfall = average - \
                               sigma*sigma*gaussian(tempVAR, average, sigma)/ \
                               (1.0-twoStdDev)
            rightExShortfall = -min(rightExShortfall, 0.0)
            exShortfall = s.expectedShortfall(twoStdDev)
            if rightExShortfall == 0.0:
                check = abs(exShortfall)
            else:
                check = abs(exShortfall-rightExShortfall)/rightExShortfall
            if not (check <= 2e-4):
                self.fail("""
wrong expected shortfall
    calculated: %(exShortfall)f
    expected:   %(rightExShortfall)f
                          """ % locals())

            rightShortfall = 0.5
            shortfall = s.shortfall(target)
            if not (abs(shortfall-rightShortfall)/rightShortfall <= 1e-8):
                self.fail("""
wrong shortfall
    calculated: %(shortFall)
    expected:   %(rightShortfall)f
                          """ % locals())

            rightAvgShortfall = 2.0*sigma/sqrt(2*pi)
            avgShortfall = s.averageShortfall(target)
            check = abs(avgShortfall-rightAvgShortfall)/rightAvgShortfall
            if not (check <= 1e-4):
                self.fail("""
wrong average shortfall
    calculated: %(avgShortfall)f
    expected:   %(rightAvgShortfall)f
                          """ % locals())

            s.reset()


if __name__ == '__main__':
    print 'testing QuantLib', QuantLib.__version__
    suite = unittest.TestSuite()
    suite.addTest(RiskStatisticsTest())
    unittest.TextTestRunner(verbosity=2).run(suite)
    raw_input('press return to continue')

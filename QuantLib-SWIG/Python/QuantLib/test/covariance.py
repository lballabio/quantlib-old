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

from QuantLib import *
import unittest

class CovarianceTest(unittest.TestCase):
    def runTest(self):
        "Testing covariance calculation"
        vol = [0.1, 0.5, 1.0]
        corr = [[1.0, 0.2, 0.5],
                [0.2, 1.0, 0.8],
                [0.5, 0.8, 1.0]]

        n = len(vol)
        expCov = Matrix(n,n)
        for i in range(n):
            expCov[i][i] = vol[i]*vol[i]
            for j in range(i):
                expCov[i][j] = expCov[j][i] = corr[i][j]*vol[i]*vol[j]

        calcCov = covariance(vol,corr)

        for i in range(n):
            for j in range(n):
                calculated = calcCov[i][j]
                expected   = expCov[i][j]
                if not (abs(calculated - expected) <= 1e-10):
                    self.fail("""
cov[%(i)d][%(j)d]: %(calculated)g
expected   : %(expected)g
tolerance exceeded
                              """ % locals())


if __name__ == '__main__':
    import QuantLib
    print 'testing QuantLib', QuantLib.__version__
    suite = unittest.TestSuite()
    suite.addTest(CovarianceTest())
    unittest.TextTestRunner(verbosity=2).run(suite)
    raw_input('press return to continue')

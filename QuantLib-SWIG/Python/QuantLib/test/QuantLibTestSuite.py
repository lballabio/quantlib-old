
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

import sys
import unittest

def test():
    import QuantLib
    print 'testing QuantLib', QuantLib.__version__

    suite = unittest.TestSuite()

    from date import DateTest
    suite.addTest(DateTest())

    from daycounters import DayCounterTest
    suite.addTest(DayCounterTest())

    from distributions import DistributionTest
    suite.addTest(DistributionTest())

    from instruments import InstrumentTest
    suite.addTest(unittest.makeSuite(InstrumentTest,'test'))
    
    from marketelements import MarketElementTest
    suite.addTest(unittest.makeSuite(MarketElementTest,'test'))
    
    from riskstatistics import RiskStatisticsTest
    suite.addTest(RiskStatisticsTest())

    from segmentintegral import SegmentIntegralTest
    suite.addTest(SegmentIntegralTest())

    from solvers1d import Solver1DTest
    suite.addTest(Solver1DTest())

    from statistics import StatisticsTest
    suite.addTest(StatisticsTest())

    result = unittest.TextTestRunner(verbosity=2).run(suite)

    if not result.wasSuccessful:
        sys.exit(1)

if __name__ == '__main__':
    test()

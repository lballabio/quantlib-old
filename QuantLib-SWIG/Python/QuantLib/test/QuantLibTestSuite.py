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

import sys
import unittest

from calendars import CalendarTest
from capfloor import CapFloorTest
from covariance import CovarianceTest
from date import DateTest
from daycounters import DayCounterTest
from distributions import DistributionTest
from europeanoption import EuropeanOptionTest
from instruments import InstrumentTest
from marketelements import MarketElementTest
from operators import OperatorTest
from piecewiseflatforward import PiecewiseFlatForwardTest
from riskstatistics import RiskStatisticsTest
from segmentintegral import SegmentIntegralTest
from simpleswap import SimpleSwapTest
from solvers1d import Solver1DTest
from statistics import StatisticsTest
from swaption import SwaptionTest
from termstructures import TermStructureTest
# the ones below test deprecated features
from old_pricers import OldPricerTest

def test():
    import QuantLib
    print 'testing QuantLib', QuantLib.__version__

    suite = unittest.TestSuite()

    suite.addTest(CalendarTest())
    suite.addTest(unittest.makeSuite(CapFloorTest,'test'))
    suite.addTest(CovarianceTest())
    suite.addTest(DateTest())
    suite.addTest(DayCounterTest())
    suite.addTest(DistributionTest())
    suite.addTest(unittest.makeSuite(EuropeanOptionTest,'test'))
    suite.addTest(unittest.makeSuite(OldPricerTest,'test')) # to be removed
    suite.addTest(unittest.makeSuite(InstrumentTest,'test'))
    suite.addTest(unittest.makeSuite(MarketElementTest,'test'))
    suite.addTest(OperatorTest())
    suite.addTest(PiecewiseFlatForwardTest())
    suite.addTest(RiskStatisticsTest())
    suite.addTest(SegmentIntegralTest())
    suite.addTest(unittest.makeSuite(SimpleSwapTest,'test'))
    suite.addTest(Solver1DTest())
    suite.addTest(StatisticsTest())
    suite.addTest(unittest.makeSuite(SwaptionTest,'test'))
    suite.addTest(unittest.makeSuite(TermStructureTest,'test'))
    
    result = unittest.TextTestRunner(verbosity=2).run(suite)

    if not result.wasSuccessful:
        sys.exit(1)

if __name__ == '__main__':
    test()

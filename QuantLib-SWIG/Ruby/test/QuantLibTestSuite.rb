=begin
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
=end

# $Id$

require 'test/unit'
require 'test/unit/testsuite'
require 'test/unit/ui/console/testrunner'

require 'calendars'
require 'capfloor'
require 'covariance'
require 'dates'
require 'daycounters'
require 'distributions'
require 'europeanoption'
require 'instruments'
require 'marketelements'
require 'operators'
require 'piecewiseflatforward'
require 'riskstatistics'
require 'segmentintegral'
require 'simpleswap'
require 'solvers1d'
require 'statistics'
require 'swaption'
require 'termstructures'
# to be removed
require 'old_pricers'

suite = Test::Unit::TestSuite.new('QuantLib test suite')
suite << CalendarTest.suite
suite << CapFloorTest.suite
suite << CovarianceTest.suite
suite << DateTest.suite
suite << DayCounterTest.suite
suite << DistributionTest.suite
suite << EuropeanOptionTest.suite
suite << InstrumentTest.suite
suite << MarketElementTest.suite
suite << MarketElementHandleTest.suite
suite << OperatorTest.suite
suite << PiecewiseFlatForwardTest.suite
suite << RiskStatisticsTest.suite
suite << SegmentIntegralTest.suite
suite << SimpleSwapTest.suite
suite << Solver1DTest.suite
suite << StatisticsTest.suite
suite << SwaptionTest.suite
suite << TermStructureTest.suite
suite << OldPricerTest.suite

verbosity = Test::Unit::UI::Console::TestRunner::VERBOSE
runner = Test::Unit::UI::Console::TestRunner.new(suite,verbosity,STDOUT)
result = runner.start
unless result.passed?
  exit(1)
end


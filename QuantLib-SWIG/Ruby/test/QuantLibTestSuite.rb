=begin
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
=end

# $Id$

require 'runit/testcase'
require 'runit/testsuite'
require 'runit/cui/testrunner'

require 'dates'
require 'daycounters'
require 'distributions'
require 'instruments'
require 'marketelements'
require 'riskstatistics'
require 'solvers1d'

suite = RUNIT::TestSuite.new
suite.add_test(DateTest.suite)
suite.add_test(DayCounterTest.suite)
suite.add_test(DistributionTest.suite)
suite.add_test(InstrumentTest.suite)
suite.add_test(MarketElementTest.suite)
suite.add_test(MarketElementHandleTest.suite)
suite.add_test(RiskStatisticsTest.suite)
suite.add_test(Solver1DTest.suite)

result = RUNIT::CUI::TestRunner.run(suite)
unless result.succeed?
  exit(1)
end



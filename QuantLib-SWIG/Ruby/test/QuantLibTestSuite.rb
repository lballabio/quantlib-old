=begin
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
=end

require 'rbconfig'
require 'test/unit'
require 'test/unit/testsuite'
require 'test/unit/ui/console/testrunner'

require 'dates'
require 'instruments'
require 'integrals'
require 'marketelements'
require 'solvers1d'
require 'termstructures'

suite = Test::Unit::TestSuite.new('QuantLib test suite')
suite << DateTest.suite
suite << InstrumentTest.suite
suite << MarketElementTest.suite
suite << MarketElementHandleTest.suite
suite << SegmentIntegralTest.suite
suite << Solver1DTest.suite
suite << TermStructureTest.suite

if Config::CONFIG['ruby_version'] == '1.8'
  verbosity = Test::Unit::UI::VERBOSE
  runner = Test::Unit::UI::Console::TestRunner.new(suite,verbosity,STDOUT)
else
  verbosity = Test::Unit::UI::Console::TestRunner::VERBOSE
  runner = Test::Unit::UI::Console::TestRunner.new(suite,verbosity,STDOUT)
end
result = runner.start
unless result.passed?
  exit(1)
end


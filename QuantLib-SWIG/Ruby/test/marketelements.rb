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

require 'QuantLib'
require 'runit/testcase'
require 'runit/cui/testrunner'

class MarketElementTest < RUNIT::TestCase
  def name
    "Testing observability of market elements..."
  end
  def test
    flag = false
    me = QuantLib::SimpleMarketElement.new(0.0)
    obs = QuantLib::Observer.new { flag = true }
    obs.registerWith(me)
    me.value = 3.14
    unless flag
        assert_fail("Observer was not notified of market element change")
    end
  end
end

class MarketElementHandleTest < RUNIT::TestCase
  def name
    "Testing observability of market element handles..."
  end
  def test
    flag = false
    me1 = QuantLib::SimpleMarketElement.new(0.0)
    h = QuantLib::MarketElementHandle.new(me1)
    obs = QuantLib::Observer.new { flag = true }
    obs.registerWith(h)
    me1.value = 3.14
    unless flag
        assert_fail("Observer was not notified of market element change")
    end
    flag = false
    me2 = QuantLib::SimpleMarketElement.new(0.0)
    h.linkTo!(me2)
    unless flag
        assert_fail("Observer was not notified of market element change")
    end
  end
end


if $0 == __FILE__
  RUNIT::CUI::TestRunner.run(MarketElementTest.suite)
  RUNIT::CUI::TestRunner.run(MarketElementHandleTest.suite)
end


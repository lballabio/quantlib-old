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

require 'QuantLib'
require 'test/unit/testcase'
require 'test/unit/ui/console/testrunner'

class InstrumentTest < Test::Unit::TestCase
  def name
    "Testing observability of stocks"
  end
  def testStock
    flag = false
    me = QuantLib::SimpleMarketElement.new(0.0)
    h = QuantLib::MarketElementHandle.new(me)
    s = QuantLib::Stock.new(h)
    obs = QuantLib::Observer.new { flag = true }
    obs.registerWith(s)
    me.value = 3.14
    unless flag
        flunk("Observer was not notified of stock value change")
    end
  end
end


if $0 == __FILE__
  Test::Unit::UI::Console::TestRunner.run(InstrumentTest)
end


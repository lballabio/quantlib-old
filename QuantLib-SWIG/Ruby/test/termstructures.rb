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

class TermStructureTest < RUNIT::TestCase
  include QuantLib
  def name
    "Testing basic term structures..."
  end
  def setup
    today = Date::todaysDate()
    settlement = Calendar.new('TARGET').advance(today,2,'days')
    @termStructure = FlatForward.new('EUR',DayCounter.new('act/365'),
                                     today,settlement,0.05)
  end
  def testImplied
    tolerance = 1.0e-10
    h = TermStructureHandle.new(@termStructure)
    new_today = @termStructure.todaysDate.plusYears(3)
    new_settlement = Calendar.new('TARGET').advance(new_today,2,'days')
    test_date = new_settlement.plusYears(5)
    implied = ImpliedTermStructure.new(h,new_today,new_settlement)
    base_discount = @termStructure.discount(new_settlement)
    discount = @termStructure.discount(test_date)
    implied_discount = implied.discount(test_date)
    unless (discount - base_discount*implied_discount).abs <= tolerance
      assert_fail(<<-MESSAGE

unable to reproduce discount from implied curve
    calculated: #{base_discount*implied_discount}
    expected:   #{discount}

                  MESSAGE
                  )
    end
  end
  def testImpliedObs
    flag = false
    h = TermStructureHandle.new(@termStructure)
    new_today = @termStructure.todaysDate.plusYears(3)
    new_settlement = Calendar.new('TARGET').advance(new_today,2,'days')
    implied = ImpliedTermStructure.new(h,new_today,new_settlement)
    obs = Observer.new { flag = true }
    obs.registerWith(implied.toObservable)
    h.linkTo!(@termStructure)
    unless flag
      assert_fail("Observer was not notified of term structure change")
    end
  end
  def testFSpreaded
    tolerance = 1.0e-10
    me = SimpleMarketElement.new(0.01)
    mh = MarketElementHandle.new(me)
    h = TermStructureHandle.new(@termStructure)
    spreaded = ForwardSpreadedTermStructure.new(h,mh)
    test_date = @termStructure.todaysDate.plusYears(5)
    forward = @termStructure.forward(test_date)
    spreaded_forward = spreaded.forward(test_date)
    unless ((forward+me.value)-spreaded_forward).abs <= tolerance
      assert_fail(<<-MESSAGE

unable to reproduce forward from spreaded curve
    calculated: #{spreaded_forward-me.value}
    expected:   #{forward}

                  MESSAGE
                  )
    end
  end
  def testFSpreadedObs
    flag = false
    me = SimpleMarketElement.new(0.01)
    mh = MarketElementHandle.new(me)
    h = TermStructureHandle.new(@termStructure)
    spreaded = ForwardSpreadedTermStructure.new(h,mh)
    obs = Observer.new { flag = true }
    obs.registerWith(spreaded.toObservable)
    h.linkTo!(@termStructure)
    unless flag
      assert_fail("Observer was not notified of term structure change")
    end
    flag = false
    me.value = 0.005
    unless flag
      assert_fail("Observer was not notified of spread change")
    end
  end
  def testZSpreaded
    tolerance = 1.0e-10
    me = SimpleMarketElement.new(0.01)
    mh = MarketElementHandle.new(me)
    h = TermStructureHandle.new(@termStructure)
    spreaded = ZeroSpreadedTermStructure.new(h,mh)
    test_date = @termStructure.todaysDate.plusYears(5)
    zero = @termStructure.zeroYield(test_date)
    spreaded_zero = spreaded.zeroYield(test_date)
    unless ((zero+me.value)-spreaded_zero).abs <= tolerance
      assert_fail(<<-MESSAGE

unable to reproduce zero yield from spreaded curve
    calculated: #{spreaded_zero-me.value}
    expected:   #{zero}

                  MESSAGE
                  )
    end
  end
  def testZSpreadedObs
    flag = false
    me = SimpleMarketElement.new(0.01)
    mh = MarketElementHandle.new(me)
    h = TermStructureHandle.new(@termStructure)
    spreaded = ZeroSpreadedTermStructure.new(h,mh)
    obs = Observer.new { flag = true }
    obs.registerWith(spreaded.toObservable)
    h.linkTo!(@termStructure)
    unless flag
      assert_fail("Observer was not notified of term structure change")
    end
    flag = false
    me.value = 0.005
    unless flag
      assert_fail("Observer was not notified of spread change")
    end
  end
end

if $0 == __FILE__
  RUNIT::CUI::TestRunner.run(TermStructureTest.suite)
end


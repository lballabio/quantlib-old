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

require 'QuantLib'
require 'test/unit/testcase'
require 'test/unit/ui/console/testrunner'

class TermStructureTest < Test::Unit::TestCase
  include QuantLib
  def name
    case @method_name
      when 'testImpliedObs'
        "Testing observability of implied term structure"
      when 'testFSpreadedObs'
        "Testing observability of forward-spreaded term structure"
      when 'testZSpreadedObs'
        "Testing observability of zero-spreaded term structure"
    end
  end
  def setup
    @calendar = Calendar.new('TARGET')
    @settlementDays = 2
    today = @calendar.adjust(Date::todaysDate)
    settlement = @calendar.advance(today,@settlementDays,'days')
    depositData = [
      [1,  'month', 4.581],
      [2, 'months', 4.573],
      [3, 'months', 4.557],
      [6, 'months', 4.496],
      [9, 'months', 4.490]
    ]
    deposits = depositData.map { |n,units,rate|
      DepositRateHelper.new(
        QuoteHandle.new(SimpleQuote.new(rate/100)),
        n, units, @settlementDays, @calendar, 'mf', DayCounter.new('act/360'))
    }    
    swapData = [
        [ 1, 4.54],
        [ 5, 4.99],
        [10, 5.47],
        [20, 5.89],
        [30, 5.96]
    ]
    swaps = swapData.map { |years,rate|
      SwapRateHelper.new(
        QuoteHandle.new(SimpleQuote.new(rate/100)),
        years, "years", @settlementDays, 
        @calendar, 1, 'unadjusted', DayCounter.new('30/360'),
        2, 'mf')
    }
    @termStructure = PiecewiseFlatForward.new(today,settlement,
                                              deposits+swaps,
                                              DayCounter.new('Act/360'))
  end
  def testImpliedObs
    flag = false
    h = TermStructureHandle.new
    new_today = @termStructure.todaysDate.plusYears(3)
    new_settlement = @calendar.advance(new_today,@settlementDays,'days')
    implied = ImpliedTermStructure.new(h,new_today,new_settlement)
    obs = Observer.new { flag = true }
    obs.registerWith(implied)
    h.linkTo!(@termStructure)
    unless flag
      flunk("Observer was not notified of term structure change")
    end
  end
  def testFSpreadedObs
    flag = false
    me = SimpleQuote.new(0.01)
    mh = QuoteHandle.new(me)
    h = TermStructureHandle.new
    spreaded = ForwardSpreadedTermStructure.new(h,mh)
    obs = Observer.new { flag = true }
    obs.registerWith(spreaded)
    h.linkTo!(@termStructure)
    unless flag
      flunk("Observer was not notified of term structure change")
    end
    flag = false
    me.value = 0.005
    unless flag
      flunk("Observer was not notified of spread change")
    end
  end
  def testZSpreadedObs
    flag = false
    me = SimpleQuote.new(0.01)
    mh = QuoteHandle.new(me)
    h = TermStructureHandle.new
    spreaded = ZeroSpreadedTermStructure.new(h,mh)
    obs = Observer.new { flag = true }
    obs.registerWith(spreaded)
    h.linkTo!(@termStructure)
    unless flag
      flunk("Observer was not notified of term structure change")
    end
    flag = false
    me.value = 0.005
    unless flag
      flunk("Observer was not notified of spread change")
    end
  end
end

if $0 == __FILE__
  Test::Unit::UI::Console::TestRunner.run(TermStructureTest)
end


=begin
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl
 Copyright (C) 2007 StatPro Italia srl

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it
 under the terms of the QuantLib license.  You should have received a
 copy of the license along with this program; if not, please email
 <quantlib-dev@lists.sf.net>. The license is also available online at
 <http://quantlib.org/license.shtml>.

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
    @calendar = TARGET.new
    @settlementDays = 2
    today = @calendar.adjust(Date::todaysDate)
    settlement = @calendar.advance(today,@settlementDays,Days)
    depositData = [
      [1, Months, 4.581],
      [2, Months, 4.573],
      [3, Months, 4.557],
      [6, Months, 4.496],
      [9, Months, 4.490]
    ]
    deposits = depositData.map { |n,units,rate|
      DepositRateHelper.new(QuoteHandle.new(SimpleQuote.new(rate/100)),
                            Period.new(n, units), @settlementDays, @calendar,
                            ModifiedFollowing, false, Actual360.new)
    }
    swapData = [
        [ 1, 4.54],
        [ 5, 4.99],
        [10, 5.47],
        [20, 5.89],
        [30, 5.96]
    ]
    swaps = swapData.map { |years,rate|
      SwapRateHelper.new(QuoteHandle.new(SimpleQuote.new(rate/100)),
                         Period.new(years, Years), @calendar,
                         1, Unadjusted, Thirty360.new, Euribor6M.new)
    }
    @termStructure = PiecewiseFlatForward.new(settlement,
                                              deposits+swaps,
                                              Actual360.new)
  end
  def testImpliedObs
    flag = false
    h = RelinkableYieldTermStructureHandle.new
    settlement = @termStructure.referenceDate
    new_settlement = @calendar.advance(settlement,3,Years)
    implied = ImpliedTermStructure.new(h,new_settlement)
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
    h = RelinkableYieldTermStructureHandle.new
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
    h = RelinkableYieldTermStructureHandle.new
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


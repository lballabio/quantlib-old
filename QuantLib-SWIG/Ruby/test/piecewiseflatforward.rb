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

class PiecewiseFlatForwardTest < Test::Unit::TestCase
  include QuantLib
  def name
    "Testing piecewise flat forward curve"
  end
  def testRates
    euriborHandle = TermStructureHandle.new
    calendar = Calendar.new('TARGET')
    today = calendar.roll(Date.todaysDate)
    settlementDays = 2
    settlement = calendar.advance(today,
                                  settlementDays,"days",
                                  'following')
    fixingDays = 2
    # deposits
    rollingConvention = 'ModifiedFollowing'
    dayCounter = DayCounter.new('Act/360')
    depositData = [
      [1,   'week', 4.559],
      [1,  'month', 4.581],
      [2, 'months', 4.573],
      [3, 'months', 4.557],
      [6, 'months', 4.496],
      [9, 'months', 4.490]
    ]
    deposits = depositData.map { |n,units,rate|
      DepositRateHelper.new(
        MarketElementHandle.new(SimpleMarketElement.new(rate/100)),
        n, units, settlementDays, calendar, rollingConvention, dayCounter)
    }
    # swaps
    swapRollingConvention = 'modifiedFollowing'
    fixedFrequency = 1
    fixedIsAdjusted = false
    fixedDayCount = DayCounter.new('30/360')
    floatingFrequency = 2
    swapData = [
        [ 1, 4.54],
        [ 2, 4.63],
        [ 3, 4.75],
        [ 4, 4.86],
        [ 5, 4.99],
        [ 6, 5.11],
        [ 7, 5.23],
        [ 8, 5.33],
        [ 9, 5.41],
        [10, 5.47],
        [12, 5.60],
        [15, 5.75],
        [20, 5.89],
        [25, 5.95],
        [30, 5.96]
    ]
    swaps = swapData.map { |years,rate|
      SwapRateHelper.new(
        MarketElementHandle.new(SimpleMarketElement.new(rate/100)),
        years, "years", settlementDays, 
        calendar, swapRollingConvention, fixedFrequency, 
        fixedIsAdjusted, fixedDayCount, floatingFrequency)
    }
    # all instruments
    instruments = deposits + swaps
    # instantiate curve
    termStructure = PiecewiseFlatForward.new(today,settlement,instruments,
                                             DayCounter.new('Act/360'))
    euriborHandle.linkTo!(termStructure)
    # check deposits
    depositData.each do |n,units,expectedRate|
      expectedRate = expectedRate/100
      index = Xibor.new("Euribor",n,units, euriborHandle)
      estimatedRate = index.fixing(today)
      unless (estimatedRate - expectedRate).abs <= 1.0e-9
        flunk(<<-MESSAGE

    #{n} #{units} deposit:
        estimated rate: #{estimatedRate}
        input rate:     #{expectedRate}

              MESSAGE
              )
      end
    end
    # check swaps
    index = Xibor.new("Euribor",12/floatingFrequency,'Months', euriborHandle)
    swapData.each do |years,expectedRate|
      expectedRate = expectedRate/100
      swap = SimpleSwap.new(true,settlement,years,'years',
                            calendar,swapRollingConvention,100.0,
                            fixedFrequency,0.0,fixedIsAdjusted,
                            fixedDayCount,floatingFrequency,index,
                            fixingDays,0.0,euriborHandle)
      estimatedRate = swap.fairRate()
      unless (estimatedRate - expectedRate).abs <= 1.0e-9
        flunk(<<-MESSAGE

    #{years} years swap:
        estimated rate: #{estimatedRate}
        input rate:     #{expectedRate}

              MESSAGE
              )
      end
    end
  end
end

if $0 == __FILE__
  Test::Unit::UI::Console::TestRunner.run(PiecewiseFlatForwardTest)
end


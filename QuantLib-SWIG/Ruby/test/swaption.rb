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

class SwaptionTest < Test::Unit::TestCase
  include QuantLib
  def name
    case @method_name
      when 'testStrikeDependency'
        "Testing swaption dependency on strike"
      when 'testSpreadDependency'
        "Testing swaption dependency on spread"
      when 'testSpreadTreatment'
        "Testing swaption treatment of spread"
      when 'testCachedValue'
        "Testing swaption value against cached value"
    end
  end
  def setup
    @today = Date.todaysDate
    @termStructure = TermStructureHandle.new
    @nominal = 100.0
    @rollingConvention = 'modifiedFollowing'
    @fixedFrequency = 1
    @floatingFrequency = 2
    @fixedDayCount = DayCounter.new('30/360')
    @fixedAdj = false
    @index = Xibor.new('Euribor',12/@floatingFrequency,'Months',
                       @termStructure)
    @calendar = @index.calendar
    @settlementDays = 2
    @settlement = @calendar.advance(@today,
                                    @settlementDays,"days",
                                    'following')
    @fixingDays = 2
    @termStructure.linkTo!(FlatForward.new(@today,@settlement,
                                           0.05,DayCounter.new('Act/365')))
    @cases = []
    [1, 2, 3, 5, 7, 10].each { |exercise|
    [1, 2, 3, 5, 7, 10, 15, 20].each { |length|
    [false,true].each { |payFixed|
      @cases.push [exercise,length,payFixed]
    }}}
  end
  def makeSwap(startDate,length,fixedRate,spread,payFixed)
    SimpleSwap.new(payFixed, startDate, length, 'years',
                   @calendar, @rollingConvention, @nominal,
                   @fixedFrequency, fixedRate, @fixedAdj, @fixedDayCount,
                   @floatingFrequency, @index, @fixingDays, spread,
                   @termStructure)
  end
  def makeSwaption(swap,exerciseDate,volatility)
    Swaption.new(swap,EuropeanExercise.new(exerciseDate),
                 @termStructure,
                 BlackSwaptionEngine.new(
                   BlackModel.new(
                     MarketElementHandle.new(
                       SimpleMarketElement.new(volatility)),
                     @termStructure)))
  end
  def testStrikeDependency
    @cases.each do |exercise,length,payFixed|
      exerciseDate = @calendar.roll(@today.plusYears(exercise))
      startDate = @calendar.advance(exerciseDate,
                                    @settlementDays,'days')
      strikes = [0.03, 0.04, 0.05, 0.06, 0.07]
      values = strikes.map { |s|
        swap = makeSwap(startDate,length,s,0.0,payFixed)
        swaption = makeSwaption(swap,exerciseDate,0.20)
        swaption.NPV
      }
      if payFixed
        # NPV must decrease with strike
        (1...values.length).each do |i|
          unless values[i] < values[i-1]
            flunk(<<-MESSAGE

    NPV is increasing with the strike in a payer swaption:
        exercise date: #{exercise}
        length: #{length} years
        value: #{values[i-1]} at strike: #{strikes[i-1]*100}%
        value: #{values[i]  } at strike: #{strikes[i]*100  }%

                  MESSAGE
                  )
          end
        end
      else
        # NPV must increase with strike
        (1...values.length).each do |i|
          unless values[i] > values[i-1]
            flunk(<<-MESSAGE

    NPV is decreasing with the strike in a receiver swaption:
        exercise date: #{exercise}
        length: #{length} years
        value: #{values[i-1]} at strike: #{strikes[i-1]*100}%
        value: #{values[i]  } at strike: #{strikes[i]*100  }%

                  MESSAGE
                  )
          end
        end
      end
    end
  end
  def testSpreadDependency
    @cases.each do |exercise,length,payFixed|
      exerciseDate = @calendar.roll(@today.plusYears(exercise))
      startDate = @calendar.advance(exerciseDate,
                                    @settlementDays,'days')
      spreads = [-0.002, -0.001, 0.0, 0.001, 0.002]
      values = spreads.map { |s|
        swap = makeSwap(startDate,length,0.06,s,payFixed)
        swaption = makeSwaption(swap,exerciseDate,0.20)
        swaption.NPV
      }
      if payFixed
        # NPV must increase with spread
        (1...values.length).each do |i|
          unless values[i] > values[i-1]
            flunk(<<-MESSAGE

    NPV is decreasing with the spread in a payer swaption:
        exercise date: #{exercise}
        length: #{length} years
        value: #{values[i-1]} for spread: #{spreads[i-1]*100}%
        value: #{values[i]  } for spread: #{spreads[i]*100  }%

                  MESSAGE
                  )
          end
        end
      else
        # NPV must decrease with spread
        (1...values.length).each do |i|
          unless values[i] < values[i-1]
            flunk(<<-MESSAGE

    NPV is increasing with the spread in a receiver swaption:
        exercise date: #{exercise}
        length: #{length} years
        value: #{values[i-1]} for spread: #{spreads[i-1]*100}%
        value: #{values[i]  } for spread: #{spreads[i]*100  }%

                  MESSAGE
                  )
          end
        end
      end
    end
  end
  def testSpreadTreatment
    @cases.each do |exercise,length,payFixed|
      exerciseDate = @calendar.roll(@today.plusYears(exercise))
      startDate = @calendar.advance(exerciseDate,
                                    @settlementDays,'days')
      spreads = [-0.002, -0.001, 0.0, 0.001, 0.002]
      spreads.each do |s|
        swap = makeSwap(startDate,length,0.06,s,payFixed)
        correction = s*swap.floatingLegBPS/swap.fixedLegBPS
        equivalentSwap = makeSwap(startDate,length,
                                  0.06+correction,0.0,payFixed)
        swaption1 = makeSwaption(swap, exerciseDate, 0.20)
        swaption2 = makeSwaption(equivalentSwap, exerciseDate, 0.20)
        unless (swaption1.NPV-swaption2.NPV).abs <= 1.0e-10
          flunk(<<-MESSAGE

    wrong spread treatment:
        exercise date: #{exercise}
        length: #{length} years
        pay fixed: #{payFixed}
        spread: #{s*100}%
        value:                        #{swaption1.NPV}
        value of equivalent swaption: #{swaption2.NPV}

                MESSAGE
                )
        end
      end
    end
  end
  def testCachedValue
    cachedToday = Date.new(13,3,2002)
    cachedSettlement = Date.new(15,3,2002)
    @termStructure.linkTo!(FlatForward.new(cachedToday,cachedSettlement,
                                           0.05,DayCounter.new('Act/365')))
    exerciseDate = @calendar.roll(cachedSettlement.plusYears(5))
    startDate = @calendar.advance(exerciseDate,
                                  @settlementDays,'days')
    swap = makeSwap(startDate,10,0.06,0.0,true)
    swaption = makeSwaption(swap,exerciseDate,0.20)
    cachedNPV = 3.645305728310
    unless (swaption.NPV-cachedNPV).abs <= 1.0e-11
      flunk(<<-MESSAGE

    failed to reproduce cached value:
        calculated: #{swaption.NPV}
        expected:   #{cachedNPV}

            MESSAGE
            )
    end
  end
end

if $0 == __FILE__
  Test::Unit::UI::Console::TestRunner.run(SwaptionTest)
end


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

class SimpleSwapTest < RUNIT::TestCase
  include QuantLib
  def name
    case @method
      when 'testFairRate'
        "Testing simple swap calculation of fair fixed rate..."
      when 'testFairSpread'
        "Testing simple swap calculation of fair floating spread..."
      when 'testRateDependency'
        "Testing simple swap dependency on fixed rate..."
      when 'testSpreadDependency'
        "Testing simple swap dependency on floating spread..."
      when 'testCachedValue'
        "Testing simple swap calculation against cached value..."
    end
  end
  def setup
    @payFixed = true
    @today = Date.todaysDate
    @settlementDays = 2
    @fixingDays = 2
    @nominal = 100
    @rollingConvention = 'modifiedFollowing'
    @fixedFrequency = 1
    @floatingFrequency = 2
    @fixedDayCount = DayCounter.new('30/360')
    @fixedAdj = false
    @euriborHandle = TermStructureHandle.new
    @index = Xibor.new("Euribor",
                       12/@floatingFrequency,'Months',
                       @euriborHandle)
    @calendar = @index.calendar
    @settlement = @calendar.advance(@today,
                                    @settlementDays, "days",
                                    "following")
    termStructure = FlatForward.new(@today, @settlement, 0.05,
                                    DayCounter.new('Act/365'))
    @euriborHandle.linkTo!(termStructure)
  end
  def makeSwap(length,fixedRate,floatingSpread)
    SimpleSwap.new(@payFixed, @settlement, length, 'years',
                   @calendar, @rollingConvention, @nominal,
                   @fixedFrequency, fixedRate, @fixedAdj,
                   @fixedDayCount, @floatingFrequency,
                   @index, @fixingDays, floatingSpread,
                   @euriborHandle)
  end
  def testFairRate
    fixedRate = 0.0
    cases = []
    [1, 2, 5, 10, 20].each { |length|
    [-0.001, -0.01, 0, 0.01, 0.001].each { |spread|
        cases.push [length,spread]
    }}

    cases.each do |length, spread|
      swap = makeSwap(length, fixedRate, spread)
      swap = makeSwap(length, swap.fairRate, spread)
      unless swap.NPV.abs <= 1e-10
        assert_fail(<<-MESSAGE

    recalculating with implied rate:
        calculated value: #{swap.NPV}
        expected value:   0.0

                    MESSAGE
                    )
      end
    end
  end
  def testFairSpread
    spread = 0.0
    cases = []
    [1, 2, 5, 10, 20].each { |length|
    [0.04, 0.05, 0.06, 0.07].each { |rate|
        cases.push [length,rate]
    }}

    cases.each do |length, rate|
      swap = makeSwap(length, rate, spread)
      swap = makeSwap(length, rate, swap.fairSpread)
      unless swap.NPV.abs <= 1e-10
        assert_fail(<<-MESSAGE

    recalculating with implied spread:
        calculated value: #{swap.NPV}
        expected value:   0.0

                    MESSAGE
                    )
      end
    end
  end
  def testRateDependency
    cases = []
    [1, 2, 5, 10, 20].each { |length|
    [-0.001, -0.01, 0, 0.01, 0.001].each { |spread|
        cases.push [length,spread]
    }}
    rates = [0.03, 0.04, 0.05, 0.06, 0.07]

    cases.each do |length,spread|
      values = rates.map { |r|
        swap = makeSwap(length,r,spread)
        swap.NPV
      }
      # We're paying fixed - NPV must decrease with rate
      (1...values.length).each do |i|
        unless values[i] < values[i-1]
          assert_fail(<<-MESSAGE

    NPV is increasing with the fixed rate in a simple swap paying fixed:
        length: #{length} years
        value: #{values[i-1]} paying rate: #{rates[i-1]*100}%
        value: #{values[i]  } paying rate: #{rates[i]*100  }%

                      MESSAGE
                      )
        end
      end
    end
  end
  def testSpreadDependency
    cases = []
    [1, 2, 5, 10, 20].each { |length|
    [0.04, 0.05, 0.06, 0.07].each { |rate|
        cases.push [length,rate]
    }}
    spreads = [-0.01, -0.002, -0.001, 0, 0.001, 0.002, 0.01]
    
    cases.each do |length,rate|
      values = spreads.map { |s|
        swap = makeSwap(length,rate,s)
        swap.NPV
      }
      # We're paying fixed - NPV must increase with spread
      (1...values.length).each do |i|
        unless values[i] > values[i-1]
          assert_fail(<<-MESSAGE

    NPV is decreasing with the spread in a simple swap paying fixed:
        length: #{length} years
        value: #{values[i-1]} receiving spread: #{spreads[i-1]*100}%
        value: #{values[i]  } receiving spread: #{spreads[i]*100  }%

                      MESSAGE
                      )
        end
      end
    end
  end
  def testCachedValue
    @today = Date.new(17,6,2002)
    @settlement = @calendar.advance(@today,
                                    @settlementDays, "days",
                                    "following")
    termStructure = FlatForward.new(@today, @settlement, 0.05,
                                    DayCounter.new('Act/365'))
    @euriborHandle.linkTo!(termStructure)

    swap = makeSwap(10,0.06,0.001)
    cachedNPV = -5.883663676727
    unless (swap.NPV-cachedNPV).abs <= 1.0e-11
      assert_fail(<<-MESSAGE

    failed to reproduce cached simple swap value:
        calculated: #{swap.NPV}
        expected:   #{cachedNPV}

                  MESSAGE
                  )
    end
  end
end

if $0 == __FILE__
  RUNIT::CUI::TestRunner.run(SimpleSwapTest.suite)
end


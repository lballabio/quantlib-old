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
require 'test/unit'
require 'test/unit/ui/console/testrunner'

class CapFloorTest < Test::Unit::TestCase
  include QuantLib
  def name
    case @method_name
      when 'testStrikeDependency'
        "Testing cap/floor dependency on strike"
      when 'testConsistency'
        "Testing consistency between cap, floor and collar"
      when 'testParity'
        "Testing put/call parity for cap and floor"
      when 'testCachedValue'
        "Testing cap/floor value against cached values"
    end
  end
  def setup
    @termStructure = TermStructureHandle.new
    @nominals = [100.0]
    @rollingConvention = 'modifiedFollowing'
    @frequency = 2
    @index = Xibor.new('Euribor',12/@frequency,'Months',
                       @termStructure)
    @calendar = @index.calendar
    @settlementDays = 2
    @today = @calendar.roll(Date.todaysDate)
    @settlement = @calendar.advance(@today,
                                    @settlementDays,"days",
                                    'following')
    @fixingDays = 2
    @termStructure.linkTo!(FlatForward.new(@today,@settlement,
                                           0.05,DayCounter.new('Act/360')))
  end
  def makeLeg(startDate,length)
    endDate = @calendar.advance(startDate,length,"years",
                                @rollingConvention)
    FloatingRateCouponVector(@nominals,startDate,endDate,@frequency,@calendar,
                             @rollingConvention,@index,@fixingDays)
  end
  def makeCapFloor(kind,leg,strike,volatility)
    kind.new(leg,[strike],@termStructure,makeEngine(volatility))
  end
  def makeEngine(volatility)
    BlackCapFloorEngine.new(
      BlackModel.new(
        MarketElementHandle.new(SimpleMarketElement.new(volatility)),
        @termStructure))
  end
  def testStrikeDependency
    startDate = @termStructure.referenceDate
    [1, 2, 3, 5, 7, 10, 15, 20].each do |length|
      [0.01, 0.05, 0.10, 0.15, 0.20].each do |vol|
        [Cap,Floor].each do |kind|

          strikes = [0.03, 0.04, 0.05, 0.06, 0.07]
          values = strikes.map { |s|
            leg = makeLeg(startDate,length)
            instrument = makeCapFloor(kind,leg,s,vol)
            instrument.NPV
          }

          if kind == Cap
            # NPV must decrease with strike
            (1...values.length).each do |i|
              unless values[i] <= values[i-1]
                flunk(<<-MESSAGE

    NPV is increasing with the strike in a cap:
        length: #{length} years
        volatility: #{vol}
        value: #{values[i-1]} at strike: #{strikes[i-1]*100}%
        value: #{values[i]  } at strike: #{strikes[i]*100}%

                      MESSAGE
                      )
              end
            end
          else
            # NPV must increase with strike
            (1...values.length).each do |i|
              unless values[i] >= values[i-1]
                flunk(<<-MESSAGE

    NPV is decreasing with the strike in a cap:
        length: #{length} years
        volatility: #{vol}
        value: #{values[i-1]} at strike: #{strikes[i-1]*100}%
        value: #{values[i]  } at strike: #{strikes[i]*100}%

                      MESSAGE
                      )
              end
            end
          end
        end
      end
    end
  end
  def testConsistency
    startDate = @termStructure.referenceDate
    [1, 2, 3, 5, 7, 10, 15, 20].each do |length|
      [0.03, 0.04, 0.05, 0.06, 0.07].each do |capRate|
        [0.03, 0.04, 0.05, 0.06, 0.07].each do |floorRate|
          [0.01, 0.05, 0.10, 0.15, 0.20].each do |vol|

            leg = makeLeg(startDate,length)
            cap = makeCapFloor(Cap,leg,capRate,vol)
            floor = makeCapFloor(Floor,leg,floorRate,vol)
            collar = Collar.new(leg,[capRate],[floorRate],
                                @termStructure,makeEngine(vol))

            unless ((cap.NPV-floor.NPV) - collar.NPV).abs <= 1.0e-10
              flunk(<<-MESSAGE

    inconsistency between cap, floor and collar:
        length      : #{length} years
        volatility  : #{vol}
        cap value   : #{cap.NPV  } at strike: #{capRate*100}%
        floor value : #{floor.NPV} at strike: #{floorRate*100}%
        collar value: #{collar.NPV}

                    MESSAGE
                    )
            end
          end
        end
      end
    end
  end
  def testParity
    startDate = @termStructure.referenceDate
    [1, 2, 3, 5, 7, 10, 15, 20].each do |length|
      [0.03, 0.04, 0.05, 0.06, 0.07].each do |strike|
        [0.01, 0.05, 0.10, 0.15, 0.20].each do |vol|
          
          leg = makeLeg(startDate,length)
          cap = makeCapFloor(Cap,leg,strike,vol)
          floor = makeCapFloor(Floor,leg,strike,vol)
          swap = SimpleSwap.new(true,startDate,length,"years",@calendar,
                                @rollingConvention,@nominals[0],
                                @frequency,strike,@index.isAdjusted?,
                                @index.dayCounter,@frequency,
                                @index,@fixingDays,0.0,@termStructure)
          unless ((cap.NPV-floor.NPV) - swap.NPV).abs <= 1.0e-10
            flunk(<<-MESSAGE

    put/call parity violated:
        length     : #{length} years
        volatility : #{vol}
        strike     : #{strike*100}%
        cap value  : #{cap.NPV}
        floor value: #{floor.NPV}
        swap value : #{swap.NPV}

                  MESSAGE
                  )
          end
        end
      end
    end
  end
  def testCachedValue
    cachedToday = Date.new(14,3,2002)
    cachedSettlement = Date.new(18,3,2002)
    @termStructure.linkTo!(FlatForward.new(cachedToday,cachedSettlement,
                                           0.05, DayCounter.new('Act/360')))
    startDate = @termStructure.referenceDate
    leg = makeLeg(startDate,20)
    cap = makeCapFloor(Cap,leg,0.07,0.20)
    floor = makeCapFloor(Floor,leg,0.03,0.20)
    cachedCapNPV = 6.958278080775
    cachedFloorNPV = 2.700476857631

    unless (cap.NPV-cachedCapNPV).abs <= 1.0e-11
      flunk(<<-MESSAGE

    failed to reproduce cached cap value:
        calculated: #{cap.NPV}
        expected:   #{cachedCapNPV}

            MESSAGE
            )
    end
    unless (floor.NPV-cachedFloorNPV).abs <= 1.0e-11
      flunk(<<-MESSAGE

    failed to reproduce cached floor value:
        calculated: #{floor.NPV}
        expected:   #{cachedFloorNPV}

            MESSAGE
            )
    end
  end
end

if $0 == __FILE__
  Test::Unit::UI::Console::TestRunner.run(CapFloorTest)
end


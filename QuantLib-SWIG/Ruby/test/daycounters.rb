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

class DayCounterTest < RUNIT::TestCase
  def name
    "Testing actual/actual day counters..."
  end
  def test
    # each case lists startDate, endDate, refStartDate, refEndDate, and
    # the results for ISDA, ISMA and AFB, respectively
    cases = [
      # first example
      [QuantLib::Date.new( 1,11,2003), QuantLib::Date.new( 1, 5,2004),
       QuantLib::Date.new( 1,11,2003), QuantLib::Date.new( 1, 5,2004),
       [0.497724380567, 0.5,            0.497267759563]],
      # short first calculation period (first period)
      [QuantLib::Date.new( 1, 2,1999), QuantLib::Date.new( 1, 7,1999),
       QuantLib::Date.new( 1, 7,1998), QuantLib::Date.new( 1, 7,1999),
       [0.410958904110, 0.410958904110, 0.410958904110]],
      # short first calculation period (second period)
      [QuantLib::Date.new( 1, 7,1999), QuantLib::Date.new( 1, 7,2000),
       QuantLib::Date.new( 1, 7,1999), QuantLib::Date.new( 1, 7,2000),
       [1.001377348600, 1.0,            1.0]],
      # long first calculation period (first period)
      [QuantLib::Date.new(15, 8,2002), QuantLib::Date.new(15, 7,2003),
       QuantLib::Date.new(15, 1,2003), QuantLib::Date.new(15, 7,2003),
       [0.915068493151, 0.915760869565, 0.915068493151]],
      # long first calculation period (second period)
      #### the ISDA case is in disagreement with mktc1198.pdf !!!!
      [QuantLib::Date.new(15, 7,2003), QuantLib::Date.new(15, 1,2004),
       QuantLib::Date.new(15, 7,2003), QuantLib::Date.new(15, 1,2004),
       [0.504004790778, 0.5,            0.504109589041]],
      # short final calculation period (penultimate period)
      [QuantLib::Date.new(30, 7,1999), QuantLib::Date.new(30, 1,2000),
       QuantLib::Date.new(30, 7,1999), QuantLib::Date.new(30, 1,2000),
       [0.503892506924, 0.5,            0.504109589041]],
      # short final calculation period (final period)
      [QuantLib::Date.new(30, 1,2000), QuantLib::Date.new(30, 6,2000),
       QuantLib::Date.new(30, 1,2000), QuantLib::Date.new(30, 7,2000),
       [0.415300546448, 0.417582417582, 0.415300546448]]
    ]
    
    isda = QuantLib::DayCounter.new('act/act(h)')
    isma = QuantLib::DayCounter.new('act/act')
    afb  = QuantLib::DayCounter.new('act/act(e)')

    cases.each { |d1,d2,refStart,refEnd,(isdaValue,ismaValue,afbValue)|

      value = isda.yearFraction(d1,d2)
      unless (value-isdaValue).abs <= 1.0e-10
        assert_fail(<<-MESSAGE

    ISDA day counter:
        first date        #{d1}
        second date       #{d2}
        expected value:   #{isdaValue}
        calculated value: #{value}

                    MESSAGE
                    )
      end

      value = isma.yearFraction(d1,d2,refStart,refEnd)
      unless (value-ismaValue).abs <= 1.0e-10
        assert_fail(<<-MESSAGE

    ISMA day counter:
        first date        #{d1}
        second date       #{d2}
        first ref.date    #{refStart}
        second ref.date   #{refEnd}
        expected value:   #{ismaValue}
        calculated value: #{value}

                    MESSAGE
                    )
      end

      value = afb.yearFraction(d1,d2)
      unless (value-afbValue).abs <= 1.0e-10
        assert_fail(<<-MESSAGE

    AFB day counter:
        first date        #{d1}
        second date       #{d2}
        expected value:   #{afbValue}
        calculated value: #{value}

                    MESSAGE
                    )
      end
    }
  end
end

if $0 == __FILE__
  RUNIT::CUI::TestRunner.run(DayCounterTest.suite)
end


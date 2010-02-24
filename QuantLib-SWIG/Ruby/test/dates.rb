=begin
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl

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
require 'test/unit'
require 'test/unit/ui/console/testrunner'

class DateTest < Test::Unit::TestCase
  def name
    "Testing date ranges"
  end
  def testAllDates
    
    minDate = QuantLib::Date.minDate
    maxDate = QuantLib::Date.maxDate

    dold   = minDate.dayOfMonth
    mold   = minDate.month
    yold   = minDate.year
    
    (minDate+1...maxDate).each do |date|

      d   = date.dayOfMonth
      m   = date.month
      y   = date.year

      # check if skipping any date
      unless (d==dold+1 and m==mold   and y==yold  ) \
          or (d==1      and m==mold+1 and y==yold  ) \
          or (d==1      and m==1      and y==yold+1)
        flunk(<<-MESSAGE

    wrong day, month, year increment
        date: #{t}
        day, month, year: #{d}, #{m}, #{y}
        previous:         #{dold}, #{mold}, #yold}

              MESSAGE
              )
      end
      dold = d
      mold = m
      yold = y

    end
  end
end

if $0 == __FILE__
  Test::Unit::UI::Console::TestRunner.run(DateTest)
end


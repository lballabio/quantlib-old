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
require 'test/unit'
require 'test/unit/ui/console/testrunner'

class DateTest < Test::Unit::TestCase
  def setup
    puts
    print "Testing dates.."
    STDOUT.flush
  end
  def testAllDates
    minDate = QuantLib::Date.minDate.serialNumber
    maxDate = QuantLib::Date.maxDate.serialNumber
    
    dyold  = QuantLib::Date.new(minDate-1).dayOfYear
    dold   = QuantLib::Date.new(minDate-1).dayOfMonth
    mold   = QuantLib::Date.new(minDate-1).month
    yold   = QuantLib::Date.new(minDate-1).year
    wdnold = QuantLib::Date.new(minDate-1).weekdayNumber
    
    minDate.upto(maxDate) do |i|
      t = QuantLib::Date.new(i)
      # check serial number consistency
      unless t.serialNumber == i
        flunk(<<-MESSAGE
                    
    inconsistent serial number:
        original:      #{i}
        date:          #{t}
        serial number: #{t.serialNumber}

              MESSAGE
              )
      end

      dy  = t.dayOfYear
      d   = t.dayOfMonth
      m   = t.month
      y   = t.year
      mm  = t.month
      wd  = t.weekday
      wdn = t.weekdayNumber

      # check if skipping any date
      unless dy==dyold+1 \
        or (dy==1 and dyold==365 and not QuantLib::Date.isLeap(yold)) \
        or (dy==1 and dyold==366 and QuantLib::Date.isLeap(yold))
        flunk(<<-MESSAGE

    wrong day of year increment:
        date: #{t}
        day of year: #{dy}
        previous:    #{dyold}

              MESSAGE
              )
      end
      dyold = dy

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

      # check month definition
      unless m>=1 and m<=12
        flunk(<<-MESSAGE

    invalid month
        date: #{t}
        month: #{m}

              MESSAGE
              )
      end

      # check day definition
      unless d >= 1
        flunk(<<-MESSAGE

    invalid day of month
        date: #{t}
        day: #{d}

              MESSAGE
              )
      end

      unless (m==1  and d<=31) \
        or (m==2  and d<=28) \
        or (m==2  and d==29 and QuantLib::Date.isLeap(y)) \
        or (m==3  and d<=31) \
        or (m==4  and d<=30) \
        or (m==5  and d<=31) \
        or (m==6  and d<=30) \
        or (m==7  and d<=31) \
        or (m==8  and d<=31) \
        or (m==9  and d<=30) \
        or (m==10 and d<=31) \
        or (m==11 and d<=30) \
        or (m==12 and d<=31)
        flunk(<<-MESSAGE

    invalid day of month
        date: #{t}
        day: #{d}
        month: #{mm}

              MESSAGE
              )
      end

      # check weekdayNumber definition
      unless wdn==wdnold+1 or (wdn==1 and wdnold==7)
        flunk(<<-MESSAGE

    wrong weekday number increment
        date: #{t}
        weekday number: #{wdn}
        previous:       #{wdnold}

              MESSAGE
              )
      end
      wdnold=wdn

      # create the same date with a different constructor
      s = QuantLib::Date.new(d,m,y)
      # check serial number consistency
      unless s.serialNumber == i
        flunk(<<-MESSAGE

    inconsistent serial number
        date: #{t}
        serial number: #{i}
        cloned date: #{s}
        serial number: #{s.serialNumber}

              MESSAGE
              )
      end

      # create the same date with yet another constructor
      s = QuantLib::Date.new(d,mm,y)
      # check serial number consistency
      unless s.serialNumber == i
        flunk(<<-MESSAGE

    inconsistent serial number
        date: #{t}
        serial number: #{i}
        cloned date: #{s}
        serial number: #{s.serialNumber}

              MESSAGE
              )
      end
    end
  end
end

if $0 == __FILE__
  Test::Unit::UI::Console::TestRunner.run(DateTest)
end


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

class CalendarTest < Test::Unit::TestCase
  def name
    "Testing joint calendars"
  end
  def testJointCalendars
    c1 = QuantLib::Calendar.new("TARGET")
    c2 = QuantLib::Calendar.new("London")
    c3 = QuantLib::Calendar.new("NewYork")
    c4 = QuantLib::Calendar.new("Tokyo")

    c12h = QuantLib::JointCalendar.new(c1,c2,'JoinHolidays')
    c12b = QuantLib::JointCalendar.new(c1,c2,'JoinBusinessDays')
    c123h = QuantLib::JointCalendar.new(c1,c2,c3,'JoinHolidays')
    c123b = QuantLib::JointCalendar.new(c1,c2,c3,'JoinBusinessDays')
    c1234h = QuantLib::JointCalendar.new(c1,c2,c3,c4,'JoinHolidays')
    c1234b = QuantLib::JointCalendar.new(c1,c2,c3,c4,'JoinBusinessDays')

    firstDate = QuantLib::Date.todaysDate
    endDate = firstDate.plusYears(1)

    (firstDate...endDate).each do |d|

      b1 = c1.isBusinessDay?(d)
      b2 = c2.isBusinessDay?(d)
      b3 = c3.isBusinessDay?(d)
      b4 = c4.isBusinessDay?(d)
      
      unless (b1 && b2) == c12h.isBusinessDay?(d)
        flunk(<<-MESSAGE

    At date #{d}:
        inconsistency between joint calendar #{c12h.name}
        and its components

              MESSAGE
              )
      end

      unless (b1 || b2) == c12b.isBusinessDay?(d)
        flunk(<<-MESSAGE

    At date #{d}:
        inconsistency between joint calendar #{c12b.name}
        and its components

              MESSAGE
              )
      end

      unless (b1 && b2 && b3) == c123h.isBusinessDay?(d)
        flunk(<<-MESSAGE

    At date #{d}:
        inconsistency between joint calendar #{c123h.name}
        and its components

              MESSAGE
              )
      end

      unless (b1 || b2 || b3) == c123b.isBusinessDay?(d)
        flunk(<<-MESSAGE

    At date #{d}:
        inconsistency between joint calendar #{c123b.name}
        and its components

              MESSAGE
              )
      end

      unless (b1 && b2 && b3 && b4) == c1234h.isBusinessDay?(d)
        flunk(<<-MESSAGE

    At date #{d}:
        inconsistency between joint calendar #{c1234h.name}
        and its components

              MESSAGE
              )
      end

      unless (b1 || b2 || b3 || b4) == c1234b.isBusinessDay?(d)
        flunk(<<-MESSAGE

    At date #{d}:
        inconsistency between joint calendar #{c1234b.name}
        and its components

              MESSAGE
              )
      end
    end
  end
end


if $0 == __FILE__
  Test::Unit::UI::Console::TestRunner.run(CalendarTest)
end


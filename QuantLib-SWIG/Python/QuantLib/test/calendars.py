"""
 Copyright (C) 2003 RiskMap srl

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it under the
 terms of the QuantLib license.  You should have received a copy of the
 license along with this program; if not, please email ferdinando@ametrano.net
 The license is also available online at http://quantlib.org/html/license.html

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
"""

__version__ = "$Revision$"
# $Id$

import QuantLib
from QuantLib import Date, Calendar, JointCalendar
import unittest

class CalendarTest(unittest.TestCase):
    def runTest(self):
        "Testing joint calendars"
        c1 = Calendar('TARGET')
        c2 = Calendar('London')
        c3 = Calendar('NewYork')
        c4 = Calendar('Tokyo')

        c12h = JointCalendar(c1,c2,'JoinHolidays')
        c12b = JointCalendar(c1,c2,'JoinBusinessDays')
        c123h = JointCalendar(c1,c2,c3,'JoinHolidays')
        c123b = JointCalendar(c1,c2,c3,'JoinBusinessDays')
        c1234h = JointCalendar(c1,c2,c3,c4,'JoinHolidays')
        c1234b = JointCalendar(c1,c2,c3,c4,'JoinBusinessDays')

        firstDate = QuantLib.Date_todaysDate()
        endDate = firstDate.plusYears(1)

        d = firstDate
        while d < endDate:
            b1 = c1.isBusinessDay(d)
            b2 = c2.isBusinessDay(d)
            b3 = c3.isBusinessDay(d)
            b4 = c4.isBusinessDay(d)

            if (b1 and b2) != c12h.isBusinessDay(d):
                self.fail("""
At date %s:
    inconsistency between joint calendar %s
    and its components
                          """ % (d,c12h.name()))

            if (b1 or b2) != c12b.isBusinessDay(d):
                self.fail("""
At date %s:
    inconsistency between joint calendar %s
    and its components
                          """ % (d,c12b.name()))

            if (b1 and b2 and b3) != c123h.isBusinessDay(d):
                self.fail("""
At date %s:
    inconsistency between joint calendar %s
    and its components
                          """ % (d,c123h.name()))

            if (b1 or b2 or b3) != c123b.isBusinessDay(d):
                self.fail("""
At date %s:
    inconsistency between joint calendar %s
    and its components
                          """ % (d,c123b.name()))

            if (b1 and b2 and b3 and b4) != c1234h.isBusinessDay(d):
                self.fail("""
At date %s:
    inconsistency between joint calendar %s
    and its components
                          """ % (d,c1234h.name()))

            if (b1 or b2 or b3 or b4) != c1234b.isBusinessDay(d):
                self.fail("""
At date %s:
    inconsistency between joint calendar %s
    and its components
                          """ % (d,c1234b.name()))

            d = d+1


if __name__ == '__main__':
    print 'testing QuantLib', QuantLib.__version__
    suite = unittest.TestSuite()
    suite.addTest(CalendarTest())
    unittest.TextTestRunner(verbosity=2).run(suite)
    raw_input('press return to continue')


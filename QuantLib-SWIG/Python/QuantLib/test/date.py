"""
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
"""

__version__ = "$Revision$"
# $Id$

import QuantLib
import unittest

class DateTest(unittest.TestCase):
    def runTest(self):
        "Testing dates"
        mindate = QuantLib.Date_minDate().serialNumber()
        maxdate = QuantLib.Date_maxDate().serialNumber() + 1 #excluded

        dyold  = QuantLib.DateFromSerialNumber(mindate-1).dayOfYear()
        dold   = QuantLib.DateFromSerialNumber(mindate-1).dayOfMonth()
        mold   = QuantLib.DateFromSerialNumber(mindate-1).month()
        yold   = QuantLib.DateFromSerialNumber(mindate-1).year()
        wdnold = QuantLib.DateFromSerialNumber(mindate-1).weekdayNumber()

        for i in range(mindate,maxdate):
            t = QuantLib.DateFromSerialNumber(i)
            serial = t.serialNumber()
            # check serial number consistency
            if not (serial == i):
                self.fail("""
inconsistent serial number:
    original:      %(i)d
    date:          %(t)s
    serial number: %(serial)d
                """ % locals())

            dy  = t.dayOfYear()
            d   = t.dayOfMonth()
            m   = t.month()
            y   = t.year()
            mm  = t.month()
            wd  = t.weekday()
            wdn = t.weekdayNumber()

            # check if skipping any date
            if not ((dy==dyold+1) or
                    (dy==1 and dyold==365
                           and not QuantLib.Date_isLeap(yold)) or
                    (dy==1 and dyold==366 and QuantLib.Date_isLeap(yold))):
                self.fail("""
wrong day of year increment:
    date: %(t)s
    day of year: %(dy)d
    previous:    %(dyold)d
                """ % locals())
            dyold = dy

            # check if skipping any date
            if not ((d==dold+1 and m==mold      and y==yold  ) or
                    (d==1      and m==mold+1    and y==yold  ) or
                    (d==1      and m==1         and y==yold+1)):
                self.fail("""
wrong day, month, year increment
    date: %(t)s
    day, month, year: %(d)d, %(m)d, %(y)d
    previous:         %(dold)d, %(mold)d, %(yold)d
                """ % locals())
            dold = d
            mold = m
            yold = y

            # check month definition
            if not (m>=1 and  m<=12):
                self.fail("""
invalid month
    date: %(t)s
    month: %(m)d
                """ % locals())

            # check day definition
            if not (d >= 1):
                self.fail("""
invalid day of month
    date: %(t)s
    day: %(d)d
                          """ % locals())

            # check day definition
            if not ((m==1   and d<=31) or
                    (m==2   and d<=28) or
                    (m==2   and d==29 and QuantLib.Date_isLeap(y)) or
                    (m==3   and d<=31) or
                    (m==4   and d<=30) or
                    (m==5   and d<=31) or
                    (m==6   and d<=30) or
                    (m==7   and d<=31) or
                    (m==8   and d<=31) or
                    (m==9   and d<=30) or
                    (m==10  and d<=31) or
                    (m==11  and d<=30) or
                    (m==12  and d<=31)):
                self.fail("""
invalid day of month
    date: %(t)s
    day: %(d)d
    month: %(mm)s
                          """ % locals())

            # check weekdayNumber definition
            if not (wdn==wdnold+1 or (wdn==1 and wdnold==7)):
                self.fail("""
wrong weekday number increment
    date: %(t)s
    weekday number: %(wdn)d
    previous:       %(wdnold)d
                          """ % locals())
            wdnold=wdn

            # create the same date with a different constructor
            s = QuantLib.Date(d,m,y)
            # check serial number consistency
            serial = s.serialNumber()
            if not (serial==i):
                self.fail("""
inconsistent serial number
    date: %(t)s
    serial number: %(i)d
    cloned date: %(s)s
    serial number: %(serial)d
                          """ % locals())

            # create the same date with a different constructor
            s = QuantLib.Date(d,mm,y)
            # check serial number consistency
            serial = s.serialNumber()
            if not (serial==i):
                self.fail("""
inconsistent serial number
    date: %(t)s
    serial number: %(i)d
    cloned date: %(s)s
    serial number: %(serial)d
	                      """ % locals())


if __name__ == '__main__':
    print 'testing QuantLib', QuantLib.__version__, 
    suite = unittest.TestSuite()
    suite.addTest(DateTest())
    unittest.TextTestRunner(verbosity=2).run(suite)
    raw_input('press return to continue')


"""
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it under the
 terms of the QuantLib license.  You should have received a copy of the
 license along with this program; if not, please email quantlib-dev@lists.sf.net
 The license is also available online at http://quantlib.org/html/license.html

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
"""

import QuantLib
import unittest

class DateTest(unittest.TestCase):
    def runTest(self):
        "Testing date arithmetics"
        date = QuantLib.Date_minDate()

        dold   = date.dayOfMonth()
        mold   = date.month()
        yold   = date.year()

        while date < QuantLib.Date_maxDate():
            date += 1

            d   = date.dayOfMonth()
            m   = date.month()
            y   = date.year()

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


if __name__ == '__main__':
    print 'testing QuantLib', QuantLib.__version__, 
    suite = unittest.TestSuite()
    suite.addTest(DateTest())
    unittest.TextTestRunner(verbosity=2).run(suite)
    raw_input('press return to continue')


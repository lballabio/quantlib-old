"""
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
"""

__version__ = "$Revision$"
# $Id$

# see http://www.isda.org/c_and_a/pdf/mktc1198.pdf for reference

import QuantLib
from QuantLib import Date, DayCounter
import unittest

class DayCounterTest(unittest.TestCase):
    def runTest(self):
        "Testing actual/actual day counters"
        ISDA_H = DayCounter('actacth')
        ISMA_B = DayCounter('actact')
        AFB__E = DayCounter('actacte')

        ISDAcases =[]
        ISMAcases =[]
        AFB_cases =[]
        # first example
        ISDAcases.append((0.497724380567, Date( 1,11,2003), Date( 1, 5,2004)))
        ISMAcases.append((0.500000000000, Date( 1,11,2003), Date( 1, 5,2004), \
            Date( 1,11,2003), Date( 1, 5,2004)))
        AFB_cases.append((0.497267759563, Date( 1,11,2003), Date( 1, 5,2004)))
        # short first calculation period (first period)
        ISDAcases.append((0.410958904110, Date( 1, 2,1999), Date( 1, 7,1999)))
        ISMAcases.append((0.410958904110, Date( 1, 2,1999), Date( 1, 7,1999), \
            Date( 1, 7,1998), Date( 1, 7,1999)))
        AFB_cases.append((0.410958904110, Date( 1, 2,1999), Date( 1, 7,1999)))
        # short first calculation period (second period)
        ISDAcases.append((1.001377348600, Date( 1, 7,1999), Date( 1, 7,2000)))
        ISMAcases.append((1.000000000000, Date( 1, 7,1999), Date( 1, 7,2000), \
            Date( 1, 7,1999), Date( 1, 7,2000)))
        AFB_cases.append((1.000000000000, Date( 1, 7,1999), Date( 1, 7,2000)))
        # long first calculation period (first period)
        ISDAcases.append((0.915068493151, Date(15, 8,2002), Date(15, 7,2003)))
        ISMAcases.append((0.915760869565, Date(15, 8,2002), Date(15, 7,2003), \
            Date(15, 1,2003), Date(15, 7,2003)))
        AFB_cases.append((0.915068493151, Date(15, 8,2002), Date(15, 7,2003)))
        # long first calculation period (second period)
        #### the following ISDA case is in disagreement with mktc1198.pdf !!!!
        ISDAcases.append((0.504004790778, Date(15, 7,2003), Date(15, 1,2004)))
        ISMAcases.append((0.500000000000, Date(15, 7,2003), Date(15, 1,2004), \
            Date(15, 7,2003), Date(15, 1,2004)))
        AFB_cases.append((0.504109589041, Date(15, 7,2003), Date(15, 1,2004)))
        # short final calculation period (penultimate period)
        ISDAcases.append((0.503892506924, Date(30, 7,1999), Date(30, 1,2000)))
        ISMAcases.append((0.500000000000, Date(30, 7,1999), Date(30, 1,2000), \
            Date(30, 7,1999), Date(30, 1,2000)))
        AFB_cases.append((0.504109589041, Date(30, 7,1999), Date(30, 1,2000)))
        # short final calculation period (final period)
        ISDAcases.append((0.415300546448, Date(30, 1,2000), Date(30, 6,2000)))
        ISMAcases.append((0.417582417582, Date(30, 1,2000), Date(30, 6,2000), \
            Date(30, 1,2000), Date(30, 7,2000)))
        AFB_cases.append((0.415300546448, Date(30, 1,2000), Date(30, 6,2000)))

        for (value, d1, d2) in ISDAcases:
            calculated = ISDA_H.yearFraction(d1, d2)
            if not (abs(calculated-value)<1e-10):
                self.fail("""
%(ISDA_H)s
    first date        %(d1)s
    second date       %(d2)s
    original value:   %(value)g
    calculated value: %(calculated)g
                          """ % locals())

        for (value, d1, d2, rd1, rd2) in ISMAcases:
            calculated = ISMA_B.yearFraction(d1, d2, rd1, rd2)
            if not (abs(calculated-value)<1e-10):
                self.fail("""
%(ISMA_B)s
    first date            %(d1)s
    second date           %(d2)s
    first reference date  %(rd1)s
    second reference date %(rd2)s
    original value:       %(value)g
    calculated value:     %(calculated)g
                          """ % locals())

        for (value, d1, d2) in AFB_cases:
            calculated = AFB__E.yearFraction(d1, d2)
            if not (abs(calculated-value)<1e-10):
                self.fail("""
%(AFB__E)s
    first date        %(d1)s
    second date       %(d2)s
    original value:   %(value)g
    calculated value: %(calculated)g
	                      """ % locals())



if __name__ == '__main__':
    print 'testing QuantLib', QuantLib.__version__
    suite = unittest.TestSuite()
    suite.addTest(DayCounterTest())
    unittest.TextTestRunner(verbosity=2).run(suite)
    raw_input('press return to continue')


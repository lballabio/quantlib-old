"""
 Copyright (C) 2009 Joseph Malicki

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
"""

import QuantLib
import unittest

class FixedRateBondHelperTest(unittest.TestCase):
    def setUp(self):
        self.settlement_days = 3
        self.face_amount = 100.0
        self.redemption = 100.0
        self.quote_handle = QuantLib.QuoteHandle(QuantLib.SimpleQuote(100.0))

        self.issue_date = QuantLib.Date(2,1,2008)
        self.maturity_date = QuantLib.Date(2,1,2018)
        self.calendar = QuantLib.UnitedStates(QuantLib.UnitedStates.GovernmentBond)
        self.day_counter = QuantLib.ActualActual(QuantLib.ActualActual.Bond)
        self.sched = QuantLib.Schedule(self.issue_date, self.maturity_date,
                                       QuantLib.Period(QuantLib.Semiannual), self.calendar,
                                       QuantLib.Unadjusted, QuantLib.Unadjusted,
                                       QuantLib.DateGeneration.Backward, False)
        self.coupons = [0.05]

        self.bond_helper = QuantLib.FixedRateBondHelper(self.quote_handle, self.settlement_days,
                                                        self.face_amount, self.sched, self.coupons,
                                                        self.day_counter, QuantLib.Following,
                                                        self.redemption, self.issue_date)
    def testBond(self):
        """ Testing FixedRateBondHelper bond() method. """
        bond = self.bond_helper.bond()
        self.assertEqual(bond.issueDate(), self.issue_date)
        self.assertEqual(bond.nextCouponRate(), self.coupons[0])

if __name__ == '__main__':
    print 'testing QuantLib', QuantLib.__version__
    suite = unittest.TestSuite()
    suite.addTest(unittest.makeSuite(FixedRateBondHelperTest,'test'))
    unittest.TextTestRunner(verbosity=2).run(suite)
    raw_input('press return to continue')

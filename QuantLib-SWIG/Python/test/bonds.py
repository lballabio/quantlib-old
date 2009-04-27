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

class FixedRateBondTest(unittest.TestCase):
    def setUp(self):
        self.settlement_days = 3
        self.face_amount = 100.0
        self.redemption = 100.0
        self.issue_date = QuantLib.Date(2,1,2008)
        self.maturity_date = QuantLib.Date(2,1,2018)
        self.calendar = QuantLib.UnitedStates(QuantLib.UnitedStates.GovernmentBond)
        self.day_counter = QuantLib.ActualActual(QuantLib.ActualActual.Bond)
        self.sched = QuantLib.Schedule(self.issue_date, self.maturity_date,
                                       QuantLib.Period(QuantLib.Semiannual), self.calendar,
                                       QuantLib.Unadjusted, QuantLib.Unadjusted,
                                       QuantLib.DateGeneration.Backward, False)
        self.coupons = [0.05]

        self.bond = QuantLib.FixedRateBond(self.settlement_days, self.face_amount,
                                           self.sched, self.coupons, self.day_counter,
                                           QuantLib.Following, self.redemption,
                                           self.issue_date)

        self.flat_forward = QuantLib.FlatForward(self.issue_date+self.settlement_days*QuantLib.Days,
                                            self.coupons[0], self.day_counter,
                                            QuantLib.Compounded, QuantLib.Semiannual)
        self.term_structure_handle = QuantLib.RelinkableYieldTermStructureHandle(self.flat_forward)
        bondEngine = QuantLib.DiscountingBondEngine(self.term_structure_handle)
        self.bond.setPricingEngine(bondEngine)

    def testFrequency(self):
        """ Testing FixedRateBond frequency() method. """
        self.assertEqual(self.bond.frequency(), QuantLib.Semiannual)

    def testDayCounter(self):
        """ Testing FixedRateBond dayCounter() method. """
        self.assertEqual(self.bond.dayCounter(), self.day_counter)

    def testSimpleInspectors(self):
        """ Testing FixedRateBond simple inspectors. """
        self.assertEqual(self.bond.settlementDays(), self.settlement_days)
        self.assertEqual(self.bond.notional(), self.face_amount)
        self.assertEqual(self.bond.issueDate(), self.issue_date)
        self.assertEqual(self.bond.maturityDate(), self.maturity_date)

    #def testSettlementValue(self):
    #    """ Testing FixedRateBond settlement value. """
    #    orig_date = QuantLib.Settings.evaluationDate
    #    QuantLib.Settings.evaluationDate = self.issue_date + 1*QuantLib.Months
    #    self.assertEqual(round(self.bond.settlementValue(100.0), 4), 102.3098)
    #    QuantLib.Settings.evaluationDate = orig_date

    def testCashFlows(self):
        """ Testing that the FixedRateBond gives the expected cash flows. """
        self.assertEqual([round(cf.amount(), 4) for cf in self.bond.cashflows()],
                         20*[round(self.face_amount * self.coupons[0] / 2, 4)] + \
                         [round(self.redemption, 4)])

    def testRedemption(self):
        """ Testing FixedRateBond redemption value and date. """
        self.assertEqual(self.bond.redemption().date(), self.maturity_date)
        self.assertEqual(self.bond.redemption().amount(), self.redemption)

    def testRedemptions(self):
        """ Testing FixedRateBond redemptions. """
        redemptions = self.bond.redemptions()
        self.assertEqual(len(redemptions), 1)
        self.assertEqual(redemptions[0].date(), self.maturity_date)
        self.assertEqual(redemptions[0].amount(), self.redemption)

    def testNotional(self):
        """ Testing FixedRateBond notional values. """
        self.assertEqual(self.bond.notional(), 100.0)
        self.assertEqual(self.bond.notionals(), (100.0, 0))

    def testNextCoupon(self):
        """ Testing FixedRateBond correct next coupon amount. """
        self.assertEqual(self.bond.nextCouponRate(self.issue_date), 0.05)

    def testPrevCoupon(self):
        """ Testing FixedRateBond correct previous coupon amount. """
        self.assertEqual(self.bond.previousCouponRate(self.issue_date), 0.05)

    def testCleanPrice(self):
        """ Testing FixedRateBond clean price. """
        self.assertEqual(round(self.bond.cleanPrice(0.05, self.day_counter, QuantLib.Compounded,
                                                    QuantLib.Semiannual, self.issue_date +
                                                    self.settlement_days*QuantLib.Days), 4),
                         100.0)
        self.assertEqual(round(self.bond.cleanPrice(0.05, self.day_counter, QuantLib.Compounded,
                                                    QuantLib.Semiannual, self.issue_date +
                                                    self.settlement_days*QuantLib.Days +
                                                    1*QuantLib.Months), 4),
                         99.9997)

        self.assertEqual(round(self.bond.cleanPrice(0.06, self.day_counter, QuantLib.Compounded,
                                                    QuantLib.Semiannual, self.issue_date +
                                                    self.settlement_days*QuantLib.Days +
                                                    1*QuantLib.Months), 4),
                         92.5639)


    def testDirtyPrice(self):
        """ Testing FixedRateBond dirty price. """
        self.assertEqual(round(self.bond.dirtyPrice(0.05, self.day_counter, QuantLib.Compounded,
                                                    QuantLib.Semiannual, self.issue_date +
                                                    self.settlement_days*QuantLib.Days), 4),
                         100.0)
        self.assertEqual(round(self.bond.dirtyPrice(0.05, self.day_counter, QuantLib.Compounded,
                                                    QuantLib.Semiannual, self.issue_date +
                                                    self.settlement_days*QuantLib.Days +
                                                    1*QuantLib.Months), 4),
                         100.0271)
        self.assertEqual(round(self.bond.dirtyPrice(0.06, self.day_counter, QuantLib.Compounded,
                                                    QuantLib.Semiannual, self.issue_date +
                                                    self.settlement_days*QuantLib.Days +
                                                    1*QuantLib.Months), 4),
                         92.5913)

    def testCleanPriceFromZSpread(self):
        """ Testing FixedRateBond clean price derived from Z-spread. """
        self.assertEqual(round(QuantLib.cleanPriceFromZSpread(
                    self.bond, self.flat_forward, 0.01,
                    self.day_counter, QuantLib.Compounded, QuantLib.Semiannual,
                    self.issue_date + 1 * QuantLib.Months), 4), 92.5637)

if __name__ == '__main__':
    print 'testing QuantLib', QuantLib.__version__
    suite = unittest.TestSuite()
    suite.addTest(unittest.makeSuite(FixedRateBondTest,'test'))
    unittest.TextTestRunner(verbosity=2).run(suite)
    raw_input('press return to continue')

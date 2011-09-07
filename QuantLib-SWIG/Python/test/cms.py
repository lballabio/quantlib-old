"""
 Copyright (C) 2011 Lluis Pujol Bajador

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

from QuantLib import *
import unittest

class CmsTest(unittest.TestCase):
    def setUp(self):
        #global data
        self.calendar = TARGET()
        self.referenceDate = self.calendar.adjust(Date.todaysDate())
        Settings.instance().evaluationDate = self.referenceDate
        self.termStructure = RelinkableYieldTermStructureHandle()
        self.termStructure.linkTo(FlatForward(self.referenceDate,
                             QuoteHandle(SimpleQuote(0.05)), Actual365Fixed()))
        self.yieldCurveModels = []
        self.numericalPricers = []
        self.analyticPricers = []

        # ATM Volatility structure
        self.atmOptionTenors = [
            Period(1, Months),
            Period(6, Months),
            Period(1, Years),
            Period(5, Years),
            Period(10, Years),
            Period(30, Years)
        ]

        self.atmSwapTenors = [
            Period(1, Years),
            Period(5, Years),
            Period(10, Years),
            Period(30, Years)
        ]

        self.m = [[0.1300, 0.1560, 0.1390, 0.1220],
                  [0.1440, 0.1580, 0.1460, 0.1260],
                  [0.1600, 0.1590, 0.1470, 0.1290],
                  [0.1640, 0.1470, 0.1370, 0.1220],
                  [0.1400, 0.1300, 0.1250, 0.1100],
                  [0.1130, 0.1090, 0.1070, 0.0930]]

        self.atmVol = SwaptionVolatilityStructureHandle(
                SwaptionVolatilityMatrix(self.calendar,
                                         Following,
                                         self.atmOptionTenors,
                                         self.atmSwapTenors,
                                         Matrix(self.m),
                                         Actual365Fixed()))

        ###Vol cubes
        self.optionTenors = [
            Period(1, Years),
            Period(10, Years),
            Period(30, Years)
        ]

        self.swapTenors = [
            Period(2, Years),
            Period(10, Years),
            Period(30, Years)
        ]

        self.strikeSpreads = [
            -0.020,
            -0.005,
            +0.000,
            +0.005,
            +0.020
        ]

        self.nRows = len(self.optionTenors)*len(self.swapTenors)
        self.nCols = len(self.strikeSpreads)
        self.volSpreadsMatrix = [[0.0599,0.0049,0.0000,-0.0001,0.0127],
                                 [0.0729,0.0086,0.0000,-0.0024,0.0098],
                                 [0.0738,0.0102,0.0000,-0.0039,0.0065],
                                 [0.0465,0.0063,0.0000,-0.0032,-0.0010],
                                 [0.0558,0.0084,0.0000,-0.0050,-0.0057],
                                 [0.0576,0.0083,0.0000,-0.0043,-0.0014],
                                 [0.0437,0.0059,0.0000,-0.0030,-0.0006],
                                 [0.0533,0.0078,0.0000,-0.0045,-0.0046],
                                 [0.0545,0.0079,0.0000,-0.0042,-0.0020]]

        self.volSpreads = []
        for i in range(self.nRows):
            self.volSpreadsRow = []
            for j in range(self.nCols):
                self.volSpreadsRow.append(
                    QuoteHandle(SimpleQuote(self.volSpreadsMatrix[i][j])))
            self.volSpreads.append(self.volSpreadsRow)

        self.iborIndex = Euribor6M(self.termStructure)
        self.swapIndexBase = EuriborSwapIsdaFixA(Period(10,Years),
                                                 self.termStructure)
        self.shortSwapIndexBase = EuriborSwapIsdaFixA(Period(2,Years),
                                                      self.termStructure)

        self.vegaWeightedSmileFit = False
        self.SabrVolCube2 = SwaptionVolatilityStructureHandle(
                SwaptionVolCube2(self.atmVol,
                                 self.optionTenors,
                                 self.swapTenors,
                                 self.strikeSpreads,
                                 self.volSpreads,
                                 self.swapIndexBase,
                                 self.shortSwapIndexBase,
                                 self.vegaWeightedSmileFit))
        self.SabrVolCube2.enableExtrapolation()

        self.guess = []
        self.guessMatrix = [[0.2,0.5,0.4,0.0],
                            [0.2,0.5,0.4,0.0],
                            [0.2,0.5,0.4,0.0],
                            [0.2,0.5,0.4,0.0],
                            [0.2,0.5,0.4,0.0],
                            [0.2,0.5,0.4,0.0],
                            [0.2,0.5,0.4,0.0],
                            [0.2,0.5,0.4,0.0],
                            [0.2,0.5,0.4,0.0]]

        for i in range(self.nRows):
            self.guessRow = []
            for j in range(4):
                self.guessRow.append(
                    QuoteHandle(SimpleQuote(self.guessMatrix[i][j])))
            self.guess.append(self.guessRow)

        self.isParameterFixed = [False,True,False,False]
        ### FIXME
        self.isAtmCalibrated = False
        ##
        self.SabrVolCube1 = SwaptionVolatilityStructureHandle(
                SwaptionVolCube1(self.atmVol,
                                 self.optionTenors,
                                 self.swapTenors,
                                 self.strikeSpreads,
                                 self.volSpreads,
                                 self.swapIndexBase,
                                 self.shortSwapIndexBase,
                                 self.vegaWeightedSmileFit,
                                 self.guess,
                                 self.isParameterFixed,
                                 self.isAtmCalibrated))
        ##SabrVolCube1.enableExtrapolation()

        self.yieldCurveModels = [
            GFunctionFactory.Standard,
            GFunctionFactory.ExactYield,
            GFunctionFactory.ParallelShifts,
            GFunctionFactory.NonParallelShifts
        ]

        self.zeroMeanRev = QuoteHandle(SimpleQuote(0.0))

        self.numericalPricers = {}
        self.analyticPricers = {}
        for m in self.yieldCurveModels:
            self.numericalPricers[m] = NumericHaganPricer(self.atmVol, m,
                                                          self.zeroMeanRev)
            self.analyticPricers[m] = AnalyticHaganPricer(self.atmVol, m,
                                                          self.zeroMeanRev)

    def testFairRate(self):
        """Testing Hagan-pricer flat-vol equivalence for coupons..."""
        swapIndex = SwapIndex("EuriborSwapIsdaFixA",
                              Period(10,Years),
                              self.iborIndex.fixingDays(),
                              self.iborIndex.currency(),
                              self.iborIndex.fixingCalendar(),
                              Period(1,Years),
                              Unadjusted,
                              self.iborIndex.dayCounter(),
                              self.iborIndex)
        startDate = self.termStructure.referenceDate() + Period(20,Years)
        paymentDate = startDate + Period(1,Years)
        endDate = paymentDate
        nominal = 1.0
        infiniteCap = nullDouble()
        infiniteFloor = nullDouble()
        gearing = 1.0
        spread = 0.0
        coupon = CappedFlooredCmsCoupon(paymentDate, nominal,
                                        startDate, endDate,
                                        swapIndex.fixingDays(), swapIndex,
                                        gearing, spread,
                                        infiniteCap, infiniteFloor,
                                        startDate, endDate,
                                        self.iborIndex.dayCounter(),False)

        for m in self.yieldCurveModels:
            self.numericalPricers[m].setSwaptionVolatility(self.atmVol)
            coupon.setPricer(self.numericalPricers[m])
            rate0 = coupon.rate()
            self.analyticPricers[m].setSwaptionVolatility(self.atmVol)
            coupon.setPricer(self.analyticPricers[m])
            rate1 = coupon.rate()
            difference =  abs(rate1-rate0)
            tol = 2.0e-4
            self.assertTrue(difference<tol)

    def testParity(self):
        """Testing put-call parity for capped-floored CMS coupons..."""
        swaptionVols = [
            self.atmVol,
            self.SabrVolCube1,
            self.SabrVolCube2
        ]
        swapIndex = EuriborSwapIsdaFixA(Period(10,Years),
                                        self.iborIndex.forwardingTermStructure())
        startDate = self.termStructure.referenceDate() + Period(20,Years)
        paymentDate = startDate + Period(1,Years)
        endDate = paymentDate
        nominal = 1.0
        infiniteCap = nullDouble()
        infiniteFloor = nullDouble()
        gearing = 1.0
        spread = 0.0
        discount = self.termStructure.discount(paymentDate)
        swaplet = CappedFlooredCmsCoupon(paymentDate, nominal,
                                         startDate, endDate,
                                         swapIndex.fixingDays(),
                                         swapIndex,
                                         gearing, spread,
                                         infiniteCap, infiniteFloor,
                                         startDate, endDate,
                                         self.iborIndex.dayCounter())
        strikes = [0.02,0.07]
        for k in strikes:
            caplet = CappedFlooredCmsCoupon(paymentDate, nominal,
                                            startDate, endDate,
                                            swapIndex.fixingDays(),
                                            swapIndex,
                                            gearing, spread,
                                            k, infiniteFloor,
                                            startDate, endDate,
                                            self.iborIndex.dayCounter())
            floorlet = CappedFlooredCmsCoupon(paymentDate, nominal,
                                              startDate, endDate,
                                              swapIndex.fixingDays(),
                                              swapIndex,
                                              gearing, spread,
                                              infiniteCap, k,
                                              startDate, endDate,
                                              self.iborIndex.dayCounter())
            for vol in swaptionVols:
                for m in self.yieldCurveModels:
                    self.numericalPricers[m].setSwaptionVolatility(vol)
                    self.analyticPricers[m].setSwaptionVolatility(vol)
                    pricers = [self.numericalPricers[m],self.analyticPricers[m]]
                    for p in pricers:
                        swaplet.setPricer(p)
                        caplet.setPricer(p)
                        floorlet.setPricer(p)
                        swapletPrice = (swaplet.price(self.termStructure)
                                        + swaplet.accrualPeriod()*k*discount)
                        capletPrice = caplet.price(self.termStructure)
                        floorletPrice = floorlet.price(self.termStructure)
                        difference = abs(capletPrice+floorletPrice-swapletPrice)
                        tol = 2.0e-5
                        self.assertTrue(difference<tol)

if __name__ == '__main__':
    print 'testing QuantLib', QuantLib.__version__
    suite = unittest.TestSuite()
    suite.addTest(unittest.makeSuite(CmsTest,'test'))
    unittest.TextTestRunner(verbosity=2).run(suite)
    raw_input('press return to continue')

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

from QuantLib import *
import unittest

class CapFloorTest(unittest.TestCase):
    def setUp(self):
        self.termStructure = TermStructureHandle()
        self.nominals = [100.0]
        self.rollingConvention = 'modifiedFollowing'
        self.frequency = 2
        self.index = Xibor('Euribor',12/self.frequency,'Months',
                           self.termStructure)
        self.calendar = self.index.calendar()
        self.settlementDays = 2
        self.today = self.calendar.roll(Date_todaysDate())
        self.settlement = self.calendar.advance(self.today,
                                                self.settlementDays,"days",
                                               'following')
        self.fixingDays = 2
        self.termStructure.linkTo(FlatForward(self.today, self.settlement,
                                              0.05, DayCounter('Act/360')))
    def makeLeg(self,startDate,length):
        endDate = self.calendar.advance(startDate,length,"years",
                                        self.rollingConvention)
        return FloatingRateCouponVector(self.nominals,startDate,endDate,
                                        self.frequency,self.calendar,
                                        self.rollingConvention,
                                        self.index, self.fixingDays)
    def makeCapFloor(self,kind,leg,strike,volatility):
        return kind(leg,[strike],
                    self.termStructure,
                    self.makeEngine(volatility))
    def makeEngine(self,volatility):
        return BlackCapFloorEngine(
                   BlackModel(
                       MarketElementHandle(
                           SimpleMarketElement(volatility)),
                       self.termStructure))
    # check 1
    def testStrikeDependency(self):
        "Testing cap/floor dependency on strike"
        cases = [ (length,vol,kind)
                   for length in [1, 2, 3, 5, 7, 10, 15, 20]
                   for vol in [0.01, 0.05, 0.10, 0.15, 0.20]
                   for kind in [Cap,Floor] ]
        startDate = self.termStructure.referenceDate()
        for (length,vol,kind) in cases:
            strikes = [0.03, 0.04, 0.05, 0.06, 0.07]
            values = []
            for s in strikes:
                leg = self.makeLeg(startDate,length)
                instrument = self.makeCapFloor(kind,leg,s,vol)
                values.append(instrument.NPV())

            if kind == Cap:
                # NPV must decrease with strike
                for i in range(1,len(values)):
                    if values[i] > values[i-1]:
                        self.fail("""
NPV is increasing with the strike in a cap:
    length: %d years
    volatility: %8.4f
    value: %8.4f at strike: %8.4f
    value: %8.4f at strike: %8.4f
                                  """ % (length,vol,
                                         values[i-1],strikes[i-1]*100,
                                         values[i],  strikes[i]*100))
            else:
                # NPV must increase with strike
                for i in range(1,len(values)):
                    if values[i] < values[i-1]:
                        self.fail("""
NPV is decreasing with the strike in a floor:
    length: %d years
    volatility: %8.4f
    value: %8.4f at strike: %8.4f
    value: %8.4f at strike: %8.4f
                                  """ % (length,vol,
                                         values[i-1],strikes[i-1]*100,
                                         values[i],  strikes[i]*100))
    # check 2
    def testConsistency(self):
        "Testing consistency between cap, floor and collar"
        cases = [ (length,capRate,floorRate,vol)
                   for length in [1, 2, 3, 5, 7, 10, 15, 20]
                   for capRate in [0.03, 0.04, 0.05, 0.06, 0.07]
                   for floorRate in [0.03, 0.04, 0.05, 0.06, 0.07]
                   for vol in [0.01, 0.05, 0.10, 0.15, 0.20] ]
        startDate = self.termStructure.referenceDate()
        for (length,capRate,floorRate,vol) in cases:
            leg = self.makeLeg(startDate,length)
            cap = self.makeCapFloor(Cap,leg,capRate,vol)
            floor = self.makeCapFloor(Floor,leg,floorRate,vol)
            collar = Collar(leg,[capRate],[floorRate],
                            self.termStructure,
                            self.makeEngine(vol))
            if abs((cap.NPV()-floor.NPV()) - collar.NPV()) > 1.0e-10:
                self.fail("""
inconsistency between cap, floor and collar:
    length      : %d years
    volatility  : %8.4f
    cap value   : %8.4f at strike: %8.4f
    floor value : %8.4f at strike: %8.4f
    collar value: %8.4f
                          """ % (length,vol,
                                 cap.NPV(),capRate*100,
                                 floor.NPV(),floorRate*100),
                                 collar.NPV())
    # check 3
    def testParity(self):
        "Testing put/call parity for cap and floor"
        cases = [ (length,strike,vol)
                   for length in [1, 2, 3, 5, 7, 10, 15, 20]
                   for strike in [0.03, 0.04, 0.05, 0.06, 0.07]
                   for vol in [0.01, 0.05, 0.10, 0.15, 0.20] ]
        startDate = self.termStructure.referenceDate()
        for (length,strike,vol) in cases:
            leg = self.makeLeg(startDate,length)
            cap = self.makeCapFloor(Cap,leg,strike,vol)
            floor = self.makeCapFloor(Floor,leg,strike,vol)
            swap = SimpleSwap(1,startDate,length,"years",self.calendar,
                              self.rollingConvention,self.nominals[0],
                              self.frequency,strike,self.index.isAdjusted(),
                              self.index.dayCounter(),self.frequency,
                              self.index,self.fixingDays,0.0,
                              self.termStructure)
            if abs((cap.NPV()-floor.NPV()) - swap.NPV()) > 1.0e-10:
                self.fail("""
put/call parity violated:
    length     : %d years
    volatility : %8.4f
    strike     : %8.4f
    cap value  : %8.4f
    floor value: %8.4f
    swap value : %8.4f
                          """ % (length,vol,
                                 strike*100,cap.NPV(),
                                 floor.NPV(),swap.NPV()))
    # check 4
    def testCachedValue(self):
        "Testing cap/floor value against cached values"
        cachedToday = Date(14,3,2002)
        cachedSettlement = Date(18,3,2002)
        self.termStructure.linkTo(FlatForward(cachedToday, cachedSettlement,
                                              0.05, DayCounter('Act/360')))
        startDate = self.termStructure.referenceDate()
        leg = self.makeLeg(startDate,20)
        cap = self.makeCapFloor(Cap,leg,0.07,0.20)
        floor = self.makeCapFloor(Floor,leg,0.03,0.20)
        cachedCapNPV = 6.960233718984
        cachedFloorNPV = 2.701296290808
        if abs(cap.NPV()-cachedCapNPV) > 1.0e-11:
            self.fail("""
failed to reproduce cached cap value:
    calculated: %18.12f
    expected:   %18.12f
                      """ % (cap.NPV(),cachedCapNPV))
        if abs(floor.NPV()-cachedFloorNPV) > 1.0e-11:
            self.fail("""
failed to reproduce cached floor value:
    calculated: %18.12f
    expected:   %18.12f
                      """ % (floor.NPV(),cachedFloorNPV))


if __name__ == '__main__':
    import QuantLib
    print 'testing QuantLib', QuantLib.__version__
    suite = unittest.TestSuite()
    suite.addTest(unittest.makeSuite(CapFloorTest,'test'))
    unittest.TextTestRunner(verbosity=2).run(suite)
    raw_input('press return to continue')

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

# $Id$


from QuantLib import *
import unittest

def relErr(x1, x2, reference):
    if reference != 0.0:
        return abs(x1-x2)/reference
    else:
        return 10e10

class OldPricerTest(unittest.TestCase):
    def testBarrierPricer(self):
        "Testing old-style barrier option pricer"
        maxErrorAllowed = 5e-5
        maxStraddleErrorAllowed = 5e-4
        underPrice = 100
        rebate = 3
        resTime = 0.5
        rRate = 0.08
        divRate = 0.04
        # this table is from:
        # "Option pricing formulas", E.G. Haug, McGraw-Hill 1998
        # pag 72
        values = [
            # barrType,  vol,strike, barrier, (   Call,     Put)
            ("DownOut", 0.25,    90,      95, ( 9.0246,  2.2798)),
            ("DownOut", 0.25,   100,      95, ( 6.7924,  2.2947)),
            ("DownOut", 0.25,   110,      95, ( 4.8759,  2.6252)),
            ("DownOut", 0.25,    90,     100, ( 3.0000,  3.0000)),
            ("DownOut", 0.25,   100,     100, ( 3.0000,  3.0000)),
            ("DownOut", 0.25,   110,     100, ( 3.0000,  3.0000)),
            ("UpOut",   0.25,    90,     105, ( 2.6789,  3.7760)),
            ("UpOut",   0.25,   100,     105, ( 2.3580,  5.4932)),
            ("UpOut",   0.25,   110,     105, ( 2.3453,  7.5187)),

            ("DownIn",  0.25,    90,      95, ( 7.7627,  2.9586)),
            ("DownIn",  0.25,   100,      95, ( 4.0109,  6.5677)),
            ("DownIn",  0.25,   110,      95, ( 2.0576, 11.9752)),
            ("DownIn",  0.25,    90,     100, (13.8333,  2.2845)),
            ("DownIn",  0.25,   100,     100, ( 7.8494,  5.9085)),
            ("DownIn",  0.25,   110,     100, ( 3.9795, 11.6465)),
            ("UpIn",    0.25,    90,     105, (14.1112,  1.4653)),
            ("UpIn",    0.25,   100,     105, ( 8.4482,  3.3721)),
            ("UpIn",    0.25,   110,     105, ( 4.5910,  7.0846)),

            ("DownOut", 0.30,    90,      95, ( 8.8334,  2.4170)),
            ("DownOut", 0.30,   100,      95, ( 7.0285,  2.4258)),
            ("DownOut", 0.30,   110,      95, ( 5.4137,  2.6246)),
            ("DownOut", 0.30,    90,     100, ( 3.0000,  3.0000)),
            ("DownOut", 0.30,   100,     100, ( 3.0000,  3.0000)),
            ("DownOut", 0.30,   110,     100, ( 3.0000,  3.0000)),
            ("UpOut",   0.30,    90,     105, ( 2.6341,  4.2293)),
            ("UpOut",   0.30,   100,     105, ( 2.4389,  5.8032)),
            ("UpOut",   0.30,   110,     105, ( 2.4315,  7.5649)),

            ("DownIn",  0.30,    90,      95, ( 9.0093,  3.8769)),
            ("DownIn",  0.30,   100,      95, ( 5.1370,  7.7989)),
            ("DownIn",  0.30,   110,      95, ( 2.8517, 13.3078)),
            ("DownIn",  0.30,    90,     100, (14.8816,  3.3328)),
            ("DownIn",  0.30,   100,     100, ( 9.2045,  7.2636)),
            ("DownIn",  0.30,   110,     100, ( 5.3043, 12.9713)),
            ("UpIn",    0.30,    90,     105, (15.2098,  2.0658)),
            ("UpIn",    0.30,   100,     105, ( 9.7278,  4.4226)),
            ("UpIn",    0.30,   110,     105, ( 5.8350,  8.3686))
        ]

        for barrType, vol, strike, barrier, results in values:
            opCall = BarrierOption(barrType, "Call", underPrice,
                                   strike, divRate, rRate, resTime,
                                   vol, barrier, rebate)
            calculated = opCall.value()
            expected   = results[0]
            error = abs(calculated - expected)
            if not (error <= maxErrorAllowed):
                self.fail("""
%(barrType)7s Call %4(strike)i %4(barrier)i
    value:    %(calculated)8.4f
    expected: %(expected)8.4f
    error:    %(error)12.2e
                """ % locals())

            opPut = BarrierOption(barrType, "Put", underPrice,
                                  strike, divRate, rRate, resTime,
                                  vol, barrier, rebate)
            calculated = opPut.value()
            expected   = results[1]
            error = abs(calculated - expected)
            if not (error <= maxErrorAllowed):
                self.fail("""
%(barrType)7s Put %4(strike)i %4(barrier)i
    value:    %(calculated)8.4f
    expected: %(expected)8.4f
    error:    %(error)12.2e
                """ % locals())

            opStraddle = BarrierOption(barrType, "Straddle",
                                       underPrice, strike, divRate, rRate,
                                       resTime, vol, barrier, rebate)
            calculated = opStraddle.value()
            expected   = results[0] + results[1]
            error = abs(calculated - expected)
            if not (error <= maxStraddleErrorAllowed):
                self.fail("""
%(barrType)7s Straddle %(strike)4i %(barrier)4i
    value:    %(calculated)8.4f
    expected: %(expected)8.4f
    error:    %(error)12.2e
                """ % locals())
    def testBinaryPricer(self):
        "Testing old-style binary option pricer"

        tolerance = {
            'delta' : 5e-5,
            'gamma' : 5e-5,
            'theta' : 5e-5,
            'rho'   : 5e-5,
            'divRho': 5e-5,
            'vega'  : 5e-5
        }

        test_data = [ (type,underlying,rRate,qRate,resTime,strike,vol)
                      for type in ['Call','Put','Straddle']
                      for underlying in [100]
                      for rRate in [0.01, 0.05, 0.15]
                      for qRate in [0.04, 0.05, 0.06]
                      for resTime in [1.0]
                      for strike in [50, 99.5, 100, 100.5, 150]
                      for vol in [0.11, 0.5, 1.2] ]

        for (type,u,r,q,T,k,v) in test_data:
            dS = u/10000.0
            dT = T/10000.0
            dVol = v/10000.0
            dR = r/10000.0
            dQ = q/10000.0
            opt = BinaryOption(type,u,k,q,r,T,v)
            opt_val = opt.value()
            if opt_val > 1e-6:
                optPs = BinaryOption(type, u+dS, k, q   , r,    T ,   v)
                optMs = BinaryOption(type, u-dS, k, q   , r,    T ,   v)
                optPt = BinaryOption(type, u   , k, q   , r,    T+dT, v)
                optMt = BinaryOption(type, u   , k, q   , r,    T-dT, v)
                optPr = BinaryOption(type, u   , k, q   , r+dR, T   , v)
                optMr = BinaryOption(type, u   , k, q   , r-dR, T   , v)
                optPq = BinaryOption(type, u   , k, q+dQ, r   , T   , v)
                optMq = BinaryOption(type, u   , k, q-dQ, r   , T   , v)
                optPv = BinaryOption(type, u   , k, q   , r   , T   , v+dVol)
                optMv = BinaryOption(type, u   , k, q   , r   , T   , v-dVol)

                expected = {
                    'delta' : opt.delta(),
                    'gamma' : opt.gamma(),
                    'theta' : opt.theta(),
                    'rho'   : opt.rho(),
                    'divRho': opt.dividendRho(),
                    'vega'  : opt.vega()
                }
                
                calculated = {
                    'delta' :  (optPs.value()-optMs.value())/(2*dS),
                    'gamma' :  (optPs.delta()-optMs.delta())/(2*dS),
                    'theta' : -(optPt.value()-optMt.value())/(2*dT),
                    'rho'   :  (optPr.value()-optMr.value())/(2*dR),
                    'divRho':  (optPq.value()-optMq.value())/(2*dQ),
                    'vega'  :  (optPv.value()-optMv.value())/(2*dVol)
                }

                for greek in ['delta','gamma','rho','divRho','theta','vega']:
                    expct = expected[greek]
                    calcl = calculated[greek]
                    if not relErr(expct,calcl,u) <= tolerance[greek]:
                        self.fail("""
Option details: %(type)s %(u)f %(k)f %(q)f %(r)f %(T)s %(v)f
    calculated %(greek)s : %(calcl)+9.5f
    expected   %(greek)s : %(expct)+9.5f
                                  """ % locals())
    def testCliquetPricer(self):
        "Testing old-style cliquet option pricer"
        spot = 60
        moneyness = 1.1
        divYield = [0.04, 0.04]
        rRate = [0.08, 0.08]
        dates = [0.25, 1.00]
        vol = [0.30, 0.30]
        cliquet = CliquetOption("Call", spot, moneyness,
            divYield, rRate, dates, vol)

        # Haug, pag 37
        storedValue = 4.4064
        pvalue = cliquet.value()

        if not (abs(pvalue-storedValue) <= 1e-4):
            self.fail("""
calculated value: %(pvalue)g
stored value:     %(storedValue)g
                          """ % locals())
    def testDividendEuropeanPricer(self):
        "Testing old-style European option pricer with dividends"
        nstp = 150
        ngrd = nstp+1

        div   = [3.92,4.21]
        dates = [0.333,0.667]

        tolerance = {
            'delta' : 1e-4,
            'gamma' : 1e-4,
            'theta' : 1e-4,
            'rho'   : 1e-4,
            'vega'  : 1e-4
        }

        test_data = [ (type,underlying,rRate,qRate,resTime,strike,vol)
                      for type in ['Call','Put','Straddle']
                      for underlying in [100]
                      for rRate in [0.01, 0.1, 0.3]
                      for qRate in [0.0, 0.05, 0.15]
                      for resTime in [1.0, 2.0]
                      for strike in [50, 99.5, 100, 100.5, 150]
                      for vol in [0.04, 0.2, 0.7] ]

        factory = FdDividendEuropeanOption
        for (type,u,r,q,T,k,v) in test_data:
            #Check Greeks
            du = u/10000.0
            dT = T/nstp
            dv = v/10000.0
            dr = r/10000.0
            option = factory(type,u,k,q,r,T,v,div,dates)
            if option.value() > 0.00001*u:
                optPs = factory(type,u+du,k,q,r   ,T   ,v,   div,dates)
                optMs = factory(type,u-du,k,q,r   ,T   ,v,   div,dates)
                optPt = factory(type,u   ,k,q,r   ,T+dT,v,   div,
                                [ t+dT for t in dates ])
                optMt = factory(type,u   ,k,q,r   ,T-dT,v,   div,
                                [ t-dT for t in dates ])
                optPr = factory(type,u   ,k,q,r+dr,T   ,v,   div,dates)
                optMr = factory(type,u   ,k,q,r-dr,T   ,v,   div,dates)
                optPv = factory(type,u   ,k,q,r   ,T   ,v+dv,div,dates)
                optMv = factory(type,u   ,k,q,r   ,T   ,v-dv,div,dates)

                expected = {
                    "delta": option.delta(),
                    "gamma": option.gamma(),
                    "theta": option.theta(),
                    "rho"  : option.rho(),
                    "vega" : option.vega()
                }

                calculated = {
                    "delta":  (optPs.value()-optMs.value())/(2*du),
                    "gamma":  (optPs.delta()-optMs.delta())/(2*du),
                    "theta": -(optPt.value()-optMt.value())/(2*dT),
                    "rho"  :  (optPr.value()-optMr.value())/(2*dr),
                    "vega" :  (optPv.value()-optMv.value())/(2*dv)
                }

                for greek in ['delta','gamma','rho','theta','vega']:
                    expct = expected[greek]
                    calcl = calculated[greek]
                    if not relErr(expct,calcl,u) <= tolerance[greek]:
                        self.fail("""
Option details: %(type)s %(u)f %(k)f %(q)f %(r)f %(T)s %(v)f
    calculated %(greek)s : %(calcl)+9.5f
    expected   %(greek)s : %(expct)+9.5f
                                  """ % locals())
    def testFdEuropeanPricer(self):
        "Testing old-style finite-difference European option pricer"
        under = 100
        strikeMin = 60
        strikeRange = 100
        rRateRange = 0.18
        qRateRange = 0.02
        volRange = 1.2
        timeMin = 0.5
        timeRange = 2.0

        tolerance = 1e-2
        totCases = 200

        rng = UniformRandomGenerator(56789012)
        for ite in range(totCases):
            strike = strikeMin + strikeRange * rng.next().value()
            qRate =              qRateRange * rng.next().value()
            rRate =              rRateRange * rng.next().value()
            vol =                volRange * rng.next().value()
            resTime = timeMin +  timeRange * rng.next().value()
            for type in ['Call', 'Put', 'Straddle']:
                anValue = EuropeanOption(type, under, strike, qRate,
                                         rRate, resTime, vol).value()
                numValue = FdEuropean(type, under, strike, qRate,
                                      rRate, resTime, vol, 100, 400).value()
                error = abs(anValue - numValue)
                if not (error <= tolerance):
                    self.fail("""
Option details: %(optType)s %(under)g %(strike)g %(Qrate)g %(Rrate)g %(resTime)g %(vol)g
    Error = %(error)12.2e
	                          """ % locals())
    def testAmericanPricers(self):
        "Testing old-style American-type pricers"
        nstp = 145
        ngrd = nstp + 1

        tolerance = {
            'delta' : 2e-3,
            'gamma' : 2e-3,
            'theta' : 2e-3,
            'rho'   : 2e-3,
            'divRho': 2e-3,
            'vega'  : 2e-3
        }

        test_data = [ (pricer,type,underlying,rRate,qRate,resTime,strike,vol)
                      for pricer in [FdAmericanOption,FdShoutOption]
                      for type in ['Call','Put','Straddle']
                      for underlying in [100]
                      for rRate in [0.01, 0.05, 0.15]
                      for qRate in [0.04, 0.05, 0.06]
                      for resTime in [1.0]
                      for strike in [50, 100, 150]
                      for vol in [0.05, 0.5, 1.2] ]

        for (pricer,type,u,r,q,T,k,v) in test_data:
            # check Greeks
            du = u/10000.0
            dv = v/10000.0
            dr = r/10000.0
            dq = q/10000.0

            option = pricer(type,u,k,q,r,T,v,nstp,ngrd)
            if option.value() > u*1e-5:
                optPs = pricer(type,u+du,k,q,   r,   T,v,   nstp,ngrd)
                optMs = pricer(type,u-du,k,q,   r,   T,v,   nstp,ngrd)
                optPr = pricer(type,u,   k,q,   r+dr,T,v,   nstp,ngrd)
                optMr = pricer(type,u,   k,q,   r-dr,T,v,   nstp,ngrd)
                optPq = pricer(type,u,   k,q+dq,r,   T,v,   nstp,ngrd)
                optMq = pricer(type,u,   k,q-dq,r,   T,v,   nstp,ngrd)
                optPv = pricer(type,u,   k,q,   r,   T,v+dv,nstp,ngrd)
                optMv = pricer(type,u,   k,q,   r,   T,v-dv,nstp,ngrd)

                calculated = {
                    "delta" : option.delta(),
                    "gamma" : option.gamma(),
                    "theta" : option.theta(),
                    "rho"   : option.rho(),
                    "divRho": option.dividendRho(),
                    "vega"  : option.vega()
                }
                
                expected = {
                    "delta" : (optPs.value()-optMs.value())/(2*du),
                    "gamma" : (optPs.delta()-optMs.delta())/(2*du),
                    "theta" : r*option.value() - (r-q)*u*option.delta() \
                              - 0.5*v*v*u*u*option.gamma(),
                    "rho"   : (optPr.value()-optMr.value())/(2*dr),
                    "divRho": (optPq.value()-optMq.value())/(2*dq),
                    "vega"  : (optPv.value()-optMv.value())/(2*dv)
                }

                for greek in ["delta","gamma","theta","rho","divRho","vega"]:
                    expct = expected[greek]
                    calcl = calculated[greek]
                    if not relErr(expct,calcl,u) <= tolerance[greek]:
                        self.fail("""
Option details: %(type)s %(u)f %(k)f %(q)f %(r)f %(T)s %(v)f
    calculated %(greek)s : %(calcl)+9.5f
    expected   %(greek)s : %(expct)+9.5f
                                  """ % locals())
    def testMcSingleFactorPricers(self):
        "Testing old-style Monte Carlo single-factor pricers"
        seed = 3456789
        fixedSamples = 100
        minimumTol = 0.01
        
        # data from "Implementing Derivatives Model",
        # Clewlow, Strickland, pag.118-123
        cases = [
            [DiscreteGeometricAPO, "Call", 100.0, 100.0, 0.03, 0.06,
             [0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0],
             0.2, 5.34255485619]
        ]

        for (pricer, optionType, underlying, strike,
             dividendYield, riskFreeRate, timeIncrements,
             volatility, storedValue) in cases:
            p = pricer(optionType, underlying, strike, dividendYield,
                       riskFreeRate, timeIncrements, volatility)
            pvalue = p.value()

            if not (abs(pvalue-storedValue) <= 1e-10):
                self.fail("""
in batch 1:
calculated value: %(pvalue)g
stored value:     %(storedValue)g
                          """ % locals())
        
        # data from "Option Pricing Formulas", Haug, pag.96-97
        cases = [
            [EuropeanOption,         "Put", 80.0, 85.0,
             -0.03, 0.05, 0.25, 0.2, 5.21858890396],
            [ContinuousGeometricAPO, "Put", 80.0, 85.0,
             -0.03, 0.05, 0.25, 0.2, 4.69221973405]
        ]

        for (pricer, optionType, underlying, strike,
             dividendYield, riskFreeRate, residualTime,
             volatility, storedValue) in cases:
            p = pricer(optionType, underlying, strike, dividendYield,
                       riskFreeRate, residualTime, volatility)
            pvalue = p.value()
            if not (abs(pvalue-storedValue) <= 1e-10):
                self.fail("""
in batch 2:
calculated value: %(pvalue)g
stored value:     %(storedValue)g
                          """ % locals())

        # trying to approximate the continous version with the discrete version
        cases = [
            [DiscreteGeometricAPO, "Put", 80.0, 85.0,
             -0.03, 0.05, 0.25, 90000, 0.2, 4.6922231469]
        ]

        for (pricer, optionType, underlying, strike,
             dividendYield, riskFreeRate, residualTime, timesteps,
             volatility, storedValue) in cases:
            dt=residualTime/timesteps
            timeIncrements=[(i+1)*dt for i in range(timesteps)]
            p = pricer(optionType, underlying, strike, dividendYield,
                       riskFreeRate, timeIncrements, volatility)
            pvalue = p.value()
            if not (abs(pvalue-storedValue) <= 1e-10):
                self.fail("""
in batch 3:
calculated value: %(pvalue)g
stored value:     %(storedValue)g
                          """ % locals())

        cases = [
            [McEuropean,      "Put", 80.0, 85.0,
             -0.03, 0.05, 0.25, 0.2, 5.9135872358, 0],
            [McEuropean,      "Put", 80.0, 85.0,
             -0.03, 0.05, 0.25, 0.2, 5.42005964479, 1],
            [McEuropean,     "Call", 80.0, 85.0,
             -0.03, 0.05, 0.25, 0.2, 1.98816310759, 0],
            [McEuropean,     "Call", 80.0, 85.0,
             -0.03, 0.05, 0.25, 0.2, 2.12098432917, 1],
            [McEuropean, "Straddle", 80.0, 85.0,
             -0.03, 0.05, 0.25, 0.2, 7.90175034339, 0],
            [McEuropean, "Straddle", 80.0, 85.0,
             -0.03, 0.05, 0.25, 0.2, 7.54104397396, 1]
        ]
        for (pricer, optionType, underlying, strike,
             dividendYield, riskFreeRate, residualTime,
             volatility, storedValue, antithetic) in cases:
            p = pricer(optionType, underlying, strike, dividendYield,
                       riskFreeRate, residualTime, volatility,
                       antithetic, seed)
            pvalue = p.valueWithSamples(fixedSamples)
            if not (abs(pvalue-storedValue) <= 1e-10):
                self.fail("""
in batch 4:
calculated value: %(pvalue)g
stored value:     %(storedValue)g
                          """ % locals())
            tol = p.errorEstimate()/pvalue
            tol = min(tol/2.0, minimumTol)
            pvalue = p.value(tol)
            accuracy = p.errorEstimate()/pvalue
            if not (accuracy <= tol):
                self.fail("""
in batch 4:
accuracy reached   : %(accuracy)g
tolerance requested: %(tol)g
                          """ % locals())

        # data from "Asian Option", Levy, 1997
        # in "Exotic Options: The State of the Art",
        # edited by Clewlow, Strickland
        cases = [
            [McDiscreteArithmeticAPO, "Put", 90.0, 87.0,
             0.06, 0.025, 0.0, 11.0/12.0, 2, 0.13, 1.38418414762, 1, 1],
            [McDiscreteArithmeticAPO, "Put", 90.0, 87.0,
             0.06, 0.025, 0.0, 11.0/12.0, 4, 0.13, 1.57691714387, 1, 1],
            [McDiscreteArithmeticAPO, "Put", 90.0, 87.0,
             0.06, 0.025, 0.0, 11.0/12.0, 8, 0.13, 1.66062743445, 1, 1],
            [McDiscreteArithmeticAPO, "Put", 90.0, 87.0,
             0.06, 0.025, 0.0, 11.0/12.0, 12, 0.13, 1.68847081883, 1, 1],
            [McDiscreteArithmeticAPO, "Put", 90.0, 87.0,
             0.06, 0.025, 0.0, 11.0/12.0, 26, 0.13, 1.72955964448, 1, 1],
            [McDiscreteArithmeticAPO, "Put", 90.0, 87.0,
             0.06, 0.025, 0.0, 11.0/12.0, 52, 0.13, 1.73372169316, 1, 1],
            [McDiscreteArithmeticAPO, "Put", 90.0, 87.0,
             0.06, 0.025, 0.0, 11.0/12.0, 100, 0.13, 1.74918801089, 1, 1],
            [McDiscreteArithmeticAPO, "Put", 90.0, 87.0,
             0.06, 0.025, 0.0, 11.0/12.0, 250, 0.13, 1.75421310915, 1, 1],
            [McDiscreteArithmeticAPO, "Put", 90.0, 87.0,
             0.06, 0.025, 0.0, 11.0/12.0, 500, 0.13, 1.75158383443, 1, 1],
            [McDiscreteArithmeticAPO, "Put", 90.0, 87.0,
             0.06, 0.025, 0.0, 11.0/12.0, 1000, 0.13, 1.7516211018 , 1, 1],
            [McDiscreteArithmeticAPO, "Put", 90.0, 87.0,
             0.06, 0.025, 1.0/12.0, 11.0/12.0, 2, 0.13, 1.83665087164, 1, 1],
            [McDiscreteArithmeticAPO, "Put", 90.0, 87.0,
             0.06, 0.025, 1.0/12.0, 11.0/12.0, 4, 0.13, 2.00560271429, 1, 1],
            [McDiscreteArithmeticAPO, "Put", 90.0, 87.0,
             0.06, 0.025, 1.0/12.0, 11.0/12.0, 8, 0.13, 2.07789721712, 1, 1],
            [McDiscreteArithmeticAPO, "Put", 90.0, 87.0,
             0.06, 0.025, 1.0/12.0, 11.0/12.0, 12, 0.13, 2.09622556625, 1, 1],
            [McDiscreteArithmeticAPO, "Put", 90.0, 87.0,
             0.06, 0.025, 1.0/12.0, 11.0/12.0, 26, 0.13, 2.14229795212, 1, 1],
            [McDiscreteArithmeticAPO, "Put", 90.0, 87.0,
             0.06, 0.025, 1.0/12.0, 11.0/12.0, 52, 0.13, 2.14470270916, 1, 1],
            [McDiscreteArithmeticAPO, "Put", 90.0, 87.0,
             0.06, 0.025, 1.0/12.0, 11.0/12.0, 100, 0.13, 2.15954145741, 1, 1],
            [McDiscreteArithmeticAPO, "Put", 90.0, 87.0,
             0.06, 0.025, 1.0/12.0, 11.0/12.0, 250, 0.13, 2.16007690017, 1, 1],
            [McDiscreteArithmeticAPO, "Put", 90.0, 87.0,
             0.06, 0.025, 1.0/12.0, 11.0/12.0, 500, 0.13, 2.159867044  , 1, 1],
            [McDiscreteArithmeticAPO, "Put", 90.0, 87.0,
             0.06, 0.025, 1.0/12.0, 11.0/12.0,1000, 0.13, 2.15951634387, 1, 1],
            [McDiscreteArithmeticAPO, "Put", 90.0, 87.0,
             0.06, 0.025, 3.0/12.0, 11.0/12.0, 2, 0.13, 2.63315092584, 1, 1],
            [McDiscreteArithmeticAPO, "Put", 90.0, 87.0,
             0.06, 0.025, 3.0/12.0, 11.0/12.0, 4, 0.13, 2.76723962361, 1, 1],
            [McDiscreteArithmeticAPO, "Put", 90.0, 87.0,
             0.06, 0.025, 3.0/12.0, 11.0/12.0, 8, 0.13, 2.83124836881, 1, 1],
            [McDiscreteArithmeticAPO, "Put", 90.0, 87.0,
             0.06, 0.025, 3.0/12.0, 11.0/12.0, 12, 0.13, 2.84290301412, 1, 1],
            [McDiscreteArithmeticAPO, "Put", 90.0, 87.0,
             0.06, 0.025, 3.0/12.0, 11.0/12.0, 26, 0.13, 2.88179560417, 1, 1],
            [McDiscreteArithmeticAPO, "Put", 90.0, 87.0,
             0.06, 0.025, 3.0/12.0, 11.0/12.0, 52, 0.13, 2.88447044543, 1, 1],
            [McDiscreteArithmeticAPO, "Put", 90.0, 87.0,
             0.06, 0.025, 3.0/12.0, 11.0/12.0, 100, 0.13, 2.89985329603, 1, 1],
            [McDiscreteArithmeticAPO, "Put", 90.0, 87.0,
             0.06, 0.025, 3.0/12.0, 11.0/12.0, 250, 0.13, 2.90047296063, 1, 1],
            [McDiscreteArithmeticAPO, "Put", 90.0, 87.0,
             0.06, 0.025, 3.0/12.0, 11.0/12.0, 500, 0.13, 2.8981341216 , 1, 1],
            [McDiscreteArithmeticAPO, "Put", 90.0, 87.0,
             0.06, 0.025, 3.0/12.0, 11.0/12.0,1000, 0.13, 2.89703362437, 1, 1]
           ]
        for (pricer, optionType, underlying, strike,
             dividendYield, riskFreeRate, first, length, fixings,
             volatility, storedValue, antithetic, controlVariate) in cases:
            dt=(length)/(fixings-1)
            timeIncrements = [i*dt+first for i in range(fixings)]
            p = pricer(optionType, underlying, strike, dividendYield,
                       riskFreeRate, timeIncrements, volatility,
                       antithetic, controlVariate, seed)
            pvalue = p.valueWithSamples(fixedSamples)
            if not (abs(pvalue-storedValue) <= 1e-10):
                self.fail("""
in batch 5:
calculated value: %(pvalue)g
stored value:     %(storedValue)g
                          """ % locals())
            tol = p.errorEstimate()/pvalue
            tol = min(tol/2.0, minimumTol)
            pvalue = p.value(tol)
            accuracy = p.errorEstimate()/pvalue
            if not (accuracy <= tol):
                self.fail("""
in batch 5:
accuracy reached   : %(accuracy)g
tolerance requested: %(tol)g
                          """ % locals())

        # data from "Asian Option", Levy, 1997
        # in "Exotic Options: The State of the Art",
        # edited by Clewlow, Strickland
        cases = [
            [McDiscreteArithmeticASO, "Call", 90.0, 87.0,
             0.06, 0.025, 0.0, 11.0/12.0,    2, 0.13, 1.51917595129, 1, 1],
            [McDiscreteArithmeticASO, "Call", 90.0, 87.0,
             0.06, 0.025, 0.0, 11.0/12.0,    4, 0.13, 1.67940165674, 1, 1],
            [McDiscreteArithmeticASO, "Call", 90.0, 87.0,
             0.06, 0.025, 0.0, 11.0/12.0,    8, 0.13, 1.75371215251, 1, 1],
            [McDiscreteArithmeticASO, "Call", 90.0, 87.0,
             0.06, 0.025, 0.0, 11.0/12.0,   12, 0.13, 1.77595318693, 1, 1],
            [McDiscreteArithmeticASO, "Call", 90.0, 87.0,
             0.06, 0.025, 0.0, 11.0/12.0,   26, 0.13, 1.8143053663 , 1, 1],
            [McDiscreteArithmeticASO, "Call", 90.0, 87.0,
             0.06, 0.025, 0.0, 11.0/12.0,   52, 0.13, 1.82269246898, 1, 1],
            [McDiscreteArithmeticASO, "Call", 90.0, 87.0,
             0.06, 0.025, 0.0, 11.0/12.0,  100, 0.13, 1.83822402464, 1, 1],
            [McDiscreteArithmeticASO, "Call", 90.0, 87.0,
             0.06, 0.025, 0.0, 11.0/12.0,  250, 0.13, 1.83875059026, 1, 1],
            [McDiscreteArithmeticASO, "Call", 90.0, 87.0,
             0.06, 0.025, 0.0, 11.0/12.0,  500, 0.13, 1.83750703638, 1, 1],
            [McDiscreteArithmeticASO, "Call", 90.0, 87.0,
             0.06, 0.025, 0.0, 11.0/12.0, 1000, 0.13, 1.83887181884, 1, 1],
            [McDiscreteArithmeticASO, "Call", 90.0, 87.0,
             0.06, 0.025, 1.0/12.0, 11.0/12.0, 2, 0.13, 1.51154400089, 1, 1],
            [McDiscreteArithmeticASO, "Call", 90.0, 87.0,
             0.06, 0.025, 1.0/12.0, 11.0/12.0, 4, 0.13, 1.67103508506, 1, 1],
            [McDiscreteArithmeticASO, "Call", 90.0, 87.0,
             0.06, 0.025, 1.0/12.0, 11.0/12.0, 8, 0.13, 1.7452968407 , 1, 1],
            [McDiscreteArithmeticASO, "Call", 90.0, 87.0,
             0.06, 0.025, 1.0/12.0, 11.0/12.0, 12, 0.13, 1.76667074564, 1, 1],
            [McDiscreteArithmeticASO, "Call", 90.0, 87.0,
             0.06, 0.025, 1.0/12.0, 11.0/12.0, 26, 0.13, 1.80528400613, 1, 1],
            [McDiscreteArithmeticASO, "Call", 90.0, 87.0,
             0.06, 0.025, 1.0/12.0, 11.0/12.0, 52, 0.13, 1.81400883891, 1, 1],
            [McDiscreteArithmeticASO, "Call", 90.0, 87.0,
             0.06, 0.025, 1.0/12.0, 11.0/12.0, 100, 0.13, 1.82922901451, 1, 1],
            [McDiscreteArithmeticASO, "Call", 90.0, 87.0,
             0.06, 0.025, 1.0/12.0, 11.0/12.0, 250, 0.13, 1.82937111773, 1, 1],
            [McDiscreteArithmeticASO, "Call", 90.0, 87.0,
             0.06, 0.025, 1.0/12.0, 11.0/12.0, 500, 0.13, 1.82826193186, 1, 1],
            [McDiscreteArithmeticASO, "Call", 90.0, 87.0,
             0.06, 0.025, 1.0/12.0, 11.0/12.0,1000, 0.13, 1.82967846654, 1, 1],
            [McDiscreteArithmeticASO, "Call", 90.0, 87.0,
             0.06, 0.025, 3.0/12.0, 11.0/12.0, 2, 0.13, 1.49648170891, 1, 1],
            [McDiscreteArithmeticASO, "Call", 90.0, 87.0,
             0.06, 0.025, 3.0/12.0, 11.0/12.0, 4, 0.13, 1.65443100462, 1, 1],
            [McDiscreteArithmeticASO, "Call", 90.0, 87.0,
             0.06, 0.025, 3.0/12.0, 11.0/12.0, 8, 0.13, 1.72817806731, 1, 1],
            [McDiscreteArithmeticASO, "Call", 90.0, 87.0,
             0.06, 0.025, 3.0/12.0, 11.0/12.0, 12, 0.13, 1.74877367895, 1, 1],
            [McDiscreteArithmeticASO, "Call", 90.0, 87.0,
             0.06, 0.025, 3.0/12.0, 11.0/12.0, 26, 0.13, 1.78733801988, 1, 1],
            [McDiscreteArithmeticASO, "Call", 90.0, 87.0,
             0.06, 0.025, 3.0/12.0, 11.0/12.0, 52, 0.13, 1.79624826757, 1, 1],
            [McDiscreteArithmeticASO, "Call", 90.0, 87.0,
             0.06, 0.025, 3.0/12.0, 11.0/12.0, 100, 0.13, 1.81114186876, 1, 1],
            [McDiscreteArithmeticASO, "Call", 90.0, 87.0,
             0.06, 0.025, 3.0/12.0, 11.0/12.0, 250, 0.13, 1.81101152587, 1, 1],
            [McDiscreteArithmeticASO, "Call", 90.0, 87.0,
             0.06, 0.025, 3.0/12.0, 11.0/12.0, 500, 0.13, 1.81002311939, 1, 1],
            [McDiscreteArithmeticASO, "Call", 90.0, 87.0,
             0.06, 0.025, 3.0/12.0, 11.0/12.0, 1000, 0.13, 1.81145760308, 1, 1]
           ]
        for (pricer, optionType, underlying, strike,
             dividendYield, riskFreeRate, first, length, fixings,
             volatility, storedValue, antithetic, controlVariate) in cases:
            dt=(length)/(fixings-1)
            timeIncrements=[i*dt+first for i in range(fixings)]
            p = pricer(optionType, underlying, dividendYield,
                       riskFreeRate, timeIncrements, volatility,
                       antithetic, controlVariate, seed)
            pvalue = p.valueWithSamples(fixedSamples)
            if not (abs(pvalue-storedValue) <= 1e-10):
                self.fail("""
in batch 6:
calculated value: %(pvalue)g
stored value:     %(storedValue)g
                          """ % locals())
            tol = p.errorEstimate()/pvalue
            tol = min(tol/2.0, minimumTol)
            pvalue = p.value(tol)
            accuracy = p.errorEstimate()/pvalue
            if not (accuracy <= tol):
                self.fail("""
in batch 6:
accuracy reached   : %(accuracy)g
tolerance requested: %(tol)g
                          """ % locals())

    def testMcMultiFactorPricers(self):
        "Testing old-style Monte Carlo multi-factor pricers"
        cor = [[1.00, 0.50, 0.30, 0.10],
               [0.50, 1.00, 0.20, 0.40],
               [0.30, 0.20, 1.00, 0.60],
               [0.10, 0.40, 0.60, 1.00]]
        volatilities = [ 0.30,  0.35,  0.25,  0.20]
        covariance = QuantLib.covariance(volatilities, cor)
        dividendYields = [0.01, 0.05, 0.04, 0.03]
        riskFreeRate = 0.05
        resTime = 1.0
        # degenerate portfolio
        perfectCorrelation = [[1.00, 1.00, 1.00, 1.00],
                              [1.00, 1.00, 1.00, 1.00],
                              [1.00, 1.00, 1.00, 1.00],
                              [1.00, 1.00, 1.00, 1.00]]
        sameAssetVols = [ 0.30,  0.30,  0.30,  0.30]
        sameAssetCovariance = QuantLib.covariance(sameAssetVols,
                                                  perfectCorrelation)
        sameAssetDividend = [ 0.030,  0.030,  0.030,  0.030]
        seed = 86421
        fixedSamples = 100
        minimumTol = 0.01

        # McEverest
        p = McEverest(dividendYields, covariance,
                      riskFreeRate, resTime, 0, seed)
        storedValue = 0.7434481
        pvalue = p.valueWithSamples(fixedSamples)
        if not (abs(pvalue-storedValue) <= 1e-5):
            self.fail("""
McEverest:
calculated value: %(pvalue)g
stored value:     %(storedValue)g
                      """ % locals())
        tol = p.errorEstimate()/pvalue
        tol = min(tol/2.0, minimumTol)
        pvalue = p.value(tol)
        accuracy = p.errorEstimate()/pvalue
        if not (accuracy <= tol):
            self.fail("""
McEverest:
accuracy reached   : %(accuracy)g
tolerance requested: %(tol)g
                      """ % locals())
        p = McEverest(dividendYields, covariance,
                      riskFreeRate, resTime, 1, seed)
        storedValue = 0.7569787
        pvalue = p.valueWithSamples(fixedSamples)
        if not (abs(pvalue-storedValue) <= 1e-5):
            self.fail("""
McEverest:
calculated value: %(pvalue)g
stored value:     %(storedValue)g
                      """ % locals())
        tol = p.errorEstimate()/pvalue
        tol = min(tol/2.0, minimumTol)
        pvalue = p.value(tol)
        accuracy = p.errorEstimate()/pvalue
        if not (accuracy <= tol):
            self.fail("""
McEverest:
accuracy reached   : %(accuracy)g
tolerance requested: %(tol)g
                      """ % locals())

        # McBasket
        sameAssetValues = [ 25,  25,   25,  25]
        type = 'Call'
        strike = 100
        p = McBasket(type, sameAssetValues, strike, sameAssetDividend,
                     sameAssetCovariance, riskFreeRate, resTime, 0, seed)
        # european would be 12.4426495605
        storedValue = 10.4484452
        pvalue = p.valueWithSamples(fixedSamples)
        if not (abs(pvalue-storedValue) <= 1e-3):
            self.fail("""
McBasket:
calculated value: %(pvalue)g
stored value:     %(storedValue)g
                      """ % locals())
        tol = p.errorEstimate()/pvalue
        tol = min(tol/2.0, minimumTol)
        pvalue = p.value(tol)
        accuracy = p.errorEstimate()/pvalue
        if not (accuracy <= tol):
            self.fail("""
McBasket:
accuracy reached   : %(accuracy)g
tolerance requested: %(tol)g
                      """ % locals())
        p = McBasket(type, sameAssetValues, strike, sameAssetDividend,
                     sameAssetCovariance, riskFreeRate, resTime, 1, seed)
        # european would be 12.4426495605
        storedValue = 12.2946771
        pvalue = p.valueWithSamples(fixedSamples)
        if not (abs(pvalue-storedValue) <= 1e-3):
            self.fail("""
McBasket:
calculated value: %(pvalue)g
stored value:     %(storedValue)g
                      """ % locals())
        tol = p.errorEstimate()/pvalue
        tol = min(tol/2.0, minimumTol)
        pvalue = p.value(tol)
        accuracy = p.errorEstimate()/pvalue
        if not (accuracy <= tol):
            self.fail("""
McBasket:
accuracy reached   : %(accuracy)g
tolerance requested: %(tol)g
                      """ % locals())

        # McMaxBasket
        assetValues = [ 100,  110,   90,  105]
        p = McMaxBasket(assetValues, dividendYields,
                        covariance, riskFreeRate, resTime, 0, seed)
        storedValue = 120.7337797
        pvalue = p.valueWithSamples(fixedSamples)
        if not (abs(pvalue-storedValue) <= 1e-5):
            self.fail("""
McMaxBasket:
calculated value: %(pvalue)g
stored value:     %(storedValue)g
                      """ % locals())
        tol = p.errorEstimate()/pvalue
        tol = min(tol/2.0, minimumTol)
        pvalue = p.value(tol)
        accuracy = p.errorEstimate()/pvalue
        if not (accuracy <= tol):
            self.fail("""
McMaxBasket:
accuracy reached   : %(accuracy)g
tolerance requested: %(tol)g
                      """ % locals())
        p = McMaxBasket(assetValues, dividendYields,
                        covariance, riskFreeRate, resTime, 1, seed)
        storedValue = 123.5209095
        pvalue = p.valueWithSamples(fixedSamples)
        if not (abs(pvalue-storedValue) <= 1e-5):
            self.fail("""
McMaxBasket:
calculated value: %(pvalue)g
stored value:     %(storedValue)g
                      """ % locals())
        tol = p.errorEstimate()/pvalue
        tol = min(tol/2.0, minimumTol)
        pvalue = p.value(tol)
        accuracy = p.errorEstimate()/pvalue
        if not (accuracy <= tol):
            self.fail("""
McMaxBasket:
accuracy reached   : %(accuracy)g
tolerance requested: %(tol)g
                      """ % locals())

        # McPagoda
        portfolio   = [0.15, 0.20, 0.35, 0.30]
        fraction = 0.62
        roof = 0.20
        timeIncrements = [0.25, 0.5, 0.75, 1]
        p = McPagoda(portfolio, fraction, roof,
                     dividendYields, covariance, riskFreeRate,
                     timeIncrements, 0, seed);
        storedValue =  0.03438975
        pvalue = p.valueWithSamples(fixedSamples)
        if not (abs(pvalue-storedValue) <= 1e-5):
            self.fail("""
McPagoda:
calculated value: %(pvalue)g
stored value:     %(storedValue)g
                      """ % locals())
        tol = p.errorEstimate()/pvalue
        tol = min(tol/2.0, minimumTol)
        pvalue = p.value(tol)
        accuracy = p.errorEstimate()/pvalue
        if not (accuracy <= tol):
            self.fail("""
McPagoda:
accuracy reached   : %(accuracy)g
tolerance requested: %(tol)g
                      """ % locals())
        p = McPagoda(portfolio, fraction, roof,
                     dividendYields, covariance, riskFreeRate,
                     timeIncrements, 1, seed);
        storedValue = 0.03860954
        pvalue = p.valueWithSamples(fixedSamples)
        if not (abs(pvalue-storedValue) <= 1e-5):
            self.fail("""
McPagoda:
calculated value: %(pvalue)g
stored value:     %(storedValue)g
                      """ % locals())
        tol = p.errorEstimate()/pvalue
        tol = min(tol/2.0, minimumTol)
        pvalue = p.value(tol)
        accuracy = p.errorEstimate()/pvalue
        if not (accuracy <= tol):
            self.fail("""
McPagoda:
accuracy reached   : %(accuracy)g
tolerance requested: %(tol)g
                      """ % locals())

        # McHimalaya
        strike = 101
        p = McHimalaya(assetValues, dividendYields, covariance,
                       riskFreeRate, strike, timeIncrements, 0, seed)
        storedValue = 5.0768499
        pvalue = p.valueWithSamples(fixedSamples)
        if not (abs(pvalue-storedValue) <= 1e-5):
            self.fail("""
McHimalaya:
calculated value: %(pvalue)g
stored value:     %(storedValue)g
                      """ % locals())
        tol = p.errorEstimate()/pvalue
        tol = min(tol/2.0, minimumTol)
        pvalue = p.value(tol)
        accuracy = p.errorEstimate()/pvalue
        if not (accuracy <= tol):
            self.fail("""
McHimalaya:
accuracy reached   : %(accuracy)g
tolerance requested: %(tol)g
                      """ % locals())
        p = McHimalaya(assetValues, dividendYields, covariance,
                       riskFreeRate, strike, timeIncrements, 1, seed)
        storedValue = 6.2478050
        pvalue = p.valueWithSamples(fixedSamples)
        if not (abs(pvalue-storedValue) <= 1e-5):
            self.fail("""
McHimalaya:
calculated value: %(pvalue)g
stored value:     %(storedValue)g
                      """ % locals())
        tol = p.errorEstimate()/pvalue
        tol = min(tol/2.0, minimumTol)
        pvalue = p.value(tol)
        accuracy = p.errorEstimate()/pvalue
        if not (accuracy <= tol):
            self.fail("""
McHimalaya:
accuracy reached   : %(accuracy)g
tolerance requested: %(tol)g
                      """ % locals())



if __name__ == '__main__':
    import QuantLib
    print 'testing QuantLib', QuantLib.__version__
    suite = unittest.TestSuite()
    suite.addTest(unittest.makeSuite(OldPricerTest,'test'))
    unittest.TextTestRunner(verbosity=2).run(suite)
    raw_input('press return to continue')


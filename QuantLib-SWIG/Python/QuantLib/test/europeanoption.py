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

def relErr(x1, x2, reference):
    if reference != 0.0:
        return abs(x1-x2)/reference
    else:
        return 10e+10

def EuropeanOption(type,underlying,strike,divCurve,rfCurve,
                   exDate,volCurve,engineType='analytic'):
    if engineType == 'analytic':
        engine = AnalyticEuropeanEngine()
    elif engineType == 'jr' or engineType == 'crr' or \
         engineType == 'eqp' or engineType == 'trigeorgis' \
         or engineType == 'tian':
        engine = BinomialEuropeanEngine(engineType,800)
    return VanillaOption(type,MarketElementHandle(underlying),strike,
                         divCurve,rfCurve,EuropeanExercise(exDate),
                         volCurve,engine)

def flatCurve(forward):
    today = Date_todaysDate()
    settlement = Calendar("TARGET").advance(today,2,'days')
    return TermStructureHandle(
        FlatForward(today, settlement, MarketElementHandle(forward),
                    DayCounter('act/365')))

def flatVolatility(volatility):
    today = Date_todaysDate()
    settlement = Calendar("TARGET").advance(today,2,'days')
    return BlackVolTermStructureHandle(
        BlackConstantVol(settlement, MarketElementHandle(volatility),
                         DayCounter('act/365')))

class EuropeanOptionTest(unittest.TestCase):
    def testGreeks(self):
        "Testing European option greeks"

        tolerance = {
            'delta' : 1.0e-4,
            'gamma' : 1.0e-4,
            'theta' : 1.0e-2,
            'rho'   : 1.0e-4,
            'divRho': 1.0e-4,
            'vega'  : 1.0e-4
        }

        test_options = [(type,strike,exDate)
                        for type in ['Call','Put','Straddle']
                        for strike in [50, 99.5, 100, 100.5, 150]
                        for exDate in [Calendar("TARGET").roll(
                                       Date_todaysDate().plusYears(2))]]
        
        test_data = [(under,qRate,rRate,vol)
                     for under in [100]
                     for qRate in [0.04, 0.05, 0.06]
                     for rRate in [0.01, 0.05, 0.15]
                     for vol in [0.11, 0.5, 1.2]]

        underlying = SimpleMarketElement(0.0)
        volatility = SimpleMarketElement(0.0)
        volCurve = flatVolatility(volatility)
        qRate = SimpleMarketElement(0.0)
        divCurve = flatCurve(qRate)
        rRate = SimpleMarketElement(0.0)
        rfCurve = flatCurve(rRate)
        
        for (type,strike,exDate) in test_options:
            opt = EuropeanOption(type,underlying,strike,
                                 divCurve,rfCurve,exDate,volCurve)

            # time-shifted exercise dates
            exDateP = Calendar("TARGET").advance(exDate,1,'day')
            exDateM = Calendar("TARGET").advance(exDate,-1,'day')
            dT = exDateP.serialNumber()-exDateM.serialNumber()
            opt_p = EuropeanOption(type, underlying , strike,
                                   divCurve,  rfCurve,
                                   exDateP ,  volCurve)
            opt_m = EuropeanOption(type, underlying , strike,
                                   divCurve,  rfCurve,
                                   exDateM ,  volCurve)
            
            for (u,q,r,v) in test_data:
            
                underlying.setValue(u)
                volatility.setValue(v)
                qRate.setValue(q)
                rRate.setValue(r)
                
                value  = opt.NPV()
                expected = {
                    'delta' : opt.delta(),
                    'gamma' : opt.gamma(),
                    'theta' : opt.theta(),
                    'rho'   : opt.rho(),
                    'divRho': opt.dividendRho(),
                    'vega'  : opt.vega()
                }

                if value>0.00001*u:
                    calculated = {}
                    
                    # perturb underlying and get delta and gamma
                    du = u/10000.0
                    underlying.setValue(u+du)
                    value_p = opt.NPV()
                    delta_p = opt.delta()
                    underlying.setValue(u-du)
                    value_m = opt.NPV()
                    delta_m = opt.delta()
                    underlying.setValue(u)
                    calculated['delta'] = (value_p-value_m)/(2*du)
                    calculated['gamma'] = (delta_p-delta_m)/(2*du)

                    # perturb rates and get rho and dividend rho
                    dr = r/10000.0
                    rRate.setValue(r+dr)
                    value_p = opt.NPV()
                    rRate.setValue(r-dr)
                    value_m = opt.NPV()
                    rRate.setValue(r)
                    calculated['rho'] = (value_p-value_m)/(2*dr)
                
                    dq = q/10000.0
                    qRate.setValue(q+dq)
                    value_p = opt.NPV()
                    qRate.setValue(q-dq)
                    value_m = opt.NPV()
                    qRate.setValue(q)
                    calculated['divRho'] = (value_p-value_m)/(2*dq)

                    # perturb volatility and get vega
                    dv = v/10000.0
                    volatility.setValue(v+dv)
                    value_p = opt.NPV()
                    volatility.setValue(v-dv)
                    value_m = opt.NPV()
                    volatility.setValue(v)
                    calculated['vega'] = (value_p-value_m)/(2*dv)

                    # get theta from time-shifted options
                    calculated['theta'] =-(opt_p.NPV()-opt_m.NPV())/(dT/365.0)

                    for greek in ['delta','gamma','rho',
                                  'divRho','theta','vega']:
                        expct = expected[greek]
                        calcl = calculated[greek]
                        if not relErr(expct,calcl,u) <= tolerance[greek]:
                            self.fail("""
Option details: %(type)s %(u)f %(strike)f %(q)f %(r)f %(exDate)s %(v)f
    calculated %(greek)s : %(calcl)+9.5f
    expected   %(greek)s : %(expct)+9.5f
                                  """ % locals())

    def testImpliedVol(self):
        "Testing European option implied volatility"

        maxEvaluations = 100
        tolerance = 1.0e-6
        
        test_options = [(type,strike,exDate)
                        for type in ['Call','Put','Straddle']
                        for strike in [50, 99.5, 100, 100.5, 150]
                        for exDate in [Date_todaysDate().plusDays(d)
                                       for d in [36,180,360,1080]]]
        
        test_data = [(under,qRate,rRate,vol)
                     for under in [80, 95, 99.9, 100, 100.1, 105, 120]
                     for qRate in [0.01, 0.05, 0.10]
                     for rRate in [0.01, 0.05, 0.10]
                     for vol in [0.01, 0.2, 0.3, 0.7, 0.9]]

        underlying = SimpleMarketElement(0.0)
        volatility = SimpleMarketElement(0.0)
        volCurve = flatVolatility(volatility)
        qRate = SimpleMarketElement(0.0)
        divCurve = flatCurve(qRate)
        rRate = SimpleMarketElement(0.0)
        rfCurve = flatCurve(rRate)
        
        for (type,strike,exDate) in test_options:
            opt = EuropeanOption(type,underlying,strike,
                                 divCurve,rfCurve,exDate,volCurve)

            for (u,q,r,v) in test_data:
            
                underlying.setValue(u)
                volatility.setValue(v)
                qRate.setValue(q)
                rRate.setValue(r)
                
                value  = opt.NPV()
                if value != 0.0:
                    # shift guess somehow
                    volatility.setValue(v*1.5)
                    try:
                        implVol = opt.impliedVolatility(value,
                                                        tolerance,
                                                        maxEvaluations)
                    except Exception, e:
                        raise """
%(e)s
Option details: %(type)s %(u)f %(strike)f %(q)f %(r)f %(exDate)s
    while trying to calculate implied vol from value %(value)f
                              """ % locals()
                    
                        
                    if abs(implVol-v) > tolerance:
                        # the difference might not matter
                        volatility.setValue(implVol)
                        value2 = opt.NPV()
                        if abs(value2-value)/u > 1.0e-6:
                            raise """
Option details: %(type)s %(u)f %(strike)f %(q)f %(r)f %(exDate)s
    original volatility: %(v)12.10f
    price:               %(value)f
    implied volatility:  %(implVol)12.10f
    corresponding price: %(value2)f
                                  """ % locals()

    def testBinomialEngines(self):
        "Testing binomial European engines against analytic results"

        tolerance = 0.1

        test_options = [(type,strike,exDate)
                        for type in ['Call','Put','Straddle']
                        for strike in [50, 100, 150]
                        for exDate in [Calendar("TARGET").roll(
                                       Date_todaysDate().plusYears(1))]]
                      
        test_data = [(under,qRate,rRate,vol)
                     for under in [100]
                     for qRate in [0.0, 0.05]
                     for rRate in [0.01, 0.05, 0.15]
                     for vol in [0.11, 0.5, 1.2]]

        engines = ['jr', 'crr', 'eqp', 'trigeorgis', 'tian']

        underlying = SimpleMarketElement(0.0)
        volatility = SimpleMarketElement(0.0)
        volCurve = flatVolatility(volatility)
        qRate = SimpleMarketElement(0.0)
        divCurve = flatCurve(qRate)
        rRate = SimpleMarketElement(0.0)
        rfCurve = flatCurve(rRate)
        
        for (type,strike,exDate) in test_options:
            refOption = EuropeanOption(type,underlying,strike,
                                       divCurve,rfCurve,exDate,volCurve)
            options = {}
            for e in engines:
                options[e] = EuropeanOption(type,underlying,strike,
                                            divCurve,rfCurve,exDate,
                                            volCurve,e)

            for (u,q,r,v) in test_data:
            
                underlying.setValue(u)
                volatility.setValue(v)
                qRate.setValue(q)
                rRate.setValue(r)
        
                refValue = refOption.NPV()
                for e in engines:
                    value = options[e].NPV()
                    if not relErr(refValue,value,u) <= tolerance:
                        self.fail("""
Option details: %(type)s %(u)f %(strike)f %(q)f %(r)f %(exDate)s %(v)f
    analytic:      %(value)9.5f
    binomial (%(e)s): %(value_jr)9.5f
                              """ % locals())


if __name__ == '__main__':
    import QuantLib
    print 'testing QuantLib', QuantLib.__version__
    suite = unittest.TestSuite()
    suite.addTest(unittest.makeSuite(EuropeanOptionTest,'test'))
    unittest.TextTestRunner(verbosity=2).run(suite)
    raw_input('press return to continue')


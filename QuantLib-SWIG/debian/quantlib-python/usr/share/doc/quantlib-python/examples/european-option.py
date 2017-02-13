
# Copyright (C) 2004, 2005, 2006, 2007 StatPro Italia srl
#
# This file is part of QuantLib, a free-software/open-source library
# for financial quantitative analysts and developers - http://quantlib.org/
#
# QuantLib is free software: you can redistribute it and/or modify it under the
# terms of the QuantLib license.  You should have received a copy of the
# license along with this program; if not, please email
# <quantlib-dev@lists.sf.net>. The license is also available online at
# <http://quantlib.org/license.shtml>.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the license for more details.

from QuantLib import *

# global data
todaysDate = Date(15,May,1998)
Settings.instance().evaluationDate = todaysDate
settlementDate = Date(17,May,1998)
riskFreeRate = FlatForward(settlementDate, 0.05, Actual365Fixed())

# option parameters
exercise = EuropeanExercise(Date(17,May,1999))
payoff = PlainVanillaPayoff(Option.Call, 8.0)

# market data
underlying = SimpleQuote(7.0)
volatility = BlackConstantVol(todaysDate, TARGET(), 0.10, Actual365Fixed())
dividendYield = FlatForward(settlementDate, 0.05, Actual365Fixed())

# report
header = ' |'.join(['%17s' % tag for tag in ['method','value',
                                            'estimated error',
                                            'actual error' ] ])
print('')
print(header)
print('-'*len(header))

refValue = None
def report(method, x, dx = None):
    e = '%.4f' % abs(x-refValue)
    x = '%.5f' % x
    if dx:
        dx = '%.4f' % dx
    else:
        dx = 'n/a'
    print(' |'.join(['%17s' % y for y in [method, x, dx, e] ]))


# good to go

process = BlackScholesMertonProcess(QuoteHandle(underlying),
                                    YieldTermStructureHandle(dividendYield),
                                    YieldTermStructureHandle(riskFreeRate),
                                    BlackVolTermStructureHandle(volatility))

option = VanillaOption(payoff, exercise)

# method: analytic
option.setPricingEngine(AnalyticEuropeanEngine(process))
value = option.NPV()
refValue = value
report('analytic',value)

# method: integral
option.setPricingEngine(IntegralEngine(process))
report('integral',option.NPV())

# method: finite differences
timeSteps = 801
gridPoints = 800

option.setPricingEngine(FDEuropeanEngine(process,timeSteps,gridPoints))
report('finite diff.',option.NPV())

# method: binomial
timeSteps = 801

option.setPricingEngine(BinomialVanillaEngine(process,'jr',timeSteps))
report('binomial (JR)',option.NPV())

option.setPricingEngine(BinomialVanillaEngine(process,'crr',timeSteps))
report('binomial (CRR)',option.NPV())

option.setPricingEngine(BinomialVanillaEngine(process,'eqp',timeSteps))
report('binomial (EQP)',option.NPV())

option.setPricingEngine(BinomialVanillaEngine(process,'trigeorgis',timeSteps))
report('bin. (Trigeorgis)',option.NPV())

option.setPricingEngine(BinomialVanillaEngine(process,'tian',timeSteps))
report('binomial (Tian)',option.NPV())

option.setPricingEngine(BinomialVanillaEngine(process,'lr',timeSteps))
report('binomial (LR)',option.NPV())

# method: finite differences
# not yet implemented

# method: Monte Carlo
option.setPricingEngine(MCEuropeanEngine(process,
                                         'pseudorandom',
                                         timeSteps = 1,
                                         requiredTolerance = 0.02,
                                         seed = 42))
report('MC (crude)', option.NPV(), option.errorEstimate())

option.setPricingEngine(MCEuropeanEngine(process,
                                         'lowdiscrepancy',
                                         timeSteps = 1,
                                         requiredSamples = 32768))
report('MC (Sobol)', option.NPV())


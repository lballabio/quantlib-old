
# Copyright (C) 2004 StatPro Italia srl
#
# This file is part of QuantLib, a free-software/open-source library
# for financial quantitative analysts and developers - http://quantlib.org/
#
# QuantLib is free software: you can redistribute it and/or modify it under the
# terms of the QuantLib license.  You should have received a copy of the
# license along with this program; if not, please email
# quantlib-dev@lists.sf.net
#
# The license is also available online at http://quantlib.org/html/license.html
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the license for more details.

from QuantLib import *

# global data
todaysDate = Date(15,5,1998)
settlementDate = Date(17,5,1998)
riskFreeRate = FlatForward(todaysDate,settlementDate,
                           0.05, DayCounter('act/365'))

# option parameters
exercise = EuropeanExercise(Date(17,5,1999))
payoff = PlainVanillaPayoff('call', 8.0)

# market data
underlying = SimpleQuote(7.0)
volatility = BlackConstantVol(todaysDate, 0.10)
dividendYield = FlatForward(todaysDate,settlementDate,
                            0.05, DayCounter('act/365'))

# report
header = ' |'.join(['%17s' % tag for tag in ['method','value',
                                            'estimated error',
                                            'actual error' ] ])
print
print header
print '-'*len(header)

refValue = None
def report(method, x, dx = None):
    e = '%.4f' % abs(x-refValue)
    x = '%.5f' % x
    if dx:
        dx = '%.4f' % dx
    else:
        dx = 'n/a'
    print ' |'.join(['%17s' % y for y in [method, x, dx, e] ])


# good to go

process = BlackScholesProcess(QuoteHandle(underlying),
                              TermStructureHandle(dividendYield),
                              TermStructureHandle(riskFreeRate),
                              BlackVolTermStructureHandle(volatility))

option = VanillaOption(process, payoff, exercise)

# method: analytic
option.setPricingEngine(AnalyticEuropeanEngine())
value = option.NPV()
refValue = value
report('analytic',value)

# method: integral
option.setPricingEngine(IntegralEngine())
report('integral',option.NPV())

# method: binomial
timeSteps = 801

option.setPricingEngine(BinomialEuropeanEngine('jr',timeSteps))
report('binomial (JR)',option.NPV())

option.setPricingEngine(BinomialEuropeanEngine('crr',timeSteps))
report('binomial (CRR)',option.NPV())

option.setPricingEngine(BinomialEuropeanEngine('eqp',timeSteps))
report('binomial (EQP)',option.NPV())

option.setPricingEngine(BinomialEuropeanEngine('trigeorgis',timeSteps))
report('bin. (Trigeorgis)',option.NPV())

option.setPricingEngine(BinomialEuropeanEngine('tian',timeSteps))
report('binomial (Tian)',option.NPV())

option.setPricingEngine(BinomialEuropeanEngine('lr',timeSteps))
report('binomial (LR)',option.NPV())

# method: finite differences
# not yet implemented

# method: Monte Carlo
option.setPricingEngine(MCEuropeanEngine('pseudorandom',
                                         timeSteps = 1,
                                         requiredTolerance = 0.02,
                                         seed = 42))
report('MC (crude)', option.NPV(), option.errorEstimate())

option.setPricingEngine(MCEuropeanEngine('lowdiscrepancy',
                                         timeSteps = 1,
                                         requiredSamples = 32768))
report('MC (Sobol)', option.NPV())


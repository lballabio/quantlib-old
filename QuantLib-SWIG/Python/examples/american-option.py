
# Copyright (C) 2004 StatPro Italia srl
#
# This file is part of QuantLib, a free-software/open-source library
# for financial quantitative analysts and developers - http://quantlib.org/
#
# QuantLib is free software: you can redistribute it and/or modify it under the
# terms of the QuantLib license.  You should have received a copy of the
# license along with this program; if not, please email quantlib-dev@lists.sf.net
# The license is also available online at http://quantlib.org/html/license.html
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the license for more details.

from QuantLib import *

# global data
todaysDate = Date(15,5,1998)
Settings.instance().evaluationDate = todaysDate
settlementDate = Date(17,5,1998)
riskFreeRate = FlatForward(settlementDate, 0.06, Actual365())

# option parameters
exercise = AmericanExercise(settlementDate, Date(17,5,1999))
payoff = PlainVanillaPayoff('put', 40.0)

# market data
underlying = SimpleQuote(36.0)
volatility = BlackConstantVol(todaysDate, 0.20)
dividendYield = FlatForward(settlementDate, 0.00, Actual365())

# report
header = '%19s' % 'method' + ' |' + \
         ' |'.join(['%17s' % tag for tag in ['value',
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
    print '%19s' % method + ' |' + \
          ' |'.join(['%17s' % y for y in [x, dx, e] ])

# good to go

process = BlackScholesProcess(QuoteHandle(underlying),
                              YieldTermStructureHandle(dividendYield),
                              YieldTermStructureHandle(riskFreeRate),
                              BlackVolTermStructureHandle(volatility))

option = VanillaOption(process, payoff, exercise)

refValue = 4.48667344

# method: analytic

option.setPricingEngine(BaroneAdesiWhaleyEngine())
report('Barone-Adesi-Whaley',option.NPV())

option.setPricingEngine(BjerksundStenslandEngine())
report('Bjerksund-Stensland',option.NPV())

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

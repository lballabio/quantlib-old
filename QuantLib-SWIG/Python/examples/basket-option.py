
# Copyright (C) 2004, 2005, 2006 StatPro Italia srl
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
underlying1 = SimpleQuote(7.0)
volatility1 = BlackConstantVol(todaysDate, TARGET(), 0.10, Actual365Fixed())
dividendYield1 = FlatForward(settlementDate, 0.05, Actual365Fixed())
underlying2 = SimpleQuote(7.0)
volatility2 = BlackConstantVol(todaysDate, TARGET(), 0.10, Actual365Fixed())
dividendYield2 = FlatForward(settlementDate, 0.05, Actual365Fixed())


process1 = BlackScholesMertonProcess(QuoteHandle(underlying1),
                                    YieldTermStructureHandle(dividendYield1),
                                    YieldTermStructureHandle(riskFreeRate),
                                    BlackVolTermStructureHandle(volatility1))

process2 = BlackScholesMertonProcess(QuoteHandle(underlying2),
                                    YieldTermStructureHandle(dividendYield2),
                                    YieldTermStructureHandle(riskFreeRate),
                                    BlackVolTermStructureHandle(volatility2))

procs = StochasticProcessVector()
procs.push_back(process1)
procs.push_back(process2)

matrix = Matrix(2,2)
matrix[0][0] = 1.0
matrix[1][1] = 1.0
matrix[0][1] = 0.5
matrix[1][0] = 0.5

process = StochasticProcessArray(procs, matrix)
basketoption = BasketOption(MaxBasketPayoff(payoff), exercise)
basketoption.setPricingEngine(MCEuropeanBasketEngine(process,
                                                     'pseudorandom',
                                                     timeStepsPerYear = 1,
                                                     requiredTolerance = 0.02,
                                                     seed = 42))
print basketoption.NPV()

basketoption = BasketOption(MinBasketPayoff(payoff), exercise)
basketoption.setPricingEngine(MCEuropeanBasketEngine(process,
                                                     'pseudorandom',
                                                     timeStepsPerYear = 1,
                                                     requiredTolerance = 0.02,
                                                     seed = 42))
print basketoption.NPV()

basketoption = BasketOption(AverageBasketPayoff(payoff, 2), exercise)
basketoption.setPricingEngine(MCEuropeanBasketEngine(process,
                                                     'pseudorandom',
                                                     timeStepsPerYear = 1,
                                                     requiredTolerance = 0.02,
                                                     seed = 42))
print basketoption.NPV()


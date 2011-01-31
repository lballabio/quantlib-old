
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

require 'QuantLib'
include QuantLib

# global data
todaysDate = Date.new(15,May,1998)
Settings.instance.evaluationDate = todaysDate
settlementDate = Date.new(17,May,1998)
riskFreeRate = FlatForward.new(settlementDate, 0.05, Actual365Fixed.new)

# option parameters
exercise = EuropeanExercise.new(Date.new(17,5,1999))
payoff = PlainVanillaPayoff.new(Option::Call, 8.0)

# market data
underlying = SimpleQuote.new(7.0)
volatility = BlackConstantVol.new(todaysDate, TARGET.new,
                                  0.10, Actual365Fixed.new)
dividendYield = FlatForward.new(settlementDate, 0.05, Actual365Fixed.new)

# report
Format = '%17s |%17s |%17s |%17s'
header = sprintf(Format, 'method', 'value', 'estimated error', 'actual error')
rule = '-'*header.length

puts
puts header
puts rule

def report(method, x, dx = nil)
    e = sprintf('%.4f', (x-RefValue).abs)
    x = sprintf('%.5f', x)
    if dx
        dx = sprintf('%.4f', dx)
    else
        dx = 'n/a'
    end
    puts sprintf(Format, method, x, dx, e)
end

# good to go

process = BlackScholesMertonProcess.new(
                                  QuoteHandle.new(underlying),
                                  YieldTermStructureHandle.new(dividendYield),
                                  YieldTermStructureHandle.new(riskFreeRate),
                                  BlackVolTermStructureHandle.new(volatility))

option = VanillaOption.new(payoff, exercise)

# method: analytic
option.pricingEngine = AnalyticEuropeanEngine.new(process)
value = option.NPV
RefValue = value
report('analytic',value)

# method: integral
option.pricingEngine = IntegralEngine.new(process)
report('integral',option.NPV)

# method: finite differences
timeSteps = 801
gridPoints = 800

option.pricingEngine = FDEuropeanEngine.new(process,timeSteps,gridPoints)
report('finite diff.',option.NPV)

# method: binomial
timeSteps = 801

option.pricingEngine = BinomialVanillaEngine.new(process,'jr',timeSteps)
report('binomial (JR)',option.NPV)

option.pricingEngine = BinomialVanillaEngine.new(process,'crr',timeSteps)
report('binomial (CRR)',option.NPV)

option.pricingEngine = BinomialVanillaEngine.new(process,'eqp',timeSteps)
report('binomial (EQP)',option.NPV)

option.pricingEngine = BinomialVanillaEngine.new(process,'trigeorgis',timeSteps)
report('bin. (Trigeorgis)',option.NPV)

option.pricingEngine = BinomialVanillaEngine.new(process,'tian',timeSteps)
report('binomial (Tian)',option.NPV)

option.pricingEngine = BinomialVanillaEngine.new(process,'lr',timeSteps)
report('binomial (LR)',option.NPV)

# method: finite differences
# not yet implemented

# method: Monte Carlo
option.pricingEngine = MCEuropeanEngine.new(process,'pseudorandom',
                                            1, nil, false,
                                            false, nil, 0.02, nil, 42)
report('MC (crude)', option.NPV, option.errorEstimate)

option.pricingEngine = MCEuropeanEngine.new(process,'lowdiscrepancy', 
                                            1, nil, false, false, 32768)
report('MC (Sobol)', option.NPV)


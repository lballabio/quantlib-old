
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

require 'QuantLib'
include QuantLib

# global data
todaysDate = Date.new(15,5,1998)
Settings.instance.evaluationDate = todaysDate
settlementDate = Date.new(17,5,1998)
riskFreeRate = FlatForward.new(settlementDate, 0.06, Actual365Fixed.new)

# option parameters
exercise = AmericanExercise.new(settlementDate, Date.new(17,5,1999))
payoff = PlainVanillaPayoff.new('put', 40.0)

# market data
underlying = SimpleQuote.new(36.0)
volatility = BlackConstantVol.new(todaysDate, 0.20, Actual365Fixed.new)
dividendYield = FlatForward.new(settlementDate, 0.00, Actual365Fixed.new)

# report
Format = '%19s |%17s |%17s |%17s'
header = sprintf(Format, 'method', 'value', 'estimated error', 'actual error')
rule = '-'*header.length

puts
puts header
puts rule

RefValue = 4.48667344

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

process = BlackScholesProcess.new(QuoteHandle.new(underlying),
                                  YieldTermStructureHandle.new(dividendYield),
                                  YieldTermStructureHandle.new(riskFreeRate),
                                  BlackVolTermStructureHandle.new(volatility))

option = VanillaOption.new(process, payoff, exercise)

report('reference value',RefValue)

# method: analytic

option.pricingEngine = BaroneAdesiWhaleyEngine.new
report('Barone-Adesi-Whaley',option.NPV)

option.pricingEngine = BjerksundStenslandEngine.new
report('Bjerksund-Stensland',option.NPV)

# method: finite differences
timeSteps = 801
gridPoints = 800

option.pricingEngine = FDAmericanEngine.new(timeSteps,gridPoints)
report('finite differences',option.NPV)

# method: binomial
timeSteps = 801

option.pricingEngine = BinomialEuropeanEngine.new('jr',timeSteps)
report('binomial (JR)',option.NPV)

option.pricingEngine = BinomialEuropeanEngine.new('crr',timeSteps)
report('binomial (CRR)',option.NPV)

option.pricingEngine = BinomialEuropeanEngine.new('eqp',timeSteps)
report('binomial (EQP)',option.NPV)

option.pricingEngine = BinomialEuropeanEngine.new('trigeorgis',timeSteps)
report('bin. (Trigeorgis)',option.NPV)

option.pricingEngine = BinomialEuropeanEngine.new('tian',timeSteps)
report('binomial (Tian)',option.NPV)

option.pricingEngine = BinomialEuropeanEngine.new('lr',timeSteps)
report('binomial (LR)',option.NPV)


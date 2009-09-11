# Example of option baskets
# Distributed under BSD license

from QuantLib import *
todaysDate = Date(15, May, 1998)
Settings.instance().evaluationDate = todaysDate
settlementDate = Date(17, May, 1998)
riskFreeQuote = SimpleQuote(0.05)
riskFreeRate = FlatForward(settlementDate,
                           QuoteHandle(riskFreeQuote), Actual365Fixed())

# option parameters
exercise1 = AmericanExercise(settlementDate, Date(17,May,1999))
exercise2 = EuropeanExercise(settlementDate)
payoff = PlainVanillaPayoff(Option.Call, 40.0)

# market data
underlying = SimpleQuote(36.0)
volatilityQuote = SimpleQuote(0.05)
volatility = BlackConstantVol(todaysDate,
                              QuoteHandle(volatilityQuote),
                              Actual365Fixed())
dividendYield = FlatForward(settlementDate, 0.00, Actual365Fixed())

# good to go
process = BlackScholesMertonProcess(QuoteHandle(underlying),
                                    YieldTermStructureHandle(dividendYield),
                                    YieldTermStructureHandle(riskFreeRate),
                                    BlackVolTermStructureHandle(volatility))
option1 = VanillaOption(process, payoff, exercise1)
option1.setPricingEngine(BaroneAdesiWhaleyEngine())


def f(x,y):
    underlying.setValue(x)
    volatilityQuote.setValue(y)
    return option1.NPV()

def setQuote(r):
    riskFreeQuote.setValue(r)

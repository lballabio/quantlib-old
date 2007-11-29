todaysDate <- Date(15, "May", 1998)
Settings_instance()$setEvaluationDate(d=todaysDate)
settlementDate <- Date(17, "May", 1998)
riskFreeRate <- FlatForward(settlementDate, 0.05, Actual365Fixed())
exercise <- EuropeanExercise(Date(17, "May", 1999))
payoff <- PlainVanillaPayoff("Call", 8.0)
underlying <- SimpleQuote(7.0)
volatility <- BlackConstantVol(todaysDate, TARGET(), 0.10, Actual365Fixed())
dividendYield <- FlatForward(settlementDate, 0.05, Actual365Fixed())
process <- BlackScholesMertonProcess(QuoteHandle(underlying),
		YieldTermStructureHandle(dividendYield),
		YieldTermStructureHandle(riskFreeRate),
		BlackVolTermStructureHandle(volatility))
option <- VanillaOption(payoff, exercise)
option$setPricingEngine(s_arg2=AnalyticEuropeanEngine(process))
value <- option$NPV()
value

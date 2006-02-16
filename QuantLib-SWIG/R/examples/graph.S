quote <- function(x) {
todaysDate <- Date(15, "May", 1998)
Settings_instance()$setEvaluationDate(d=todaysDate)
settlementDate <- Date(17, "May", 1998)
riskFreeRate <- FlatForward(settlementDate, 0.05, Actual365Fixed())
exercise <- EuropeanExercise(Date(17, "May", 1999))
payoff <- PlainVanillaPayoff("Call", 8.0)
underlying <- SimpleQuote(x)
volatility <- BlackConstantVol(todaysDate, 0.10, Actual365Fixed())
dividendYield <- FlatForward(settlementDate, 0.05, Actual365Fixed())
process <- BlackScholesProcess(QuoteHandle(underlying),
		YieldTermStructureHandle(dividendYield),
		YieldTermStructureHandle(riskFreeRate),
		BlackVolTermStructureHandle(volatility))
option <- VanillaOption(process, payoff, exercise)
Instrument_setPricingEngine(option, s_arg2=AnalyticEuropeanEngine())
value <- option$NPV()
value
}

plot(seq(1.0,10.0,by=0.1), lapply(seq(1.0,10.0,by=0.1), quote))
require('lattice')
spot<-seq(10.0,95.00,len=20)
vol<-seq(0.20,1.0,len=20)
g<-expand.grid(spot=spot,vol=vol)

todaysDate <- Date(15, "May", 1998)
Settings_instance()$setEvaluationDate(d=todaysDate)
settlementDate <- Date(17, "May", 1998)
riskFreeRate <- FlatForward(settlementDate, 0.05, Actual365Fixed())
exercise <- EuropeanExercise(Date(17, "May", 1999))
payoff <- PlainVanillaPayoff("Call", 8.0)
dividendYield <- FlatForward(settlementDate, 0.05, Actual365Fixed())
underlying <- SimpleQuote(10.0)
engine <- AnalyticEuropeanEngine()
		
g["NPV"] <- mapply(function(x,y) {
underlying$setValue(value=x)
volatility <- BlackConstantVol(todaysDate, y, Actual365Fixed())
process <- BlackScholesProcess(QuoteHandle(underlying),
		YieldTermStructureHandle(dividendYield),
		YieldTermStructureHandle(riskFreeRate),
		BlackVolTermStructureHandle(volatility))
option <- VanillaOption(process, payoff, exercise)
option$setPricingEngine(s_arg2=engine)
option$NPV()}, g$spot, g$vol)
		
print(wireframe(NPV ~ spot*vol,g,drape=TRUE))



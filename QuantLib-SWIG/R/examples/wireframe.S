require('lattice')
spot<-seq(10.0,95.00,len=20)
vol<-seq(0.20,1.0,len=20)
g<-expand.grid(spot=spot,vol=vol)

todaysDate <- Date(15, "May", 1998)
Settings_instance()$setEvaluationDate(d=todaysDate)
settlementDate <- Date(17, "May", 1998)
riskFreeRate <- FlatForward(settlementDate, 0.05, Actual365Fixed())
exercise <- EuropeanExercise(Date(17, "May", 1999))
payoff <- PlainVanillaPayoff("Call", 50.0)
dividendYield <- FlatForward(settlementDate, 0.05, Actual365Fixed())
underlying <- SimpleQuote(10.0)
engine <- AnalyticEuropeanEngine()
		
t <- mapply(function(x,y) {
underlying$setValue(value=x)
volatility <- BlackConstantVol(todaysDate, y, Actual365Fixed())
process <- BlackScholesProcess(QuoteHandle(underlying),
		YieldTermStructureHandle(dividendYield),
		YieldTermStructureHandle(riskFreeRate),
		BlackVolTermStructureHandle(volatility))
option <- VanillaOption(process, payoff, exercise)
option$setPricingEngine(s_arg2=engine)
list(NPV=option$NPV(), gamma=option$gamma(),
	delta=option$delta())}, g$spot, g$vol, SIMPLIFY=FALSE)

g$NPV <- sapply(1:length(t), function(x) t[[x]]$NPV)
g$gamma <- sapply(1:length(t), function(x) t[[x]]$gamma)	
g$delta <- sapply(1:length(t), function(x) t[[x]]$delta)
par(mfrow = c(2,3))
plot(wireframe(NPV ~ spot*vol, g, drape=TRUE))
plot(wireframe(gamma ~ spot*vol, g, drape=TRUE))




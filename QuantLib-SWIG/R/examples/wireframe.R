require('lattice')
require('grid')
spot<-seq(10.0,95.00,len=20)
vol<-seq(0.20,1.0,len=20)
g<-expand.grid(spot=spot,vol=vol)

todaysDate <- Date(15, "May", 1998)
Settings_instance()$setEvaluationDate(d=todaysDate)
settlementDate <- Date(17, "May", 1998)
riskQuote <- SimpleQuote(0.05)
riskFreeRate <- FlatForward(settlementDate, QuoteHandle(riskQuote), 
	     Actual365Fixed())
exercise <- EuropeanExercise(Date(17, "May", 1999))
payoff <- PlainVanillaPayoff("Call", 50.0)
dividendYield <- FlatForward(settlementDate, 0.05, Actual365Fixed())
underlying <- SimpleQuote(10.0)
engine <- AnalyticEuropeanEngine()
volatilityQuote <- SimpleQuote(0.05)
volatility <- BlackConstantVol(todaysDate,
	   QuoteHandle(volatilityQuote), 
	      Actual365Fixed())
process <- BlackScholesProcess(QuoteHandle(underlying),
		YieldTermStructureHandle(dividendYield),
		YieldTermStructureHandle(riskFreeRate),
		BlackVolTermStructureHandle(volatility))
option <- VanillaOption(process, payoff, exercise)
option$setPricingEngine(s_arg2=engine)
		
t <- mapply(function(x,y) {
underlying$setValue(value=x)
volatilityQuote$setValue(value=y)
list(NPV=option$NPV(), gamma=option$gamma(),
	delta=option$delta(), vega=option$vega())}, 
	g$spot, g$vol, SIMPLIFY=FALSE)

g$NPV <- sapply(1:length(t), function(x) t[[x]]$NPV)
g$gamma <- sapply(1:length(t), function(x) t[[x]]$gamma)	
g$delta <- sapply(1:length(t), function(x) t[[x]]$delta)
g$vega <- sapply(1:length(t), function(x) t[[x]]$vega)
par(mfrow = c(2,3))
plot <- vector("list",10)
plot[[1]] <- wireframe(NPV ~ spot*vol, g, drape=TRUE)
plot[[2]] <- wireframe(gamma ~ spot*vol, g, drape=TRUE)
plot[[3]] <- wireframe(delta ~ spot*vol, g, drape=TRUE)
plot[[4]] <- wireframe(vega ~ spot*vol, g, drape=TRUE)

grid.newpage()
pushViewport(viewport(layout=grid.layout(2,2)))
for (i in 1:4) {
pushViewport(viewport(layout.pos.col=((i - 1) %% 2) + 1,
            layout.pos.row=((i - 1) %/% 2) + 1))
  print(plot[[i]], newpage=FALSE)
	  popViewport()
}
popViewport()


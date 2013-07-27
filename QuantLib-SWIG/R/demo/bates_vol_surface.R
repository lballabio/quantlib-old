
library(QuantLib)
library(lattice)

todaysDate <- Date(18, "May", 2013)
Settings_instance()$setEvaluationDate(d=todaysDate)
settlementDate <- Date(18, "May", 2013)

riskFreeRate <- YieldTermStructureHandle(FlatForward(settlementDate, 0.01, Actual365Fixed()))

dividendYield <-  YieldTermStructureHandle(FlatForward(settlementDate, 0.04, Actual365Fixed()))

underlying <- QuoteHandle(SimpleQuote(100))

bsProcess <- BlackScholesMertonProcess(underlying, dividendYield, riskFreeRate,
                                       BlackVolTermStructureHandle(
                                           BlackConstantVol(todaysDate, TARGET(),
                                                            QuoteHandle(SimpleQuote(0.25)),
                                                            Actual365Fixed())))

strikes <- seq(20, 200, length=30)
maturities <- seq(0.25, 2, length=30)
g <- expand.grid(strikes=strikes, maturities=maturities)

batesEngine <- BatesEngine(BatesModel(BatesProcess(
    riskFreeRate, dividendYield, underlying,
    0.1, 1.5, 0.25, 0.75, -0.75, 0.75, -0.05, 0.3)), 128)

impliedVol <- function(strike, maturity) {
    exercise <- EuropeanExercise(Date(18, "May", 2013) +
                                 Period(maturity*365, "Days"))
    payoff <- PlainVanillaPayoff("Call", strike)
    option <- VanillaOption(payoff, exercise)
    option$setPricingEngine(s_arg2=batesEngine)
    VanillaOption_impliedVolatility(option,
                                    targetValue=option$NPV(),
                                    process=bsProcess,
                                    accuracy=1e-16,
                                    maxEvaluations=100,
                                    minVol=0.1,
                                    maxVol=5.1)
}

g$vol <- mapply(impliedVol, g$strikes, g$maturities)

newcols <- colorRampPalette(c("grey90", "grey10"))
print(wireframe(vol ~ strikes*maturities, g,
                xlab="Strike",ylab="Maturitiy",zlab="Vol",
                drape=TRUE,col.regions=rainbow(100,end=0.99,alpha=0.9),
                screen = list(z = -25, x = -65),scale=list(arrows=FALSE)))

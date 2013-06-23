
## expanded to follow european-option.py

## if the package QuantLib exists, load it; else carry on
try(suppressMessages(library(QuantLib)))

# global data
todaysDate <- Date(15, "May", 1998)
invisible(Settings_instance()$setEvaluationDate(d=todaysDate))
settlementDate <- Date(17, "May", 1998)
riskFreeRate <- FlatForward(settlementDate, 0.05, Actual365Fixed())

# option parameters
exercise <- EuropeanExercise(Date(17, "May", 1999))
payoff <- PlainVanillaPayoff("Call", 8.0)

# market data
underlying <- SimpleQuote(7.0)
volatility <- BlackConstantVol(todaysDate, TARGET(), 0.10, Actual365Fixed())
dividendYield <- FlatForward(settlementDate, 0.05, Actual365Fixed())

process <- BlackScholesMertonProcess(QuoteHandle(underlying),
                                     YieldTermStructureHandle(dividendYield),
                                     YieldTermStructureHandle(riskFreeRate),
                                     BlackVolTermStructureHandle(volatility))

cat(sprintf("%17s  %8s  %6s  %6s\n", "method", "value", "errest", "error"))
cat(rep("=", 43), "\n", sep="")
report <- function(method, x, dx=NA) {
    err <- abs(x - refValue)            # refValue is a global
    cat(sprintf("%17s  %8.5f  %6.4f  %6.4f\n", method, x, dx, err))
    invisible(NULL)
}

option <- VanillaOption(payoff, exercise)
invisible(option$setPricingEngine(option, AnalyticEuropeanEngine(process)))
value <- option$NPV()
refValue <- value
report("analytic", value)

invisible(option$setPricingEngine(option, IntegralEngine(process)))
report('integral', option$NPV())


## method: finite differences
timeSteps <- 801
gridPoints <- 800

invisible(option$setPricingEngine(option, FDEuropeanEngine(process,timeSteps,gridPoints)))
report('finite diff.', option$NPV())


## method: binomial
timeSteps <- 801

invisible(option$setPricingEngine(option, BinomialVanillaEngine(process,'jr',timeSteps)))
report('binomial (JR)', option$NPV())

invisible(option$setPricingEngine(option, BinomialVanillaEngine(process,'crr',timeSteps)))
report('binomial (CRR)', option$NPV())

invisible(option$setPricingEngine(option, BinomialVanillaEngine(process,'eqp',timeSteps)))
report('binomial (EQP)', option$NPV())

invisible(option$setPricingEngine(option, BinomialVanillaEngine(process,'trigeorgis',timeSteps)))
report('bin. (Trigeorgis)', option$NPV())

invisible(option$setPricingEngine(option, BinomialVanillaEngine(process,'tian',timeSteps)))
report('binomial (Tian)', option$NPV())

invisible(option$setPricingEngine(option, BinomialVanillaEngine(process,'lr',timeSteps)))
report('binomial (LR)', option$NPV())


MCEuropeanEngine <- function(process, traits, timeSteps, timeStepsPerYear=NA,
                             brownianBridge=FALSE, antitheticVariate=FALSE,
                             requiredSamples=NULL,
                             requiredTolerance=1.0e-3, maxSamples=NULL, seed=0L) {
    traits <- as(traits, "character")
    timeSteps <- as.integer(timeSteps)                  ##as(input, "integer");
    timeStepsPerYear <- as.integer(timeStepsPerYear)    ##as(input, "integer");
    brownianBridge <- as.logical(brownianBridge)
    antitheticVariate <- as.logical(antitheticVariate)
    requiredSamples <- as.integer(requiredSamples)      ##as(input, "integer");
    requiredTolerance <- as.numeric(requiredTolerance)  ##as(input, "numeric");
    maxSamples <- as.integer(maxSamples)                ##as(input, "integer");
    seed <- as.integer(seed)
    if (length(seed) > 1) {
        warning("using only the first element of seed")
    }

    ans <- .Call('R_swig_new_MCEuropeanEngine',
                 process, traits, timeSteps,
                 timeStepsPerYear, brownianBridge,
                 antitheticVariate, requiredSamples,
                 requiredTolerance, maxSamples, seed,
                 PACKAGE='QuantLib')
    class(ans) <- "_p_MCEuropeanEnginePtr"

    reg.finalizer(ans, delete_MCEuropeanEngine)
    ans
}


## method: Monte Carlo
invisible(option$setPricingEngine(option, MCEuropeanEngine(process,
                                  'pseudorandom',
                                  timeSteps = 1,
                                  requiredTolerance = 0.02,
                                  seed = 42)))
report('MC (crude)', option$NPV(), option$errorEstimate())

invisible(option$setPricingEngine(option, MCEuropeanEngine(process,
                                  'lowdiscrepancy',
                                  timeSteps = 1,
                                  requiredSamples = 32768)))
report('MC (Sobol)', option$NPV())


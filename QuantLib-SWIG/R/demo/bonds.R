## bonds.R -- following what bonds.py does for the QuantLib bindings for Python

suppressMessages(library(QuantLib))

## global data
calendar <- TARGET()

settlementDate <- Date(18, "September", 2008)
settlementDate <- Calendar_adjust(calendar, settlementDate)

fixingDays <- 3
settlementDays <- 3
todaysDate <- Calendar_advance(calendar, settlementDate, -fixingDays, "Days")
invisible(Settings_instance()$setEvaluationDate(d=todaysDate))

cat('Today          : ', todaysDate$`__str__`(), "\n")
cat('Settlement Date: ', settlementDate$`__str__`(), "\n")

## market quotes
## constructing bond yield curve
zcQuotes <- list(rates=c(0.0096, 0.0145, 0.0194),
                 tenor=c(Period(3, "Months"),
                 Period(6, "Months"),
                 Period(1, "Years")))

zcBondsDayCounter <- Actual365Fixed()

bondInstruments <- RateHelperVector()

for (i in 1:3) {
    r <- zcQuotes[["rates"]][i]
    tenor <- zcQuotes[["tenor"]][[i]]
    drh <- DepositRateHelper(QuoteHandle(SimpleQuote(r)),
                             tenor,
                             fixingDays,
                             calendar,
                             "ModifiedFollowing",
                             TRUE,
                             zcBondsDayCounter)
    RateHelperVector_push_back(bondInstruments, drh)
}

## setup bonds
redemption <- 100.0
numberOfBonds <- 5

bondQuotes <- list(list(Date(15,"March",2005),
                        Date(31,"August",2010),
                        0.02375, 100.390625),
                   list(Date(15,"June",2005),
                        Date(31,"August",2011),
                        0.04625, 106.21875),
                   list(Date(30,"June",2006),
                        Date(31,"August",2013),
                        0.03125, 100.59375),
                   list(Date(15,"November",2002),
                        Date(15,"August",2018),
                        0.04000, 101.6875),
                   list(Date(15,"May",1987),
                        Date (15,"May",2038),
                        0.04500, 102.140625)
                   )

# Definition of the rate helpers
for (i in 1:5) {
    issueDate <- bondQuotes[[i]][[1]]
    maturity <- bondQuotes[[i]][[2]]
    couponRate <- bondQuotes[[i]][[3]]
    marketQuote <- bondQuotes[[i]][[4]]
    schedule <- Schedule(issueDate, maturity, Period("Semiannual"),
                         UnitedStates("GovernmentBond"),
                         "Unadjusted", "Unadjusted",
                         copyToR(DateGeneration(), "Backward"),
                         FALSE)

    bh <- FixedRateBondHelper(QuoteHandle(SimpleQuote(marketQuote)),
                              settlementDays,
                              100.0,
                              schedule,
                              couponRate,
                              ActualActual("Bond"),
                              "Unadjusted",
                              redemption,
                              issueDate)
    RateHelperVector_push_back(bondInstruments, bh)
}
termStructureDayCounter <-  ActualActual("ISDA")

# not needed as defined in the interface file:  tolerance = 1.0e-15

bondDiscountingTermStructure <- PiecewiseFlatForward(settlementDate,
                                                     bondInstruments,
                                                     termStructureDayCounter)


# Building of the Libor forecasting curve
# deposits
dQuotes <- list(list(0.043375,  Period(1,"Weeks")),
                list(0.031875,  Period(1,"Months")),
                list(0.0320375, Period(3,"Months")),
                list(0.03385,   Period(6,"Months")),
                list(0.0338125, Period(9,"Months")),
                list(0.0335125, Period(1,"Years")))
sQuotes <- list(list(0.0295, Period(2,"Years")),
                list(0.0323, Period(3,"Years")),
                list(0.0359, Period(5,"Years")),
                list(0.0412, Period(10,"Years")),
                list(0.0433, Period(15,"Years")))

## deposits
depositDayCounter <- Actual360()
depoSwapInstruments <- RateHelperVector()

for (i in 1:length(dQuotes)) {
    rate  <- dQuotes[[i]][[1]]
    tenor <- dQuotes[[i]][[2]]
    drh <- DepositRateHelper(QuoteHandle(SimpleQuote(rate)),
                             tenor, fixingDays,
                             calendar, "ModifiedFollowing",
                             TRUE, depositDayCounter)
    RateHelperVector_push_back(depoSwapInstruments, drh)
}


## swaps
swFixedLegFrequency <- "Annual"
swFixedLegConvention <- "Unadjusted"
swFixedLegDayCounter <- Thirty360("European")
swFloatingLegIndex <- Euribor6M()
forwardStart <- Period(1,"Days")
for (i in 1:length(sQuotes)) {
    rate  <- sQuotes[[i]][[1]]
    tenor <- sQuotes[[i]][[2]]
    srh <- SwapRateHelper(QuoteHandle(SimpleQuote(rate)), tenor,
                          calendar, swFixedLegFrequency,
                          swFixedLegConvention, swFixedLegDayCounter,
                          swFloatingLegIndex, QuoteHandle(),forwardStart)
    RateHelperVector_push_back(depoSwapInstruments, srh)
}

depoSwapTermStructure <- PiecewiseFlatForward(settlementDate, depoSwapInstruments,
                                              termStructureDayCounter)

## Term structures that will be used for pricing:
## the one used for discounting cash flows

discountingTermStructure <- RelinkableYieldTermStructureHandle()

## the one used for forward rate forecasting
forecastingTermStructure <- RelinkableYieldTermStructureHandle()


########################################
##        BONDS TO BE PRICED           #
########################################

## common data

faceAmount <- 100

## pricing engine
bondEngine <- DiscountingBondEngine(discountingTermStructure)

## zero coupon bond
zeroCouponBond <- ZeroCouponBond(settlementDays,
                                 UnitedStates("GovernmentBond"),
                                 faceAmount,
                                 Date(15,"August",2013),
                                 "Following",
                                 116.92,
                                 Date(15,"August",2003))

invisible(Instrument_setPricingEngine(zeroCouponBond, bondEngine))

## fixed 4.5% US Treasury note

fixedBondSchedule <- Schedule(Date(15, "May", 2007),
                              Date(15, "May",2017), Period("Semiannual"),
                              UnitedStates("GovernmentBond"),
                              "Unadjusted", "Unadjusted",
                              copyToR(DateGeneration(), "Backward"), FALSE)

fixedRateBond <- FixedRateBond(settlementDays,
                               faceAmount,
                               fixedBondSchedule,
                               0.045,
                               ActualActual("Bond"),
                               "ModifiedFollowing",
                               100.0, Date(15, "May", 2007))
invisible(Instrument_setPricingEngine(fixedRateBond, bondEngine))

## Floating rate bond (3M USD Libor + 0.1%)
## Should and will be priced on another curve later...

liborTermStructure <- RelinkableYieldTermStructureHandle()

libor3m <- USDLibor(Period(3,"Months"),liborTermStructure)
invisible(Index_addFixing(libor3m, Date(17, "July", 2008), 0.0278625))

floatingBondSchedule <- Schedule(Date(21, "October", 2005),
                                 Date(21, "October", 2010), Period("Quarterly"),
                                 UnitedStates("NYSE"),
                                 "Unadjusted", "Unadjusted",
                                 copyToR(DateGeneration(), "Backward"), TRUE)

floatingRateBond <- FloatingRateBond(settlementDays,
                                     faceAmount,
                                     floatingBondSchedule,
                                     libor3m,
                                     Actual360(),
                                     "ModifiedFollowing",
                                     2,
                                     1.0,         # Gearings
                                     0.001,       # Spreads
                                     numeric(0),  #[],      # Caps
                                     numeric(0),  #[],      # Floors
                                     TRUE,    # Fixing in arrears
                                     100.0,
                                     Date(21, "October", 2005))

invisible(Instrument_setPricingEngine(floatingRateBond, bondEngine))

## coupon pricers

pricer <- BlackIborCouponPricer()

## optionlet volatilities
volatility <- 0.0
vol <- ConstantOptionletVolatility(settlementDays,
                                   calendar,
                                   "ModifiedFollowing",
                                   volatility,
                                   Actual365Fixed())

invisible(IborCouponPricer_setCapletVolatility(pricer, OptionletVolatilityStructureHandle(vol)))
invisible(setCouponPricer(Bond_cashflows(floatingRateBond), pricer))


## Yield curve bootstrapping
invisible(RelinkableQuoteHandle_linkTo(forecastingTermStructure, depoSwapTermStructure))
invisible(RelinkableQuoteHandle_linkTo(discountingTermStructure, bondDiscountingTermStructure))

## We are using the depo & swap curve to estimate the future Libor rates
invisible(RelinkableQuoteHandle_linkTo(liborTermStructure, depoSwapTermStructure))

##
df <- data.frame(zeroCoupon=c(Instrument_NPV(zeroCouponBond),
                   Bond_cleanPrice(zeroCouponBond),
                   Bond_dirtyPrice(zeroCouponBond),
                   Bond_accruedAmount(zeroCouponBond),
                   NA,
                   NA,
                   100*Bond_yield(zeroCouponBond, Actual360(), "Compounded", "Annual")),
                 fixedRate=c(Instrument_NPV(fixedRateBond),
                   Bond_cleanPrice(fixedRateBond),
                   Bond_dirtyPrice(fixedRateBond),
                   Bond_accruedAmount(fixedRateBond),
                   100*Bond_previousCouponRate(fixedRateBond),
                   100*Bond_nextCouponRate(fixedRateBond),
                   100*Bond_yield(fixedRateBond, Actual360(), "Compounded", "Annual")),
                 floatingRate=c(Instrument_NPV(floatingRateBond),
                   Bond_cleanPrice(floatingRateBond),
                   Bond_dirtyPrice(floatingRateBond),
                   Bond_accruedAmount(floatingRateBond),
                   100*Bond_previousCouponRate(floatingRateBond),
                   100*Bond_nextCouponRate(floatingRateBond),
                   100*Bond_yield(floatingRateBond, Actual360(), "Compounded", "Annual")),
                 row.names=c("NPV", "Clean Price", "Dirty Price",
                 "Accrued Amount", "Previous Coupon", "Next Coupon", "Yield"))
cat("\nResults:\n")
print(df, digits=5)

# Other computations

cat("\nSample indirect computations (for the floating rate bond):\n")
yld <- Bond_yield(floatingRateBond, Actual360(), "Compounded", "Annual")
clnPrc <- Bond_cleanPrice(floatingRateBond, yld, Actual360(), "Compounded", "Annual", settlementDate)
cat("Yield to Clean Price: ", clnPrc, "\n")
yld <- Bond_yield(floatingRateBond, clnPrc, Actual360(), "Compounded", "Annual",settlementDate)
cat("Clean Price to Yield: ", yld, "\n")

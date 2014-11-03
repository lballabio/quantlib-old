# Copyright (C) 2008 Florent Grenier
# Copyright (C) 2010 Lluis Pujol Bajador
#
# This file is part of QuantLib, a free-software/open-source library
# for financial quantitative analysts and developers - http://quantlib.org/
#
# QuantLib is free software: you can redistribute it and/or modify it
# under the terms of the QuantLib license.  You should have received a
# copy of the license along with this program; if not, please email
# <quantlib-dev@lists.sf.net>. The license is also available online at
# <http://quantlib.org/license.shtml>.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the license for more details.

#    This example shows how to set up a term structure and then price
#    some simple bonds. The last part is dedicated to peripherical
#    computations such as "Yield to Price" or "Price to Yield"

from QuantLib import *

# global data
calendar = TARGET()
settlementDate = Date(18,September,2008)
settlementDate = calendar.adjust(settlementDate)

fixingDays = 3
settlementDays = 3

todaysDate = calendar.advance(settlementDate,-fixingDays, Days)
Settings.instance().evaluationDate = todaysDate

print('Today: '  + str(todaysDate))
print('Settlement Date: ' + str(settlementDate))

# market quotes

# constructing bond yield curve

zcQuotes = [(0.0096, Period(3,Months)),
            (0.0145, Period(6,Months)),
            (0.0194, Period(1,Years))]

zcBondsDayCounter = Actual365Fixed()

zcHelpers = [ DepositRateHelper(QuoteHandle(SimpleQuote(r)),
                                tenor, fixingDays,
                                calendar, ModifiedFollowing,
                                True, zcBondsDayCounter)
              for (r,tenor) in zcQuotes ]

# setup bonds

redemption = 100.0
numberOfBonds = 5

bondQuotes = [
    (Date(15,March,2005),    Date(31,August,2010), 0.02375, 100.390625),
    (Date(15,June,2005),     Date(31,August,2011), 0.04625, 106.21875),
    (Date(30,June,2006),     Date(31,August,2013), 0.03125, 100.59375),
    (Date(15,November,2002), Date(15,August,2018), 0.04000, 101.6875),
    (Date(15,May,1987),      Date (15,May,2038),   0.04500, 102.140625)
]

# Definition of the rate helpers

bondsHelpers =[]

for issueDate, maturity, couponRate, marketQuote in bondQuotes:
    schedule = Schedule(issueDate, maturity, Period(Semiannual),
                        UnitedStates(UnitedStates.GovernmentBond),
                        Unadjusted, Unadjusted,
                        DateGeneration.Backward, False)
    bondsHelpers.append(
        FixedRateBondHelper(QuoteHandle(SimpleQuote(marketQuote)),
                            settlementDays,
                            100.0,
                            schedule,
                            [couponRate],
                            ActualActual(ActualActual.Bond),
                            Unadjusted,
                            redemption,
                            issueDate))

###################################
####  **  CURVE BUILDING  **  #####
###################################

termStructureDayCounter =  ActualActual(ActualActual.ISDA)  

# not needed as defined in the interface file:  tolerance = 1.0e-15

bondInstruments = zcHelpers + bondsHelpers

bondDiscountingTermStructure = PiecewiseFlatForward(
    settlementDate, bondInstruments,
    termStructureDayCounter)

# Building of the Libor forecasting curve
# deposits
dQuotes = [(0.043375, Period(1,Weeks)),
           (0.031875, Period(1,Months)),
           (0.0320375, Period(3,Months)),
           (0.03385, Period(6,Months)),
           (0.0338125, Period(9,Months)),
           (0.0335125, Period(1,Years))]
sQuotes = [(0.0295, Period(2,Years)),
           (0.0323, Period(3,Years)),
           (0.0359, Period(5,Years)),
           (0.0412, Period(10,Years)),
           (0.0433, Period(15,Years))]

# deposits

depositDayCounter = Actual360()
depositHelpers = [
    DepositRateHelper(QuoteHandle(SimpleQuote(rate)),
                      tenor, fixingDays,
                      calendar, ModifiedFollowing,
                      True, depositDayCounter)
    for rate, tenor in dQuotes ]

# swaps

swFixedLegFrequency = Annual
swFixedLegConvention = Unadjusted
swFixedLegDayCounter = Thirty360(Thirty360.European)
swFloatingLegIndex = Euribor6M()
forwardStart = Period(1,Days)
swapHelpers = [
    SwapRateHelper(QuoteHandle(SimpleQuote(rate)), tenor,
                   calendar, swFixedLegFrequency,
                   swFixedLegConvention, swFixedLegDayCounter,
                   swFloatingLegIndex, QuoteHandle(),forwardStart)
    for rate, tenor in sQuotes ]

depoSwapInstruments = depositHelpers + swapHelpers

depoSwapTermStructure = PiecewiseFlatForward(
    settlementDate, depoSwapInstruments,
    termStructureDayCounter)

# Term structures that will be used for pricing:
# the one used for discounting cash flows

discountingTermStructure = RelinkableYieldTermStructureHandle()

# the one used for forward rate forecasting

forecastingTermStructure = RelinkableYieldTermStructureHandle()

#######################################
#        BONDS TO BE PRICED           #
#######################################

# common data

faceAmount = 100;

# pricing engine
bondEngine = DiscountingBondEngine(discountingTermStructure)

# zero coupon bond

zeroCouponBond = ZeroCouponBond(settlementDays,
                                UnitedStates(UnitedStates.GovernmentBond),
                                faceAmount,
                                Date(15,August,2013),
                                Following,
                                116.92,
                                Date(15,August,2003))

zeroCouponBond.setPricingEngine(bondEngine)

# fixed 4.5% US Treasury note

fixedBondSchedule = Schedule(Date(15, May, 2007),
                             Date(15,May,2017), Period(Semiannual),
                             UnitedStates(UnitedStates.GovernmentBond),
                             Unadjusted, Unadjusted,
                             DateGeneration.Backward, False)

fixedRateBond = FixedRateBond(settlementDays,
                              faceAmount,
                              fixedBondSchedule,
                              [0.045],
                              ActualActual(ActualActual.Bond),
                              ModifiedFollowing,
                              100.0, Date(15, May, 2007))

fixedRateBond.setPricingEngine(bondEngine);

# Floating rate bond (3M USD Libor + 0.1%)
# Should and will be priced on another curve later...

liborTermStructure = RelinkableYieldTermStructureHandle()

libor3m = USDLibor(Period(3,Months),liborTermStructure)
libor3m.addFixing(Date(17, July, 2008),0.0278625)

floatingBondSchedule = Schedule(Date(21, October, 2005),
                                Date(21, October, 2010), Period(Quarterly),
                                UnitedStates(UnitedStates.NYSE),
                                Unadjusted, Unadjusted,
                                DateGeneration.Backward, True);

floatingRateBond = FloatingRateBond(settlementDays,
                                    faceAmount,
                                    floatingBondSchedule,
                                    libor3m,
                                    Actual360(),
                                    ModifiedFollowing,
                                    spreads=[0.001],
                                    inArrears=True,
                                    issueDate=Date(21, October, 2005))

floatingRateBond.setPricingEngine(bondEngine);

# coupon pricers

pricer = BlackIborCouponPricer()

# optionlet volatilities
volatility = 0.0;
vol = ConstantOptionletVolatility(settlementDays,
                                  calendar,
                                  ModifiedFollowing,
                                  volatility,
                                  Actual365Fixed())

pricer.setCapletVolatility(OptionletVolatilityStructureHandle(vol))
setCouponPricer(floatingRateBond.cashflows(),pricer)

# Yield curve bootstrapping
forecastingTermStructure.linkTo(depoSwapTermStructure)
discountingTermStructure.linkTo(bondDiscountingTermStructure)

#We are using the depo & swap curve to estimate the future Libor rates
liborTermStructure.linkTo(depoSwapTermStructure)

#############################
#       BOND PRICING        #
#############################

# write column headings
def formatPrice(p,digits=2):
    format = '%%.%df' % digits
    return format % p

def formatRate(r,digits=2):
    format = '%%.%df %%%%' % digits
    return format % (r*100)

def report(Info, Zc, Fix, Frn, format):
    if format== "Price":
        Zc = formatPrice(Zc)
        Fix = formatPrice(Fix)
        Frn = formatPrice(Frn)
    else:
        if Info.find("coupon")==-1:
            Zc  = formatRate(Zc)
        else:
            Zc  = "N/A"
        Fix = formatRate(Fix)
        Frn = formatRate(Frn)
        
    print('%19s' % Info + ' |' +
          ' |'.join(['%10s' % y for y in [Zc, Fix, Frn] ]))



headers = [ "ZC", "Fixed", "Floating" ]
print('')
print('%19s' % '' + ' |' +
          ' |'.join(['%10s' % y for y in headers]))
                     
separator = " | "
widths = [ 18, 10, 10, 10 ]
width = widths[0] + widths[1] + widths[2]  + widths[3] + widths[3];
rule = "-" * width
dblrule = "=" * width
tab = " " * 8

print(rule)
report( "Net present value",
        zeroCouponBond.NPV(),
        fixedRateBond.NPV(),
        floatingRateBond.NPV(),
        "Price")
report( "Clean price",
        zeroCouponBond.cleanPrice(),
        fixedRateBond.cleanPrice(),
        floatingRateBond.cleanPrice(),
        "Price")
report( "Dirty price",
        zeroCouponBond.dirtyPrice(),
        fixedRateBond.dirtyPrice(),
        floatingRateBond.dirtyPrice(),
        "Price")
report( "Accrued coupon",
        zeroCouponBond.accruedAmount(),
        fixedRateBond.accruedAmount(),
        floatingRateBond.accruedAmount(),
        "Price")
report( "Previous coupon",
        0,
        fixedRateBond.previousCouponRate(),
        floatingRateBond.previousCouponRate(),
        "Rate")
report( "Next coupon",
        0,
        fixedRateBond.nextCouponRate(),
        floatingRateBond.nextCouponRate(),
        "Rate")
report( "Yield",
        zeroCouponBond.bondYield(Actual360(),Compounded,Annual)
        ,fixedRateBond.bondYield(Actual360(),Compounded,Annual),
        floatingRateBond.bondYield(Actual360(),Compounded,Annual),
        "Rate")
print('')

# Other computations

print("Sample indirect computations (for the floating rate bond): ")
print(rule)
print("Yield to Clean Price: " + formatPrice(
    floatingRateBond.cleanPrice(floatingRateBond.bondYield(Actual360(),
                                                           Compounded,Annual),
                                Actual360(),Compounded,Annual,settlementDate)))
print("Clean Price to Yield: " + formatRate(
    floatingRateBond.bondYield(floatingRateBond.cleanPrice(),
                               Actual360(),Compounded,
                               Annual,settlementDate)))

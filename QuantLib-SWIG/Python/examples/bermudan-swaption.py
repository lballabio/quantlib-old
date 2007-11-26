
# Copyright (C) 2004, 2005, 2006, 2007 StatPro Italia srl
#
# This file is part of QuantLib, a free-software/open-source library
# for financial quantitative analysts and developers - http://quantlib.org/
#
# QuantLib is free software: you can redistribute it and/or modify it under the
# terms of the QuantLib license.  You should have received a copy of the
# license along with this program; if not, please email
# <quantlib-dev@lists.sf.net>. The license is also available online at
# <http://quantlib.org/license.shtml>.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the license for more details.

from QuantLib import *

swaptionVols = [ # maturity,          length,             volatility
                 (Period(1, Years), Period(5, Years), 0.1148),
                 (Period(2, Years), Period(4, Years), 0.1108),
                 (Period(3, Years), Period(3, Years), 0.1070),
                 (Period(4, Years), Period(2, Years), 0.1021),
                 (Period(5, Years), Period(1, Years), 0.1000) ]

def formatVol(v, digits = 2):
    format = '%%.%df %%%%' % digits
    return format % (v * 100)

def formatPrice(p, digits = 2):
    format = '%%.%df' % digits
    return format % p

def calibrate(model, helpers, l, name):

    format = '%12s |%12s |%12s |%12s |%12s'
    header = format % ('maturity','length','volatility','implied','error')
    rule = '-' * len(header)
    dblrule = '=' * len(header)

    print
    print dblrule
    print name
    print rule

    method = Simplex(l);
    model.calibrate(helpers, method, EndCriteria(1000, 250, 1e-7, 1e-7, 1e-7))

    print 'Parameters: %s' % model.params()
    print rule

    print header
    print rule

    totalError = 0.0
    for swaption, helper in zip(swaptionVols, helpers):
        maturity, length, vol = swaption
        NPV = helper.modelValue()
        implied = helper.impliedVolatility(NPV, 1.0e-4, 1000, 0.05, 0.50)
        error = implied - vol
        totalError += abs(error)
        print format % (maturity, length,
                        formatVol(vol,4), formatVol(implied,4),
                        formatVol(error,4))
    averageError = totalError/len(helpers)

    print rule
    format = '%%%ds' % len(header)
    print format % ('Average error: ' + formatVol(averageError,4))
    print dblrule

todaysDate = Date(15,February,2002)
Settings.instance().evaluationDate = todaysDate
calendar = TARGET()
settlementDate = Date(19,February,2002);

# flat yield term structure impling 1x5 swap at 5%
rate = QuoteHandle(SimpleQuote(0.04875825))
termStructure = YieldTermStructureHandle(
    FlatForward(settlementDate,rate,Actual365Fixed()))

# define the ATM/OTM/ITM swaps

swapEngine = DiscountingSwapEngine(termStructure)

fixedLegFrequency = Annual
fixedLegTenor = Period(1,Years)
fixedLegConvention = Unadjusted
floatingLegConvention = ModifiedFollowing
fixedLegDayCounter = Thirty360(Thirty360.European);
floatingLegFrequency = Semiannual
floatingLegTenor = Period(6,Months)

payFixed = VanillaSwap.Payer
fixingDays = 2
index = Euribor6M(termStructure)
floatingLegDayCounter = index.dayCounter()

swapStart = calendar.advance(settlementDate,1,Years,floatingLegConvention)
swapEnd = calendar.advance(swapStart,5,Years,floatingLegConvention)

fixedSchedule = Schedule(swapStart, swapEnd,
                         fixedLegTenor, calendar,
                         fixedLegConvention, fixedLegConvention,
                         DateGeneration.Forward, False)
floatingSchedule = Schedule(swapStart, swapEnd,
                            floatingLegTenor, calendar,
                            floatingLegConvention, floatingLegConvention,
                            DateGeneration.Forward, False)

dummy = VanillaSwap(payFixed, 100.0,
                    fixedSchedule, 0.0, fixedLegDayCounter,
                    floatingSchedule, index, 0.0,
                    floatingLegDayCounter)
dummy.setPricingEngine(swapEngine)
atmRate = dummy.fairRate()

atmSwap = VanillaSwap(payFixed, 1000.0,
                      fixedSchedule, atmRate, fixedLegDayCounter,
                      floatingSchedule, index, 0.0,
                      floatingLegDayCounter)
otmSwap = VanillaSwap(payFixed, 1000.0,
                      fixedSchedule, atmRate*1.2, fixedLegDayCounter,
                      floatingSchedule, index, 0.0,
                      floatingLegDayCounter)
itmSwap = VanillaSwap(payFixed, 1000.0,
                      fixedSchedule, atmRate*0.8, fixedLegDayCounter,
                      floatingSchedule, index, 0.0,
                      floatingLegDayCounter)
atmSwap.setPricingEngine(swapEngine)
otmSwap.setPricingEngine(swapEngine)
itmSwap.setPricingEngine(swapEngine)

helpers = [ SwaptionHelper(maturity, length,
                           QuoteHandle(SimpleQuote(vol)),
                           index, index.tenor(), index.dayCounter(),
                           index.dayCounter(), termStructure)
            for maturity, length, vol in swaptionVols ]

times = {}
for h in helpers:
    for t in h.times():
        times[t] = 1
times = times.keys()
times.sort()

grid = TimeGrid(times, 30)

G2model = G2(termStructure)
HWmodel = HullWhite(termStructure)
HWmodel2 = HullWhite(termStructure)
BKmodel = BlackKarasinski(termStructure)

print "Calibrating..."

for h in helpers:
    h.setPricingEngine(G2SwaptionEngine(G2model,6.0,16))
calibrate(G2model, helpers, 0.05, "G2 (analytic formulae)")

for h in helpers:
    h.setPricingEngine(JamshidianSwaptionEngine(HWmodel))
calibrate(HWmodel, helpers, 0.05, "Hull-White (analytic formulae)")

for h in helpers:
    h.setPricingEngine(TreeSwaptionEngine(HWmodel2,grid))
calibrate(HWmodel2, helpers, 0.05, "Hull-White (numerical calibration)")

for h in helpers:
    h.setPricingEngine(TreeSwaptionEngine(BKmodel,grid))
calibrate(BKmodel, helpers, 0.05, "Black-Karasinski (numerical calibration)")


# price Bermudan swaptions on defined swaps

bermudanDates = [ d for d in fixedSchedule ][:-1]
exercise = BermudanExercise(bermudanDates)

format = '%17s |%17s |%17s |%17s'
header = format % ('model', 'in-the-money', 'at-the-money', 'out-of-the-money')
rule = '-' * len(header)
dblrule = '=' * len(header)

print
print dblrule
print 'Pricing Bermudan swaptions...'
print rule
print header
print rule

atmSwaption = Swaption(atmSwap, exercise)
otmSwaption = Swaption(otmSwap, exercise)
itmSwaption = Swaption(itmSwap, exercise)

atmSwaption.setPricingEngine(TreeSwaptionEngine(G2model, 50))
otmSwaption.setPricingEngine(TreeSwaptionEngine(G2model, 50))
itmSwaption.setPricingEngine(TreeSwaptionEngine(G2model, 50))

print format % ('G2 analytic', formatPrice(itmSwaption.NPV()),
                formatPrice(atmSwaption.NPV()), formatPrice(otmSwaption.NPV()))

atmSwaption.setPricingEngine(TreeSwaptionEngine(HWmodel, 50))
otmSwaption.setPricingEngine(TreeSwaptionEngine(HWmodel, 50))
itmSwaption.setPricingEngine(TreeSwaptionEngine(HWmodel, 50))

print format % ('HW analytic', formatPrice(itmSwaption.NPV()),
                formatPrice(atmSwaption.NPV()), formatPrice(otmSwaption.NPV()))

atmSwaption.setPricingEngine(TreeSwaptionEngine(HWmodel2, 50))
otmSwaption.setPricingEngine(TreeSwaptionEngine(HWmodel2, 50))
itmSwaption.setPricingEngine(TreeSwaptionEngine(HWmodel2, 50))

print format % ('HW numerical', formatPrice(itmSwaption.NPV()),
                formatPrice(atmSwaption.NPV()), formatPrice(otmSwaption.NPV()))

atmSwaption.setPricingEngine(TreeSwaptionEngine(BKmodel, 50))
otmSwaption.setPricingEngine(TreeSwaptionEngine(BKmodel, 50))
itmSwaption.setPricingEngine(TreeSwaptionEngine(BKmodel, 50))

print format % ('BK numerical', formatPrice(itmSwaption.NPV()),
                formatPrice(atmSwaption.NPV()), formatPrice(otmSwaption.NPV()))

print dblrule


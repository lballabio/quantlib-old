
# Copyright (C) 2000-2004 StatPro Italia srl
#
# This file is part of QuantLib, a free-software/open-source library
# for financial quantitative analysts and developers - http://quantlib.org/
#
# QuantLib is free software: you can redistribute it and/or modify it under the
# terms of the QuantLib license.  You should have received a copy of the
# license along with this program; if not, please email quantlib-dev@lists.sf.net
# The license is also available online at http://quantlib.org/html/license.html
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the license for more details.

from QuantLib import *

# global data
calendar = Calendar('TARGET')
todaysDate = Date(6,11,2001);
settlementDate = Date(8,11,2001);

# market quotes
deposits = { (1,'week'): 0.0382,
             (1,'month'): 0.0372,
             (3,'months'): 0.0363,
             (6,'months'): 0.0353,
             (9,'months'): 0.0348,
             (1,'year'): 0.0345 }

FRAs = { (3,6): 0.037125,
         (6,9): 0.037125,
         (9,12): 0.037125 }

futures = { Date(19,12,2001): 96.2875,
            Date(20,3,2002): 96.7875,
            Date(19,6,2002): 96.9875,
            Date(18,9,2002): 96.6875,
            Date(18,12,2002): 96.4875,
            Date(19,3,2003): 96.3875,
            Date(18,6,2003): 96.2875,
            Date(17,9,2003): 96.0875 }

swaps = { (2,'years'): 0.037125,
          (3,'years'): 0.0398,
          (5,'years'): 0.0443,
          (10,'years'): 0.05165,
          (15,'years'): 0.055175 }

# convert them to Quote objects
for n,unit in deposits.keys():
    deposits[(n,unit)] = SimpleQuote(deposits[(n,unit)])
for n,m in FRAs.keys():
    FRAs[(n,m)] = SimpleQuote(FRAs[(n,m)])
for d in futures.keys():
    futures[d] = SimpleQuote(futures[d])
for n,unit in swaps.keys():
    swaps[(n,unit)] = SimpleQuote(swaps[(n,unit)])

# build rate helpers

dayCounter = DayCounter('act/360')
settlementDays = 2
depositHelpers = [ DepositRateHelper(QuoteHandle(deposits[(n,unit)]),
                                     n, unit, settlementDays,
                                     calendar, 'mf', dayCounter)
                   for n, unit in [(1,'week'),(1,'month'),(3,'months'),
                                   (6,'months'),(9,'months'),(1,'year')] ]

dayCounter = DayCounter('act/360')
settlementDays = 2
fraHelpers = [ FraRateHelper(QuoteHandle(FRAs[(n,m)]),
                             n, m, settlementDays,
                             calendar, 'mf', dayCounter)
               for n, m in FRAs.keys() ]

dayCounter = DayCounter('act/360')
months = 3
futuresHelpers = [ FuturesRateHelper(QuoteHandle(futures[d]),
                                     d, months,
                                     calendar, 'mf', dayCounter)
                   for d in futures.keys() ]

settlementDays = 2
fixedLegFrequency = 1
fixedLegAdjustment = 0
fixedLegDayCounter = DayCounter('30/360')
floatingLegFrequency = 2
swapHelpers = [ SwapRateHelper(QuoteHandle(swaps[(n,unit)]),
                               n, unit, settlementDays,
                               calendar, 'mf',
                               fixedLegFrequency, fixedLegAdjustment,
                               fixedLegDayCounter, floatingLegFrequency)
                for n, unit in swaps.keys() ]

# term structure handles

discountTermStructure = TermStructureHandle()
forecastTermStructure = TermStructureHandle()

# term-structure construction

helpers = depositHelpers[:2] + futuresHelpers + swapHelpers[1:]
depoFuturesSwapCurve = PiecewiseFlatForward(todaysDate, settlementDate,
                                            helpers, DayCounter('act/360'))

helpers = depositHelpers[:3] + fraHelpers + swapHelpers
depoFraSwapCurve = PiecewiseFlatForward(todaysDate, settlementDate,
                                        helpers, DayCounter('act/360'))

# swaps to be priced

nominal = 1000000
length = 5
payFixed = 1

fixedLegFrequency = 1
fixedLegAdjustment = 0
fixedLegDayCounter = DayCounter('30/360')
fixedRate = 0.04

floatingLegFrequency = 2
spread = 0.0
fixingDays = 2
index = Xibor('Euribor', 6, 'months', forecastTermStructure)

spot = SimpleSwap(payFixed, settlementDate, length, 'years',
                  calendar, 'mf', nominal, fixedLegFrequency,
                  fixedRate, fixedLegAdjustment, fixedLegDayCounter,
                  floatingLegFrequency, index, fixingDays, spread,
                  discountTermStructure)

forward = SimpleSwap(payFixed, calendar.advance(settlementDate,1,'year'),
                     length, 'years', calendar, 'mf', nominal,
                     fixedLegFrequency, fixedRate, fixedLegAdjustment,
                     fixedLegDayCounter, floatingLegFrequency, index,
                     fixingDays, spread, discountTermStructure)

# price on the bootstrapped curves

def formatPrice(p,digits=2):
    format = '%%.%df' % digits
    return format % p

def formatRate(r,digits=2):
    format = '%%.%df %%%%' % digits
    return format % (r*100)

headers = ("term structure", "net present value",
           "fair spread", "fair fixed rate" )
separator = " | "

format = ''
width = 0
for h in headers[:-1]:
    format += '%%%ds' % len(h)
    format += separator
    width += len(h) + len(separator)
format += '%%%ds' % len(headers[-1])
width += len(headers[-1])

rule = "-" * width
dblrule = "=" * width
tab = " " * 8

def report(swap, name):
    print format % (name, formatPrice(swap.NPV(),2),
                    formatRate(swap.fairSpread(),4),
                    formatRate(swap.fairRate(),4))

print dblrule
print "5-year market swap-rate = %s" % formatRate(swaps[(5,'years')].value())
print dblrule

# price on two different term structures

print tab + "5-years swap paying %s" % formatRate(fixedRate)
print separator.join(headers)
print rule

discountTermStructure.linkTo(depoFuturesSwapCurve)
forecastTermStructure.linkTo(depoFuturesSwapCurve)
report(spot,'depo-fut-swap')

discountTermStructure.linkTo(depoFraSwapCurve)
forecastTermStructure.linkTo(depoFraSwapCurve)
report(spot,'depo-FRA-swap')

print rule

# price the 1-year forward swap

print tab + "5-years, 1-year forward swap paying %s" % formatRate(fixedRate)
print rule

discountTermStructure.linkTo(depoFuturesSwapCurve)
forecastTermStructure.linkTo(depoFuturesSwapCurve)
report(forward,'depo-fut-swap')

discountTermStructure.linkTo(depoFraSwapCurve)
forecastTermStructure.linkTo(depoFraSwapCurve)
report(forward,'depo-FRA-swap')

# modify the 5-years swap rate and reprice

swaps[(5,'years')].setValue(0.046)

print dblrule
print "5-year market swap-rate = %s" % formatRate(swaps[(5,'years')].value())
print dblrule
    
discountTermStructure.linkTo(depoFuturesSwapCurve)
forecastTermStructure.linkTo(depoFuturesSwapCurve)
report(spot,'depo-fut-swap')

discountTermStructure.linkTo(depoFraSwapCurve)
forecastTermStructure.linkTo(depoFraSwapCurve)
report(spot,'depo-FRA-swap')

print rule

print tab + "5-years, 1-year forward swap paying %s" % formatRate(fixedRate)
print rule

discountTermStructure.linkTo(depoFuturesSwapCurve)
forecastTermStructure.linkTo(depoFuturesSwapCurve)
report(forward,'depo-fut-swap')

discountTermStructure.linkTo(depoFraSwapCurve)
forecastTermStructure.linkTo(depoFraSwapCurve)
report(forward,'depo-FRA-swap')

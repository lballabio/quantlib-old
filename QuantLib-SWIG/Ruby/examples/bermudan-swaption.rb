
# Copyright (C) 2004 StatPro Italia srl
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

require 'QuantLib'
include QuantLib

SwaptionVols = [ # maturity,          length,             volatility
                 [Period.new(1, 'Year'),  Period.new(1,'Years'),  0.1810],
                 [Period.new(1, 'Year'),  Period.new(3,'Years'),  0.1590],
                 [Period.new(1, 'Year'),  Period.new(5,'Years'),  0.1400],
                 [Period.new(1, 'Year'),  Period.new(10,'Years'), 0.1220],
                 [Period.new(2, 'Years'), Period.new(5, 'Years'), 0.1290],
                 [Period.new(2, 'Years'), Period.new(7, 'Years'), 0.1230],
                 [Period.new(3, 'Years'), Period.new(5, 'Years'), 0.1201],
                 [Period.new(4, 'Years'), Period.new(4, 'Years'), 0.1189],
                 [Period.new(4, 'Years'), Period.new(5, 'Years'), 0.1146],
                 [Period.new(5, 'Years'), Period.new(3, 'Years'), 0.1183],
                 [Period.new(5, 'Years'), Period.new(5, 'Years'), 0.1108],
                 [Period.new(7, 'Years'), Period.new(2, 'Years'), 0.1110],
                 [Period.new(7, 'Years'), Period.new(5, 'Years'), 0.1040],
                 [Period.new(10,'Years'), Period.new(1, 'Year'),  0.1109],
                 [Period.new(10,'Years'), Period.new(5, 'Year'),  0.0977] ]

def formatVol(v, digits = 2)
  format = "%.#{digits}f %%"
  return sprintf(format, v*100)
end

def formatPrice(p, digits = 2)
  format = "%.#{digits}f"
  return sprintf(format, p)
end

def calibrate(model, helpers, l, name)

  format = '%12s |%12s |%12s |%12s |%12s'
  header = sprintf(format,'maturity','length','volatility','implied','error')
  rule = '-' * header.length
  dblrule = '=' * header.length

  puts 
  puts dblrule
  puts name
  puts rule

  method = Simplex.new(l, 1.0e-9);
  method.endCriteria = EndCriteria.new(1000, 1.0e-7)
  model.calibrate!(helpers, method)

  puts "Parameters: #{model.params}"
  puts rule

  puts header
  puts rule

  totalError = 0.0
  n = helpers.length
  
  (0...n).each do |i|
    maturity, length, vol = SwaptionVols[i]
    npv = helpers[i].modelValue
    implied = helpers[i].impliedVolatility(npv, 1.0e-4, 1000, 0.05, 0.50)
    error = implied - vol
    totalError += error.abs
    puts sprintf(format, maturity, length,
                 formatVol(vol,4), formatVol(implied,4),
                 formatVol(error,4))
  end
  
  averageError = totalError/helpers.length

  puts rule
  format = "%#{header.length}s"
  puts sprintf(format, "Average error: #{formatVol(averageError,4)}")
  puts dblrule
end



todaysDate = Date.new(15,2,2002)
calendar = Calendar.new('TARGET')
settlementDate = Date.new(19,2,2002);

settlementDays = 2
dayCounter = DayCounter.new('30/360')
depositRates = [[1, 'week',   0.03295],
                [1, 'month',  0.0331],
                [3, 'months', 0.0329],
                [6, 'months', 0.0333],
                [9, 'months', 0.0341],
                [1, 'year',   0.0353]]
depositHelpers = depositRates.map { |n,unit,rate|
  DepositRateHelper.new(QuoteHandle.new(SimpleQuote.new(rate)),
                        n, unit, settlementDays,
                        calendar, 'mf', dayCounter)
}

fixedLegFrequency = 1
fixedLegAdjustment = false
fixedLegDayCounter = DayCounter.new('30/360')
floatingLegFrequency = 2
swapRates = [[2,  'years', 0.04875],
             [3,  'years', 0.0438],
             [5,  'years', 0.0474325],
             [10, 'years', 0.051825],
             [20, 'years', 0.0545125]]
swapHelpers = swapRates.map { |n,unit,rate|
  SwapRateHelper.new(QuoteHandle.new(SimpleQuote.new(rate)),
                     n, unit, settlementDays,
                     calendar, 'mf',
                     fixedLegFrequency, fixedLegAdjustment,
                     fixedLegDayCounter, floatingLegFrequency)
}

termStructure = TermStructureHandle.new
termStructure.linkTo!(PiecewiseFlatForward.new(todaysDate, settlementDate,
                                               depositHelpers+swapHelpers,
                                               DayCounter.new('30/360')))


# define the ATM/OTM/ITM swaps

payFixed = true
fixingDays = 2
swapStart = settlementDate.plusYears(1)
index = Xibor.new('Euribor', 6, 'months', termStructure)
swapLength = 5
swapEnd = swapStart.plusYears(swapLength)

atmRate = SimpleSwap.new(payFixed, swapStart, swapLength, 'years', 
                         calendar, 'mf', 100.0, fixedLegFrequency, 0.0, 
                         fixedLegAdjustment, fixedLegDayCounter, 
                         floatingLegFrequency, index, fixingDays, 0.0, 
                         termStructure).fairRate

atmSwap = SimpleSwap.new(payFixed, swapStart, swapLength, 'years', 
                         calendar, 'mf', 100.0, fixedLegFrequency, atmRate, 
                         fixedLegAdjustment, fixedLegDayCounter, 
                         floatingLegFrequency, index, fixingDays, 0.0, 
                         termStructure)
otmSwap = SimpleSwap.new(payFixed, swapStart, swapLength, 'years', 
                         calendar, 'mf', 100.0, fixedLegFrequency, atmRate*1.2,
                         fixedLegAdjustment, fixedLegDayCounter, 
                         floatingLegFrequency, index, fixingDays, 0.0, 
                         termStructure)
itmSwap = SimpleSwap.new(payFixed, swapStart, swapLength, 'years', 
                         calendar, 'mf', 100.0, fixedLegFrequency, atmRate*0.8,
                         fixedLegAdjustment, fixedLegDayCounter, 
                         floatingLegFrequency, index, fixingDays, 0.0, 
                         termStructure)

helpers = SwaptionVols.map { |maturity, length, vol|
  SwaptionHelper.new(maturity, length,
                     QuoteHandle.new(SimpleQuote.new(vol)),
                     index, termStructure)
}

times = []
helpers.each { |h|
  times = times + h.times
}
times.sort!.uniq!

grid = TimeGrid.new(times)

HW = HullWhite.new(termStructure)
HW2 = HullWhite.new(termStructure)
BK = BlackKarasinski.new(termStructure)

puts "Calibrating..."

helpers.each { |h|
  h.pricingEngine = JamshidianSwaption.new(HW)
}
calibrate(HW, helpers, 0.05, "Hull-White (analytic formulae)")

helpers.each { |h|
  h.pricingEngine = TreeSwaption.new(HW2,grid)
}
calibrate(HW2, helpers, 0.05, "Hull-White (numerical calibration)")

helpers.each { |h|
  h.pricingEngine = TreeSwaption.new(BK,grid)
}
calibrate(BK, helpers, 0.05, "Black-Karasinski (numerical calibration)")


# price Bermudan swaptions on defined swaps

schedule = Schedule.new(calendar, swapStart, swapEnd, 1, 'mf', true)
bermudanDates = schedule.map { |d| d }
exercise = BermudanExercise.new(bermudanDates[0...-1])

format = '%17s |%17s |%17s |%17s'
header = sprintf(format,'model','in-the-money',
                 'at-the-money','out-of-the-money')
rule = '-' * header.length
dblrule = '=' * header.length

puts
puts dblrule
puts 'Pricing Bermudan swaptions...'
puts rule
puts header
puts rule

atmSwaption = Swaption.new(atmSwap, exercise, termStructure,
                           TreeSwaption.new(HW, 100))
otmSwaption = Swaption.new(otmSwap, exercise, termStructure,
                           TreeSwaption.new(HW, 100))
itmSwaption = Swaption.new(itmSwap, exercise, termStructure,
                           TreeSwaption.new(HW, 100))

puts sprintf(format,'HW analytic', formatPrice(itmSwaption.NPV),
             formatPrice(atmSwaption.NPV), formatPrice(otmSwaption.NPV))

atmSwaption.pricingEngine = TreeSwaption.new(HW2, 100)
otmSwaption.pricingEngine = TreeSwaption.new(HW2, 100)
itmSwaption.pricingEngine = TreeSwaption.new(HW2, 100)

puts sprintf(format,'HW numerical', formatPrice(itmSwaption.NPV),
             formatPrice(atmSwaption.NPV), formatPrice(otmSwaption.NPV))

atmSwaption.pricingEngine = TreeSwaption.new(BK, 100)
otmSwaption.pricingEngine = TreeSwaption.new(BK, 100)
itmSwaption.pricingEngine = TreeSwaption.new(BK, 100)

puts sprintf(format,'BK numerical', formatPrice(itmSwaption.NPV),
             formatPrice(atmSwaption.NPV), formatPrice(otmSwaption.NPV))

puts dblrule


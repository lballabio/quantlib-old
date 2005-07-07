
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
                 [Period.new(1, Years), Period.new(5, Years), 0.1148],
                 [Period.new(2, Years), Period.new(4, Years), 0.1108],
                 [Period.new(3, Years), Period.new(3, Years), 0.1070],
                 [Period.new(4, Years), Period.new(2, Years), 0.1021],
                 [Period.new(5, Years), Period.new(1, Years), 0.1000] ]

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

todaysDate = Date.new(15,February,2002)
Settings.instance.evaluationDate = todaysDate
calendar = TARGET.new
settlementDate = Date.new(19,February,2002);

# flat yield term structure impling 1x5 swap at 5%
rate = QuoteHandle.new(SimpleQuote.new(0.04875825))
termStructure = YieldTermStructureHandle.new
termStructure.linkTo!(FlatForward.new(settlementDate,rate,Actual365Fixed.new))


# define the ATM/OTM/ITM swaps
fixedLegFrequency = Annual
fixedLegConvention = Unadjusted
floatingLegConvention = ModifiedFollowing
fixedLegDayCounter = Thirty360.new(Thirty360::European)
floatingLegFrequency = Semiannual

payFixed = true
fixingDays = 2
index = Euribor.new(6, Months, termStructure)

swapStart = calendar.advance(settlementDate,1,Years,floatingLegConvention)
swapEnd = calendar.advance(swapStart,5,Years,floatingLegConvention)

fixedSchedule = Schedule.new(calendar, swapStart, swapEnd,
                             fixedLegFrequency, fixedLegConvention)
floatingSchedule = Schedule.new(calendar, swapStart, swapEnd,
                                floatingLegFrequency, floatingLegConvention)

atmRate = SimpleSwap.new(payFixed, 100.0,
                         fixedSchedule, 0.0, fixedLegDayCounter,
                         floatingSchedule, index, fixingDays, 0.0,
                         termStructure).fairRate

atmSwap = SimpleSwap.new(payFixed, 1000.0,
                         fixedSchedule, atmRate, fixedLegDayCounter,
                         floatingSchedule, index, fixingDays, 0.0,
                         termStructure)
otmSwap = SimpleSwap.new(payFixed, 1000.0,
                         fixedSchedule, atmRate*1.2, fixedLegDayCounter,
                         floatingSchedule, index, fixingDays, 0.0,
                         termStructure)
itmSwap = SimpleSwap.new(payFixed, 1000.0,
                         fixedSchedule, atmRate*0.8, fixedLegDayCounter,
                         floatingSchedule, index, fixingDays, 0.0,
                         termStructure)

helpers = SwaptionVols.map { |maturity, length, vol|
  SwaptionHelper.new(maturity, length,
                     QuoteHandle.new(SimpleQuote.new(vol)),
                     index, index.frequency, index.dayCounter,
                     termStructure)
}

times = []
helpers.each { |h|
  times = times + h.times
}
times.sort!.uniq!

grid = TimeGrid.new(times, 30)

G2model = G2.new(termStructure)
HWmodel = HullWhite.new(termStructure)
HWmodel2 = HullWhite.new(termStructure)
BKmodel = BlackKarasinski.new(termStructure)

puts "Calibrating..."

helpers.each { |h|
  h.pricingEngine = G2SwaptionEngine.new(G2model,6.0,16)
}
calibrate(G2model, helpers, 0.05, "G2 (analytic formulae)")

helpers.each { |h|
  h.pricingEngine = JamshidianSwaptionEngine.new(HWmodel)
}
calibrate(HWmodel, helpers, 0.05, "Hull-White (analytic formulae)")

helpers.each { |h|
  h.pricingEngine = TreeSwaptionEngine.new(HWmodel2,grid)
}
calibrate(HWmodel2, helpers, 0.05, "Hull-White (numerical calibration)")

helpers.each { |h|
  h.pricingEngine = TreeSwaptionEngine.new(BKmodel,grid)
}
calibrate(BKmodel, helpers, 0.05, "Black-Karasinski (numerical calibration)")


# price Bermudan swaptions on defined swaps

bermudanDates = fixedSchedule.map { |d| d }
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
                           TreeSwaptionEngine.new(G2model, 50))
otmSwaption = Swaption.new(otmSwap, exercise, termStructure,
                           TreeSwaptionEngine.new(G2model, 50))
itmSwaption = Swaption.new(itmSwap, exercise, termStructure,
                           TreeSwaptionEngine.new(G2model, 50))

puts sprintf(format,'G2 analytic', formatPrice(itmSwaption.NPV),
             formatPrice(atmSwaption.NPV), formatPrice(otmSwaption.NPV))

atmSwaption.pricingEngine = TreeSwaptionEngine.new(HWmodel, 50)
otmSwaption.pricingEngine = TreeSwaptionEngine.new(HWmodel, 50)
itmSwaption.pricingEngine = TreeSwaptionEngine.new(HWmodel, 50)

puts sprintf(format,'HW analytic', formatPrice(itmSwaption.NPV),
             formatPrice(atmSwaption.NPV), formatPrice(otmSwaption.NPV))

atmSwaption.pricingEngine = TreeSwaptionEngine.new(HWmodel2, 50)
otmSwaption.pricingEngine = TreeSwaptionEngine.new(HWmodel2, 50)
itmSwaption.pricingEngine = TreeSwaptionEngine.new(HWmodel2, 50)

puts sprintf(format,'HW numerical', formatPrice(itmSwaption.NPV),
             formatPrice(atmSwaption.NPV), formatPrice(otmSwaption.NPV))

atmSwaption.pricingEngine = TreeSwaptionEngine.new(BKmodel, 50)
otmSwaption.pricingEngine = TreeSwaptionEngine.new(BKmodel, 50)
itmSwaption.pricingEngine = TreeSwaptionEngine.new(BKmodel, 50)

puts sprintf(format,'BK numerical', formatPrice(itmSwaption.NPV),
             formatPrice(atmSwaption.NPV), formatPrice(otmSwaption.NPV))

puts dblrule


=begin
 Copyright (C) 2000, 2001, 2002 RiskMap srl

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it under the
 terms of the QuantLib license.  You should have received a copy of the
 license along with this program; if not, please email ferdinando@ametrano.net
 The license is also available online at http://quantlib.org/html/license.html

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
=end

# $Id$

require 'QuantLibc'

module QuantLibc
  
  # mixins
  
  # Comparable classes
  class Date
    include Comparable
  end
  
  class Calendar
    include Comparable
  end
  
  # Enumerable classes
  class Array
    include Enumerable
  end

  class IntVector
    include Enumerable
  end
  
  class DoubleVector
    include Enumerable
  end
  
  class DateVector
    include Enumerable
  end

  class CashFlowVector
    include Enumerable
  end

  class Path
    include Enumerable
  end
  
  # interface enhancements
  class Array
    def *(x)
      if (x.is_a? Float) || (x.is_a? Integer)
        mul_d(x)
      else
        mul_a(x)
      end
    end
  end

  class Observer
    alias cpp_initialize initialize
    alias cpp_registerWith registerWith
    alias cpp_unregisterWith unregisterWith
    def initialize(*args,&block)
      if (block)
        cpp_initialize(block)
      elsif (args.size > 0 and args[0].respond_to? "call")
        cpp_initialize(args[0])
      else
        raise "block or callable object needed"
      end
    end
    def registerWith(x)
      cpp_registerWith(x.toObservable)
    end
    def unregisterWith(x)
      cpp_unregisterWith(x.toObservable)
    end
  end

  class History
    alias cpp_initialize initialize
    def initialize(dates,values)
      vs = values.map { |v| v || QuantLibc::nullDouble }
      cpp_initialize(dates,vs)
    end
  end

  class MarketElementHandle
    alias cpp_initialize initialize
    def initialize(marketElement = nil)
      cpp_initialize()
      unless marketElement.nil?
        linkTo! marketElement
      end
    end
  end

  class TermStructureHandle
    alias cpp_initialize initialize
    def initialize(termStructure = nil)
      cpp_initialize()
      unless termStructure.nil?
        linkTo! termStructure
      end
    end
  end

  class FlatForward
    alias cpp_initialize initialize
    def initialize(settlementDate,forward,dayCounter)
      if (forward.is_a? Float) || (forward.is_a? Integer)
        h = MarketElementHandle.new(SimpleMarketElement.new(forward))
        cpp_initialize(settlementDate,h,dayCounter)
      else
        cpp_initialize(settlementDate,forward,dayCounter)
      end
    end
  end

  class SimpleSwap
    alias cpp_initialize initialize
    def initialize(payFixedRate, startDate, n, unit, calendar,\
                   rollingConvention, nominal, fixedFrequency, fixedRate,\
                   fixedIsAdjusted, fixedDayCount, floatingFrequency,\
                   index, indexFixingDays, spread, termStructure,\
                   isinCode = "unknown", description = "interest rate swap")
      fixedLeg = FixedSwapLeg.new(fixedFrequency, fixedRate,\
                                  fixedIsAdjusted, fixedDayCount)
      floatingLeg = FloatingSwapLeg.new(floatingFrequency, index,\
                                        indexFixingDays, spread)
      cpp_initialize(payFixedRate, startDate, n, unit, calendar, 
                     rollingConvention, nominal, fixedLeg, floatingLeg,
                     termStructure, isinCode, description)
    end
  end

end

QuantLib = QuantLibc


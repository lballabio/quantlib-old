=begin
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it under the
 terms of the QuantLib license.  You should have received a copy of the
 license along with this program; if not, please email quantlib-dev@lists.sf.net
 The license is also available online at http://quantlib.org/html/license.html

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
=end

require 'QuantLibc'

module QuantLibc
  
  class Observer
    alias cpp_initialize initialize
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
      _registerWith(x.toObservable)
    end
    def unregisterWith(x)
      _unregisterWith(x.toObservable)
    end
  end

  class History
    alias cpp_initialize initialize
    def initialize(dates,values)
      vs = values.map { |v| v || QuantLibc::nullDouble }
      cpp_initialize(dates,vs)
    end
  end

  class Date
    alias cpp_add +
    alias cpp_sub -
    def +(x)
      if x.type == Array and x.length == 2
        cpp_add(Period.new(x[0],x[1]))
      else
        cpp_add(x)
      end
    end
    def -(x)
      if x.type == Array and x.length == 2
        cpp_sub(Period.new(x[0],x[1]))
      else
        cpp_sub(x)
      end
    end
  end

  TermStructure = YieldTermStructure
  TermStructureHandle = YieldTermStructureHandle

  CapFlatVolatilityStructure = CapVolatilityStructure
  CapFlatVolatilityStructureHandle = CapVolatilityStructureHandle

  CapFlatVolatilityVector = CapVolatilityVector

end

QuantLib = QuantLibc


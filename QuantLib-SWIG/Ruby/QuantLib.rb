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
  
  
  # interface enhancements
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
  end

  class History
    alias cpp_initialize initialize
    def initialize(dates,values)
      vs = []
      values.each { |v| vs.push(v || QuantLibc::nullDouble) }
      cpp_initialize(dates,vs)
    end
  end

end

QuantLib = QuantLibc


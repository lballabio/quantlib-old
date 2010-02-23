=begin
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it
 under the terms of the QuantLib license.  You should have received a
 copy of the license along with this program; if not, please email
 <quantlib-dev@lists.sf.net>. The license is also available online at
 <http://quantlib.org/license.shtml>.

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
=end

require 'QuantLib'
require 'test/unit/testcase'
require 'test/unit/ui/console/testrunner'

def Gauss(x)
  Math.exp(-x*x/2.0)/Math.sqrt(2*Math::PI)
end

class SegmentIntegralTest < Test::Unit::TestCase
  def name
    "Testing segment integral"
  end
  def testCalculation
    tolerance = 1e-4
    integrate = QuantLib::SegmentIntegral.new(10000)

    calculated = integrate.call(0.0,1.0) { |x| 1.0 }
    expected   = 1.0
    unless (calculated-expected).abs <= tolerance
      flunk(<<-MESSAGE

    integrating f(x) = 1.0
        calculated: #{calculated}
        expected  : #{expected}

            MESSAGE
            )
    end
    
    calculated = integrate.call(0.0,1.0) { |x| x }
    expected   = 0.5
    unless (calculated-expected).abs <= tolerance
      flunk(<<-MESSAGE

    integrating f(x) = x
        calculated: #{calculated}
        expected  : #{expected}

            MESSAGE
            )
    end
    
    calculated = integrate.call(0.0,1.0) { |x| x*x }
    expected   = 1.0/3.0
    unless (calculated-expected).abs <= tolerance
      flunk(<<-MESSAGE

    integrating f(x) = x^2
        calculated: #{calculated}
        expected  : #{expected}

            MESSAGE
            )
    end
    
    calculated = integrate.call(0.0,Math::PI) { |x| Math.sin(x) }
    expected   = 2.0
    unless (calculated-expected).abs <= tolerance
      flunk(<<-MESSAGE

    integrating f(x) = sin(x)
        calculated: #{calculated}
        expected  : #{expected}

            MESSAGE
            )
    end
    
    calculated = integrate.call(0.0,Math::PI) { |x| Math.cos(x) }
    expected   = 0.0
    unless (calculated-expected).abs <= tolerance
      flunk(<<-MESSAGE

    integrating f(x) = cos(x)
        calculated: #{calculated}
        expected  : #{expected}

            MESSAGE
            )
    end

    calculated = integrate.call(-10.0,10.0) { |x| Gauss(x) }
    expected   = 1.0
    unless (calculated-expected).abs <= tolerance
      flunk(<<-MESSAGE

    integrating f(x) = Gauss(x)
        calculated: #{calculated}
        expected  : #{expected}

            MESSAGE
            )
    end
    
  end
end

if $0 == __FILE__
  Test::Unit::UI::Console::TestRunner.run(SegmentIntegralTest)
end


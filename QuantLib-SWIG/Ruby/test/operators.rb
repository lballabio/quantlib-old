=begin
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl

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

require 'QuantLib'
require 'test/unit/testcase'
require 'test/unit/ui/console/testrunner'

# define the norm of a discretized function and the difference between two
module Discretized
  def sum
    result = 0
    each { |i| result += i }
    result
  end
  def norm(h)
    f2 = map { |i| i*i }
    # numeric integral of f^2
    Math.sqrt(h*(f2.sum-0.5*f2[0]-0.5*f2[-1]))
  end
  def diff(a)
    result = Array.new
    each_with_index { |x,i| result << x-a[i] }
    result
  end
end

class Array
  include Discretized
end

# Test

class OperatorTest < Test::Unit::TestCase
  def name
    "Testing differential operators"
  end
  def testOperators
    average = 0.0
    sigma = 1.0

    normal = QuantLib::NormalDistribution.new(average, sigma)
    cum =    QuantLib::CumulativeNormalDistribution.new(average, sigma)

    xMin = average - 4*sigma
    xMax = average + 4*sigma

    n = 10001
    h = (xMax-xMin)/(n-1)

    x = Array.new(n)        # creates a list of N elements
    0.upto(n-1) { |i| x[i] = xMin+h*i }

    y = x.map { |z| normal.call(z) }
    yIntegrated = x.map { |z| cum.call(z) }
    yDerivative = x.map { |z| normal.derivative(z) }

    # define the differential operators...
    d = QuantLib::DZero.new(n,h)
    d2 = QuantLib::DPlusDMinus.new(n,h)
    # ...and calculate the derivatives
    yTemp  = d.applyTo(yIntegrated)
    ydTemp = d2.applyTo(yIntegrated)

    # check that first order derivative operator = gaussian
    e = y.diff(yTemp).norm(h)
    unless e <= 1.0e-6
      flunk("\nnorm of FD 1st deriv. of cum " + \
            "minus analytic gaussian: #{e}\n")
    end

    # check that second order derivative operator = normal.derivative
    e = yDerivative.diff(ydTemp).norm(h)
    unless e <= 1.0e-4
      flunk("\nnorm of FD 2nd deriv. of cum " + \
            "minus analytic gaussian derivative: #{e}\n")
    end

  end
end

if $0 == __FILE__
  Test::Unit::UI::Console::TestRunner.run(OperatorTest)
end


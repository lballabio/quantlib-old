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

require 'QuantLib'
require 'test/unit/testcase'
require 'test/unit/ui/console/testrunner'

module Enumerable
    def sum
        s = 0.0
        each { |i| s += i }
        s
    end
end

class Array
    def times(v)
        a = []
        each_with_index { |x,i| a << x*v[i] }
        a
    end
end

class StatisticsTest < Test::Unit::TestCase
    def name
      "Testing statistics"
    end
    def testCalculation
        tolerance = 1.0e-9
        data      = [3.0, 4.0, 5.0, 2.0, 3.0, 4.0, 5.0, 6.0, 4.0, 7.0]
        weights   = [1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]

        s = QuantLib::Statistics.new
        s.add(data, weights)

        unless s.samples == data.length
            flunk(<<-MESSAGE

    wrong number of samples
        calculated: #{s.samples}
        expected:   #{data.length}

                  MESSAGE
                  )
        end

        unless s.weightSum == weights.sum
            flunk(<<-MESSAGE

    wrong sum of weights
        calculated: #{s.weightSum}
        expected:   #{weights.sum}

                  MESSAGE
                  )
        end

        unless s.min == data.min
            flunk(<<-MESSAGE

    wrong minimum value
        calculated: #{s.min}
        expected:   #{data.min}

                  MESSAGE
                  )
        end

        unless s.max == data.max
            flunk(<<-MESSAGE

    wrong maximum value
        calculated: #{s.max}
        expected:   #{data.max}

                  MESSAGE
                  )
        end

        unless (s.mean-data.times(weights).sum/weights.sum).abs <= tolerance
            flunk(<<-MESSAGE

    wrong mean value
        calculated: #{s.mean}
        expected:   #{data.times(weights).sum/weights.sum}

                  MESSAGE
                  )
        end

        unless (s.variance-2.23333333333).abs <= tolerance
            flunk(<<-MESSAGE

    wrong variance
        calculated: #{s.variance}
        expected:   2.23333333333

                  MESSAGE
                  )
        end

        unless (s.standardDeviation-1.4944341181).abs <= tolerance
            flunk(<<-MESSAGE

    wrong std. deviation
        calculated: #{s.standardDeviation}
        expected:   1.4944341181

                  MESSAGE
                  )
        end

        unless (s.skewness-0.359543071407).abs <= tolerance
            flunk(<<-MESSAGE

    wrong skewness
        calculated: #{s.skewness}
        expected:   0.359543071407

                  MESSAGE
                  )
        end

        unless (s.kurtosis+0.151799637209).abs <= tolerance
            flunk(<<-MESSAGE

    wrong kurtosis
        calculated: #{s.kurtosis}
        expected:   -0.151799637209

                  MESSAGE
                  )
        end
    end
end

if $0 == __FILE__
    Test::Unit::UI::Console::TestRunner.run(StatisticsTest)
end


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

require 'QuantLib'
require 'runit/testcase'
require 'runit/cui/testrunner'

# define a Gaussian
def gaussian(x, average, sigma)
  normFact = sigma*Math.sqrt(2*Math::PI)
  dx = x-average
  Math.exp(-dx*dx/(2.0*sigma*sigma))/normFact
end

class RiskStatisticsTest < RUNIT::TestCase
  def name
    "Testing risk statistics..."
  end
  def test
    s = QuantLib::RiskStatistics.new
    [-100.0, 0.0, 100.0].each do |average|
    [0.1, 1.0, 10].each       do |sigma|
        
        n = 25000
        numberOfSigma = 15

        #target cannot be changed:
        #it is a strong assumption to compute values to be checked
        target = average
        normal = QuantLib::NormalDistribution.new(average, sigma)

        dataMin = average - numberOfSigma*sigma
        dataMax = average + numberOfSigma*sigma
        # even NOT to include average
        h = (dataMax-dataMin)/(n-1)

        data = Array.new(n)        # creates a list of N elements
        0.upto(n-1) { |i| data[i] = dataMin+h*i }
        
        weights = data.map { |x| gaussian(x,average,sigma) }
        s.addWeightedSequence(data, weights)

        unless s.samples == n
          assert_fail(<<-MESSAGE

    wrong number of samples
        calculated: #{s.samples}
        expected  : #{n}

                      MESSAGE
                      )
        end

        rightWeightSum = 0.0
        weights.each { |w| rightWeightSum = rightWeightSum + w }
        unless s.weightSum == rightWeightSum
          assert_fail(<<-MESSAGE

    wrong sum of weights
        calculated: #{s.weightSum}
        expected  : #{rightWeightSum}

                      MESSAGE
                      )
        end

        unless s.min == dataMin
          assert_fail(<<-MESSAGE

    wrong minimum value
        calculated: #{s.min}
        expected  : #{dataMin}

                      MESSAGE
                      )
        end
        unless (s.max-dataMax).abs <= 1e-13
          assert_fail(<<-MESSAGE

    wrong maximum value
        calculated: #{s.max}
        expected  : #{dataMax}

                      MESSAGE
                      )
        end

        check = (s.mean-average).abs
        if average != 0.0
          check = check/average.abs
        end
        unless check <= 1e-13
          assert_fail(<<-MESSAGE

    wrong mean value
        calculated: #{s.mean}
        expected  : #{average}

                      MESSAGE
                      )
        end

        sigma2 = sigma*sigma
        unless (s.variance-sigma2).abs/sigma2 <= 1e-4
          assert_fail(<<-MESSAGE

    wrong variance
        calculated: #{s.variance}
        expected  : #{sigma2}

                      MESSAGE
                      )
        end

        unless (s.standardDeviation-sigma).abs/sigma <= 1e-4
          assert_fail(<<-MESSAGE

    wrong standard deviation
        calculated: #{s.standardDeviation}
        expected  : #{sigma}

                      MESSAGE
                      )
        end

        unless s.skewness.abs <= 1e-4
          assert_fail(<<-MESSAGE

    wrong skewness
        calculated: #{s.skewness}
        expected  : 0.0

                      MESSAGE
                      )
        end

        unless s.kurtosis.abs <= 1e-1
          assert_fail(<<-MESSAGE
                      
    wrong kurtosis
        calculated: #{kurtosis}
        expected  : 0.0
                      MESSAGE
                      )
        end

        cum = QuantLib::CumulativeNormalDistribution.new(average, sigma)
        twoStdDev = cum.call(average+2.0*sigma)
        rightPotentialUpside = [average+2.0*sigma, 0.0].max
        potentialUpside = s.potentialUpside(twoStdDev)
        check = (potentialUpside-rightPotentialUpside).abs
        if rightPotentialUpside != 0.0
          check = check/rightPotentialUpside
        end
        unless check <= 1e-3
          assert_fail(<<-MESSAGE

    wrong potential upside
        calculated: #{potentialUpside}
        expected:   #{rightPotentialUpside}

                      MESSAGE
                      )
        end

        rightVar = -[average-2.0*sigma, 0.0].min
        var = s.valueAtRisk(twoStdDev)
        check = (var-rightVar).abs
        if rightVar != 0.0
            check = check/rightVar
        end
        unless check <= 1e-3
          assert_fail(<<-MESSAGE

    wrong value at risk
        calculated: #{var}
        expected:   #{rightVar}

                      MESSAGE
                      )
        end

        tempVar = average-2.0*sigma
        rightExShortfall = average - \
            sigma*sigma*gaussian(tempVar, average, sigma)/(1.0-twoStdDev)
        rightExShortfall = -[rightExShortfall, 0.0].min
        exShortfall = s.expectedShortfall(twoStdDev)
        check = (exShortfall-rightExShortfall).abs
        if rightExShortfall != 0.0
            check = check/rightExShortfall
        end
        unless check <= 1e-4
          assert_fail(<<-MESSAGE

    wrong expected shortfall
        calculated: #{exShortfall}
        expected:   #{rightExShortfall}
        
                      MESSAGE
                      )
        end

        rightShortfall = 0.5
        shortfall = s.shortfall(target)
        unless (shortfall-rightShortfall).abs/rightShortfall <= 1e-8
          assert_fail(<<-MESSAGE

    wrong shortfall
        calculated: #{shortFall}
        expected:   #{rightShortfall}

                      MESSAGE
                      )
          end

        rightAvgShortfall = sigma/Math.sqrt(2*Math::PI)
        avgShortfall = s.averageShortfall(target)
        check = (avgShortfall-rightAvgShortfall).abs/rightAvgShortfall
        unless check <= 1e-4
          assert_fail(<<-MESSAGE

    wrong average shortfall
        calculated: #{avgShortFall}
        expected:   #{rightAvgShortfall}
        
                      MESSAGE
                      )
        end

        s.reset!
      end
    end
  end
end

if $0 == __FILE__
  RUNIT::CUI::TestRunner.run(RiskStatisticsTest.suite)
end


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
require 'test/unit'
require 'test/unit/ui/console/testrunner'

class CovarianceTest < Test::Unit::TestCase
  def name
    "Testing covariance calculation"
  end
  def testCalculation
    vol = [0.1, 0.5, 1.0]
    corr = [[1.0, 0.2, 0.5],
            [0.2, 1.0, 0.8],
            [0.5, 0.8, 1.0]]

    n = vol.length
    expCov = QuantLib::Matrix.new(n,n)
    0.upto(n-1) { |i|
      expCov[i][i] = vol[i]*vol[i]
      0.upto(i-1) { |j|
        expCov[i][j] = corr[i][j]*vol[i]*vol[j]
        expCov[j][i] = expCov[i][j]
      }
    }

    calcCov = QuantLib::getCcovariance(vol,corr)

    0.upto(n-1) do |i|
      0.upto(n-1) do |j|
        unless (calcCov[i][j] - expCov[i][j]).abs <= 1e-10
          flunk(<<-MESSAGE
                      
    cov[#{i}][#{j}]: #{calcCov[i][j]}
    expected : #{expCov[i][j]}
                        
                  MESSAGE
                  )
        end
      end
    end
  end
end

if $0 == __FILE__
  Test::Unit::UI::Console::TestRunner.run(CovarianceTest)
end


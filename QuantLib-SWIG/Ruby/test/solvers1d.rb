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

class Solver1DTest < RUNIT::TestCase
    def name
      "Testing 1-D solvers..."
    end
    def test
      [QuantLib::Brent,
       QuantLib::Bisection, 
       QuantLib::FalsePosition, 
       QuantLib::Ridder,
       QuantLib::Secant].each { |factory|
        solver = factory.new
        [1.0e-4, 1.0e-6, 1.0e-8].each { |accuracy|
          root = solver.solve(accuracy,1.5,0.1) { |x| x*x-1.0 }
          unless (root-1.0).abs <= accuracy
            assert_fail(<<-MESSAGE

#{factory}
    solve():
    expected:         1.0
    calculated root:  #{root}
    accuracy:         #{accuracy}

                        MESSAGE
                        )
          end

          root = solver.bracketedSolve(accuracy,1.5,0.0,1.0) { |x| x*x-1.0 }
          unless (root-1.0).abs <= accuracy
            assert_fail(<<-MESSAGE

#{factory}
    bracketedSolve():
    expected:         1.0
    calculated root:  #{root}
    accuracy:         #{accuracy}

                        MESSAGE
                        )
          end
        }
      }
    end
end

if $0 == __FILE__
    RUNIT::CUI::TestRunner.run(Solver1DTest.suite)
end


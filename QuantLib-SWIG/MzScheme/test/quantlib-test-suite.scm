; Copyright (C) 2000, 2001, 2002 RiskMap srl
;
; This file is part of QuantLib, a free-software/open-source library
; for financial quantitative analysts and developers - http://quantlib.org/
;
; QuantLib is free software developed by the QuantLib Group; you can
; redistribute it and/or modify it under the terms of the QuantLib License;
; either version 1.0, or (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; QuantLib License for more details.
;
; You should have received a copy of the QuantLib License along with this
; program; if not, please email ferdinando@ametrano.net
;
; The QuantLib License is also available at http://quantlib.org/license.html
; The members of the QuantLib Group are listed in the QuantLib License
;
; $Id$

(load "unittest.scm")
(load "date.scm")
(load "daycounters.scm")
(load "distributions.scm")
(load "instruments.scm")
(load "marketelements.scm")
(load "operators.scm")
(load "riskstatistics.scm")
(load "segmentintegral.scm")
(load "solvers1d.scm")
(load "statistics.scm")
(load "termstructures.scm")

(let ((suite (make-suite)))
  (suite-add-test suite Date-test
                  "Testing dates")
  (suite-add-test suite Day-counter-test
                  "Testing act/act day counters")
  (suite-add-test suite Distribution-test
                  "Testing distributions")
  (suite-add-test suite Instrument-test
                  "Testing observability of stocks")
  (suite-add-test suite Market-element-test-1
                  "Testing observability of market elements")
  (suite-add-test suite Market-element-test-2
                  "Testing derived market elements")
  (suite-add-test suite Market-element-test-3
                  "Testing composite market elements")
  (suite-add-test suite Market-element-handle-test
                  "Testing observability of market element handles")
  (suite-add-test suite Operator-test
                  "Testing differential operators")
  (suite-add-test suite Risk-statistics-test
                  "Testing risk statistics")
  (suite-add-test suite Segment-integral-test
                  "Testing segment integral")
  (suite-add-test suite Solver-1D-test
                  "Testing 1D solvers")
  (suite-add-test suite Statistics-test
                  "Testing statistics")
  (suite-add-test suite Term-structure-test-1
                  "Testing consistency of implied term structure")
  (suite-add-test suite Term-structure-test-2
                  "Testing observability of implied term structure")
  (suite-add-test suite Term-structure-test-3
                  "Testing consistency of forward-spreaded term structure")
  (suite-add-test suite Term-structure-test-4
                  "Testing observability of forward-spreaded term structure")
  (suite-add-test suite Term-structure-test-5
                  "Testing consistency of zero-spreaded term structure")
  (suite-add-test suite Term-structure-test-6
                  "Testing observability of zero-spreaded term structure")
  (suite-run suite))


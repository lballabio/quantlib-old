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
(load "riskstatistics.scm")
(load "solvers1d.scm")

(let ((suite (make-suite)))
  (suite-add-test suite Date-test
                  "Testing dates")
  (suite-add-test suite DayCounter-test
                  "Testing act/act day counters")
  (suite-add-test suite Distribution-test
                  "Testing distributions")
  (suite-add-test suite Instrument-test
                  "Testing observability of stocks")
  (suite-add-test suite Market-element-test
                  "Testing observability of market elements")
  (suite-add-test suite Market-element-handle-test
                  "Testing observability of market element handles")
  (suite-add-test suite Risk-statistics-test
                  "Testing risk statistics")
  (suite-add-test suite Solver-1D-test
                  "Testing 1-D solvers")
  (suite-run suite))


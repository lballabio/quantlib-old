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

(require (lib "quantlib.ss" "quantlib"))
(load "unittest.scm")
(load "utilities.scm")

(let ((test-dir (build-path ".." ".." "test")))
  (if (directory-exists? test-dir)
      (current-directory test-dir)))

(load "calendars.scm")
(load "capfloor.scm")
(load "covariance.scm")
(load "date.scm")
(load "daycounters.scm")
(load "distributions.scm")
(load "europeanoption.scm")
(load "instruments.scm")
(load "marketelements.scm")
(load "operators.scm")
(load "piecewiseflatforward.scm")
(load "riskstatistics.scm")
(load "segmentintegral.scm")
(load "simpleswap.scm")
(load "solvers1d.scm")
(load "statistics.scm")
(load "swaption.scm")
(load "termstructures.scm")
; to be removed
(load "old_pricers.scm")

(let ((suite (make-suite)))
  (suite-add-test suite Calendar-test
                  "Testing joint calendars")
  (suite-add-test suite CapFloor-strike-dependency-test
                  "Testing cap/floor dependency on strike")
  (suite-add-test suite CapFloor-consistency-test
                  "Testing consistency between cap, floor and collar")
  (suite-add-test suite CapFloor-parity-test
                  "Testing put/call parity for cap and floor")
  (suite-add-test suite CapFloor-cached-value-test
                  "Testing cap/floor value against cached values")
  (suite-add-test suite Covariance-test
                  "Testing covariance calculation")
  (suite-add-test suite Date-test
                  "Testing dates")
  (suite-add-test suite Day-counter-test
                  "Testing act/act day counters")
  (suite-add-test suite Distribution-test
                  "Testing distributions")
  (suite-add-test suite European-option-Greek-test
                  "Testing European option greeks")
  (suite-add-test suite European-option-implied-vol-test
                  "Testing European option implied volatility")
  (suite-add-test suite European-option-binomial-engine-test
                  "Testing binomial European engines against analytic results")
  (suite-add-test suite Instrument-test
                  "Testing observability of stocks")
  (suite-add-test suite Market-element-observability-test
                  "Testing observability of market elements")
  (suite-add-test suite Derived-market-element-test
                  "Testing derived market elements")
  (suite-add-test suite Composite-market-element-test
                  "Testing composite market elements")
  (suite-add-test suite Market-element-handle-observability-test
                  "Testing observability of market element handles")
  (suite-add-test suite Operator-test
                  "Testing differential operators")
  (suite-add-test suite PiecewiseFlatForward-test
                  "Testing piecewise flat forward curve")
  (suite-add-test suite Risk-statistics-test
                  "Testing risk statistics")
  (suite-add-test suite Segment-integral-test
                  "Testing segment integral")
  (suite-add-test suite SimpleSwap-fair-rate-test
                  "Testing simple swap calculation of fair fixed rate")
  (suite-add-test suite SimpleSwap-fair-spread-test
                  "Testing simple swap calculation of fair floating spread")
  (suite-add-test suite SimpleSwap-rate-dependency-test
                  "Testing simple swap dependency on fixed rate")
  (suite-add-test suite SimpleSwap-spread-dependency-test
                  "Testing simple swap dependency on floating spread")
  (suite-add-test suite SimpleSwap-cached-value-test
                  "Testing simple swap calculation against cached value")
  (suite-add-test suite Solver-1D-test
                  "Testing 1D solvers")
  (suite-add-test suite Statistics-test
                  "Testing statistics")
  (suite-add-test suite Swaption-strike-dependency-test
                  "Testing swaption dependency on strike")
  (suite-add-test suite Swaption-spread-dependency-test
                  "Testing swaption dependency on spread")
  (suite-add-test suite Swaption-spread-treatment-test
                  "Testing swaption treatment of spread")
  (suite-add-test suite Swaption-cached-value-test
                  "Testing swaption value against cached value")
  (suite-add-test suite Implied-term-structure-consistency-test
                  "Testing consistency of implied term structure")
  (suite-add-test suite Implied-term-structure-observability-test
                  "Testing observability of implied term structure")
  (suite-add-test suite Forward-spreaded-term-structure-consistency-test
                  "Testing consistency of forward-spreaded term structure")
  (suite-add-test suite Forward-spreaded-term-structure-observability-test
                  "Testing observability of forward-spreaded term structure")
  (suite-add-test suite Zero-spreaded-term-structure-consistency-test
                  "Testing consistency of zero-spreaded term structure")
  (suite-add-test suite Zero-spreaded-term-structure-observability-test
                  "Testing observability of zero-spreaded term structure")
  ; to be removed
  (suite-add-test suite OldBarrierPricer-test
                  "Testing old-style barrier option pricer")
  (suite-add-test suite OldBinaryPricer-test
                  "Testing old-style binary option pricer")
  (suite-add-test suite OldCliquetPricer-test
                  "Testing old-style cliquet option pricer")
  (suite-add-test suite OldDividendEuropeanPricer-test
                  "Testing old-style European option pricer with dividends")
  (suite-add-test suite OldFdEuropeanPricer-test
                  "Testing old-style finite-difference European option pricer")
  (suite-add-test suite OldAmericanPricers-test
                  "Testing old-style American-type pricers")
  (suite-add-test suite OldMcSingleFactorPricers-test
                  "Testing old-style Monte Carlo single-factor pricers")
  (suite-add-test suite OldMcMultiFactorPricers-test
                  "Testing old-style Monte Carlo multi-factor pricers")
  (suite-run suite))


; Copyright (C) 2002, 2003 RiskMap srl
;
; This file is part of QuantLib, a free-software/open-source library
; for financial quantitative analysts and developers - http://quantlib.org/
;
; QuantLib is free software: you can redistribute it and/or modify it under the
; terms of the QuantLib license.  You should have received a copy of the
; license along with this program; if not, please email ferdinando@ametrano.net
; The license is also available online at http://quantlib.org/html/license.html
;
; This program is distributed in the hope that it will be useful, but WITHOUT
; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
; FOR A PARTICULAR PURPOSE.  See the license for more details.

(load "unittest.scm")
(load "common.scm")

(define (CapFloor-test tag)
  ; pseudo-globals
  (let ((nominals '(100.0))
        (rolling-convention "modifiedFollowing")
        (frequency 2)
        (settlement-days 2)
        (fixing-days 2))
    (deleting-let* ((real-today (Date-todays-date) delete-Date)
                    (term-structure (new-TermStructureHandle)
                                    delete-TermStructureHandle)
                    (index (new-Xibor "Euribor" (/ 12 frequency) "months"
                                      term-structure)
                           delete-Index)
                    (calendar (Xibor-calendar index) delete-Calendar)
                    (today (Calendar-roll calendar real-today) delete-Date)
                    (settlement (Calendar-advance calendar today
                                                  settlement-days "days"
                                                  "following")
                                delete-Date))
      ; helper functions
      (letrec
          ((make-leg 
            (lambda (start-date length)
              (deleting-let ((end-date (Calendar-advance calendar start-date
                                                         length "years"
                                                         rolling-convention)
                                       delete-Date))
                (FloatingRateCouponVector nominals start-date end-date 
                                          frequency calendar rolling-convention
                                          index fixing-days))))
           (delete-leg
            (lambda (leg)
              (map delete-CashFlow (vector->list leg))))
           (make-capfloor
            (lambda (kind leg strike volatility)
              (deleting-let ((engine (make-engine volatility) 
                                     delete-PricingEngine))
                (kind leg (list strike) term-structure engine))))
           (make-engine
            (lambda (volatility)
              (deleting-let* ((vol-element (new-SimpleMarketElement volatility)
                                           delete-MarketElement)
                              (vol-handle (new-MarketElementHandle vol-element)
                                          delete-MarketElementHandle)
                              (model (new-BlackModel vol-handle term-structure)
                                     delete-BlackModel))
                (new-BlackCapFloorEngine model)))))
        ; setup
        (deleting-let* ((day-counter (new-DayCounter "act/360") 
                                     delete-DayCounter)
                        (flat-curve (new-FlatForward today settlement 0.05 
                                                     day-counter)
                                    delete-TermStructure))
          (TermStructureHandle-link-to! term-structure flat-curve)
          (cond ((equal? tag 'strike-dep)
                 ; check dependency on strike
                 (deleting-let ((start-date 
                                 (TermStructureHandle-reference-date
                                  term-structure)
                                 delete-Date))
                   (for-each-combination ((length '(1 2 3 5 7 10 15 20))
                                          (vol '(0.01 0.05 0.1 0.15 0.2))
                                          (kind (list new-Cap new-Floor)))
                     (let ((values (map
                                    (lambda (s)
                                      (deleting-let* 
                                          ((leg (make-leg start-date length)
                                                delete-leg)
                                           (instrument (make-capfloor kind leg
                                                                      s vol)
                                                       delete-Instrument))
                                        (Instrument-NPV instrument)))
                                    '(0.03 0.04 0.05 0.06 0.07)))) ; strikes
                       (if (equal? kind new-Cap)
                           ; NPV must decrease with strike
                           (check (sorted? values >=)
                                  "NPV is increasing with the strike "
                                  "in a cap" cr
                                  "length: " length cr
                                  "vol: " vol)
                           ; else NPV must increase with strike
                           (check (sorted? values <=)
                                  "NPV is decreasing with the strike "
                                  "in a floor" cr
                                  "length: " length cr
                                  "vol: " vol))))))
                ((equal? tag 'collar)
                 ; check consistency between cap, floor and collar
                 (deleting-let ((start-date 
                                 (TermStructureHandle-reference-date
                                  term-structure)
                                 delete-Date))
                   (for-each-combination 
                       ((length '(1 2 3 5 7 10 15 20))
                        (cap-rate '(0.03 0.04 0.05 0.06 0.07))
                        (floor-rate '(0.03 0.04 0.05 0.06 0.07))
                        (vol '(0.01 0.05 0.1 0.15 0.2)))
                     (deleting-let* ((leg (make-leg start-date length)
                                          delete-leg)
                                     (cap (make-capfloor new-Cap leg 
                                                         cap-rate vol)
                                          delete-Instrument)
                                     (floor (make-capfloor new-Floor leg
                                                           floor-rate vol)
                                            delete-Instrument)
                                     (engine (make-engine vol)
                                             delete-PricingEngine)
                                     (collar (new-Collar leg (list cap-rate)
                                                         (list floor-rate)
                                                         term-structure engine)
                                             delete-Instrument))
                       (check-equal 
                        (- (Instrument-NPV cap) (Instrument-NPV floor))
                        (Instrument-NPV collar)
                        1.0e-10
                        "inconsistency between cap, floor and collar:" cr
                        "length: " length cr
                        "volatility: " vol cr
                        "cap value: " (Instrument-NPV cap) cr
                        "floor value: " (Instrument-NPV floor) cr
                        "collar value: " (Instrument-NPV collar))))))
                ((equal? tag 'parity)
                 ; check put/call parity
                 (deleting-let ((start-date 
                                 (TermStructureHandle-reference-date
                                  term-structure)
                                 delete-Date))
                   (for-each-combination 
                       ((length '(1 2 3 5 7 10 15 20))
                        (strike '(0.03 0.04 0.05 0.06 0.07))
                        (vol '(0.01 0.05 0.1 0.15 0.2)))
                     (deleting-let* 
                         ((leg (make-leg start-date length) delete-leg)
                          (cap (make-capfloor new-Cap leg strike vol)
                               delete-Instrument)
                          (floor (make-capfloor new-Floor leg strike vol)
                                 delete-Instrument)
                          (day-counter (Xibor-day-counter index)
                                       delete-DayCounter)
                          (swap (new-SimpleSwap #t start-date length "years" 
                                                calendar rolling-convention
                                                (car nominals) frequency
                                                strike 
                                                (Xibor-is-adjusted? index)
                                                day-counter frequency index
                                                fixing-days 0.0 term-structure)
                                delete-Instrument))
                       (check-equal
                        (- (Instrument-NPV cap) (Instrument-NPV floor))
                        (Instrument-NPV swap)
                        1.0e-10
                        "put/call parity violated:" cr
                        "length: " length cr
                        "volatility: " vol cr
                        "strike: " (* strike 100) "%" cr
                        "cap value: " (Instrument-NPV cap) cr
                        "floor value: " (Instrument-NPV floor) cr
                        "swap value: " (Instrument-NPV swap))))))
                ((equal? tag 'cached)
                 ; check calculations against cached values
                 (deleting-let* ((cached-today (new-Date 14 3 2002)
                                                    delete-Date)
                                 (cached-settlement (new-Date 18 3 2002)
                                                    delete-Date)
                                 (day-counter (new-DayCounter "act/360")
                                              delete-DayCounter)
                                 (flat-curve (new-FlatForward cached-today
                                                              cached-settlement
                                                              0.05 
                                                              day-counter)
                                             delete-TermStructure))
                   (TermStructureHandle-link-to! term-structure flat-curve)
                   (deleting-let* ((start-date 
                                    (TermStructureHandle-reference-date
                                     term-structure)
                                    delete-Date)
                                   (leg (make-leg start-date 20) delete-leg)
                                   (cap (make-capfloor new-Cap leg 0.07 0.2)
                                        delete-Instrument)
                                   (floor (make-capfloor new-Floor leg 
                                                         0.03 0.2)
                                          delete-Instrument))
                     (let ((cached-cap-NPV 6.960233718984)
                           (cached-floor-NPV 2.701296290808))
                       (check-expected (Instrument-NPV cap) cached-cap-NPV
                                       1.0e-11
                                       "failed to reproduce "
                                       "cached cap value:")
                       (check-expected (Instrument-NPV floor) cached-floor-NPV
                                       1.0e-11
                                       "failed to reproduce "
                                       "cached floor value:")))))))))))

(define (CapFloor-strike-dependency-test)
  (CapFloor-test 'strike-dep))
(define (CapFloor-consistency-test)
  (CapFloor-test 'collar))
(define (CapFloor-parity-test)
  (CapFloor-test 'parity))
(define (CapFloor-cached-value-test)
  (CapFloor-test 'cached))

(define CapFloor-suite
  (make-test-suite 
   "Cap/floor tests"
   (make-test-case/msg "Testing cap/floor dependency on strike"
                       (CapFloor-strike-dependency-test))
   (make-test-case/msg "Testing consistency between cap, floor and collar"
                       (CapFloor-consistency-test))
   (make-test-case/msg "Testing put/call parity for cap and floor"
                       (CapFloor-parity-test))
   (make-test-case/msg "Testing cap/floor value against cached values"
                       (CapFloor-cached-value-test))))


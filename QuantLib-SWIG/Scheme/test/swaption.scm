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
;
; $Id$

(load "common.scm")

(define (Swaption-test tag)
  ; helper functions
  (define (make-swap start-date length fixed-rate
                     floating-spread pay-fixed?
                     term-structure)
    (let ((nominal 100.0)
          (rolling-convention "modifiedFollowing")
          (fixed-frequency 1)
          (floating-frequency 2)
          (fixed-adj? #f)
          (fixing-days 2))
      (deleting-let* ((fixed-day-count (new-DayCounter "30/360")
                                       delete-DayCounter)
                      (index (new-Xibor "Euribor" (/ 12 floating-frequency)
                                        "Months" term-structure)
                             delete-Index)
                      (calendar (Xibor-calendar index) delete-Calendar))
        (new-SimpleSwap pay-fixed? start-date length "years"
                        calendar rolling-convention nominal
                        fixed-frequency fixed-rate fixed-adj? fixed-day-count
                        floating-frequency index fixing-days floating-spread
                        term-structure))))
  (define (make-swaption swap exercise-date volatility
                         term-structure)
    (deleting-let* ((exercise (new-EuropeanExercise exercise-date)
                              delete-Exercise)
                    (vol-element (new-SimpleMarketElement volatility)
                                 delete-MarketElement)
                    (vol-handle (new-MarketElementHandle vol-element)
                                delete-MarketElementHandle)
                    (model (new-BlackModel vol-handle term-structure)
                           delete-BlackModel)
                    (engine (new-BlackSwaptionEngine model)
                            delete-PricingEngine))
      (new-Swaption swap exercise term-structure engine)))
  (define (make-flat-curve today settlement)
    (deleting-let ((day-counter (new-DayCounter "act/365") delete-DayCounter))
      (new-FlatForward today settlement 0.05 day-counter)))
  ; setup
  (deleting-let* ((today (Date-todays-date) delete-Date)
                  (calendar (new-Calendar "TARGET") delete-Calendar)
                  (settlement (Calendar-advance calendar today 2 "days")
                              delete-Date)
                  (flat-curve (make-flat-curve today settlement)
                              delete-TermStructure)
                  (term-structure (new-TermStructureHandle flat-curve)
                                  delete-TermStructureHandle))
    (cond ((equal? tag 'strike-dep)
           ; check dependency on strike
           (for-each-combination ((exercise '(1 2 3 5 7 10))
                                  (length '(1 2 3 5 7 10 15 20))
                                  (pay-fixed? '(#f #t)))
             (deleting-let* ((temp-date (Date-plus-years today exercise) 
                                        delete-Date)
                             (exercise-date (Calendar-roll calendar temp-date)
                                            delete-Date)
                             (start-date (Calendar-advance calendar 
                                                           exercise-date
                                                           2 "days")
                                         delete-Date))
               (let ((values (map 
                              (lambda (s)
                                (deleting-let* ((swap (make-swap
                                                       start-date length s
                                                       0.0 pay-fixed?
                                                       term-structure)
                                                      delete-Instrument)
                                                (swaption (make-swaption 
                                                           swap exercise-date
                                                           0.20 term-structure)
                                                          delete-Instrument))
                                  (Instrument-NPV swaption)))
                              '(0.03 0.04 0.05 0.06 0.07)))) ; strikes
                 (if pay-fixed?
                     ; NPV must decrease with strike
                     (assert (sorted? values >)
                             "NPV is increasing with the strike "
                             "in a payer swaption" cr
                             "exercise: " exercise cr
                             "length: " length)
                     ; else NPV must increase with strike
                     (assert (sorted? values <)
                             "NPV is decreasing with the strike "
                             "in a receiver swaption" cr
                             "exercise: " exercise cr
                             "length: " length))))))
          ((equal? tag 'spread-dep)
           ; check dependency on spread
           (for-each-combination ((exercise '(1 2 3 5 7 10))
                                  (length '(1 2 3 5 7 10 15 20))
                                  (pay-fixed? '(#f #t)))
             (deleting-let* ((temp-date (Date-plus-years today exercise) 
                                        delete-Date)
                             (exercise-date (Calendar-roll calendar temp-date)
                                            delete-Date)
                             (start-date (Calendar-advance calendar 
                                                           exercise-date
                                                           2 "days")
                                         delete-Date))
               (let ((values (map 
                              (lambda (s)
                                (deleting-let* ((swap (make-swap
                                                       start-date length 0.06
                                                       s pay-fixed?
                                                       term-structure)
                                                      delete-Instrument)
                                                (swaption (make-swaption
                                                           swap exercise-date
                                                           0.20 term-structure)
                                                          delete-Instrument))
                                  (Instrument-NPV swaption)))
                              '(-0.002 -0.001 0.0 0.001 0.002)))) ; spreads
                 (if pay-fixed?
                     ; NPV must increase with spread
                     (assert (sorted? values <)
                             "NPV is decreasing with the spread "
                             "in a payer swaption" cr
                             "exercise: " exercise cr
                             "length: " length)
                     ; else NPV must decrease with spread
                     (assert (sorted? values >)
                             "NPV is increasing with the spread "
                             "in a receiver swaption" cr
                             "exercise: " exercise cr
                             "length: " length))))))
          ((equal? tag 'spread)
           ; check treatment of spread
           (for-each-combination ((exercise '(1 2 3 5 7 10))
                                  (length '(1 2 3 5 7 10 15 20))
                                  (pay-fixed? '(#f #t))
                                  (spread '(-0.002 -0.001 0.0 0.001 0.002)))
             (deleting-let* ((temp-date (Date-plus-years today exercise) 
                                        delete-Date)
                             (exercise-date (Calendar-roll calendar temp-date)
                                            delete-Date)
                             (start-date (Calendar-advance calendar 
                                                           exercise-date
                                                           2 "days")
                                         delete-Date)
                             (swap (make-swap start-date length 0.06
                                              spread pay-fixed? 
                                              term-structure)
                                   delete-Instrument)
                             (swaption (make-swaption swap exercise-date
                                                      0.20 term-structure)
                                       delete-Instrument))
               (let ((correction (* spread
                                    (/ (SimpleSwap-floating-leg-BPS swap)
                                       (SimpleSwap-fixed-leg-BPS swap)))))
                 (deleting-let* ((eq-swap (make-swap start-date length
                                                     (+ 0.06 correction)
                                                     0.0 pay-fixed?
                                                     term-structure)
                                          delete-Instrument)
                                 (eq-swaption (make-swaption eq-swap 
                                                             exercise-date
                                                             0.20 
                                                             term-structure)
                                              delete-Instrument))
                   (check-expected (Instrument-NPV eq-swaption)
                                   (Instrument-NPV swaption)
                                   1.0e-10
                                   "Wrong spread treatment" cr
                                   "exercise: " exercise cr
                                   "length: " length cr
                                   "spread: " spread))))))
          ((equal? tag 'cached)
           ; check cached value recalculation
           (deleting-let* ((today (new-Date 13 3 2002) delete-Date)
                           (settlement (new-Date 15 3 2002) delete-Date)
                           (calendar (new-Calendar "TARGET") delete-Calendar)
                           (temp-date (Date-plus-years settlement 5) 
                                      delete-Date)
                           (exercise-date (Calendar-roll calendar temp-date)
                                          delete-Date)
                           (start-date (Calendar-advance calendar exercise-date
                                                         2 "days")
                                       delete-Date)
                           (flat-curve (make-flat-curve today settlement)
                                       delete-TermStructure)
                           (term-structure (new-TermStructureHandle flat-curve)
                                           delete-TermStructureHandle)
                           (swap (make-swap start-date 10 0.06
                                            0.0 #t term-structure)
                                 delete-Instrument)
                           (swaption (make-swaption swap exercise-date
                                                    0.20 term-structure)
                                     delete-Instrument))
             (let ((cached-NPV 3.645305998559))
               (check-expected (Instrument-NPV swaption) cached-NPV 1.0e-11
                               "failed to reproduce cached swaption value"
                               )))))))

(define (Swaption-strike-dependency-test)
  (Swaption-test 'strike-dep))
(define (Swaption-spread-dependency-test)
  (Swaption-test 'spread-dep))
(define (Swaption-spread-treatment-test)
  (Swaption-test 'spread))
(define (Swaption-cached-value-test)
  (Swaption-test 'cached))

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

(load "common.scm")

(define (SimpleSwap-test tag)
  ; helper functions
  (define (make-swap today length fixed-rate floating-spread)
    (let ((pay-fixed #t)
          (settlement-days 2)
          (fixing-days 2)
          (nominal 100)
          (rolling-convention "modifiedFollowing")
          (fixed-frequency 1)
          (floating-frequency 2)
          (fixed-adj #f))
      (deleting-let* ((fixed-day-count (new-DayCounter "30/360")
                                       delete-DayCounter)
                      (euribor-handle (new-TermStructureHandle)
                                      delete-TermStructureHandle)
                      (index (new-Xibor "Euribor" (/ 12 floating-frequency)
                                        "months" euribor-handle)
                             delete-Index)
                      (calendar (Xibor-calendar index) delete-Calendar)
                      (settlement (Calendar-advance calendar today 
                                                    settlement-days "days" 
                                                    "following")
                                  delete-Date)
                      (curve-day-count (new-DayCounter "act/365")
                                       delete-DayCounter)
                      (term-structure (new-FlatForward today settlement 0.05
                                                       curve-day-count)
                                      delete-TermStructure))
        (TermStructureHandle-link-to! euribor-handle term-structure)
        (new-SimpleSwap pay-fixed settlement length "years" calendar
                        rolling-convention nominal fixed-frequency fixed-rate
                        fixed-adj fixed-day-count floating-frequency index
                        fixing-days floating-spread euribor-handle))))
  ; setup
  (deleting-let ((today (Date-todays-date) delete-Date))
    (cond ((equal? tag 'fair-rate)
           ; check fair rate calculation
           (let ((fixed-rate 0.0))
             (for-each-combination ((length '(1 2 5 10 20))
                                    (spread '(-0.001 -0.01 0 0.01 0.001)))
               (deleting-let ((swap (make-swap today length fixed-rate spread)
                                    delete-Instrument))
                 (let ((fair-rate (SimpleSwap-fair-rate swap)))
                   (deleting-let ((swap (make-swap today length 
                                                   fair-rate spread)
                                        delete-Instrument))
                     (check-expected (Instrument-NPV swap) 0.0 1.0e-10
                                     "length: " length cr
                                     "spread: " spread cr
                                     "Recalculating NPV with fair rate")))))))
          ((equal? tag 'fair-spread)
           ; check fair spread calculation
           (let ((spread 0.0))
             (for-each-combination ((length '(1 2 5 10 20))
                                    (rate '(0.04 0.05 0.06 0.07)))
               (deleting-let ((swap (make-swap today length rate spread)
                                    delete-Instrument))
                 (let ((fair-spread (SimpleSwap-fair-spread swap)))
                   (deleting-let ((swap (make-swap today length 
                                                   rate fair-spread)
                                        delete-Instrument))
                     (check-expected (Instrument-NPV swap) 0.0 1.0e-10
                                     "length: " length cr
                                     "rate: " rate cr
                                     "Recalculating NPV with fair spread"
                                     )))))))
          ((equal? tag 'rate-dep)
           ; check dependency on fixed rate
           (for-each-combination ((length '(1 2 5 10 20))
                                  (spread '(-0.001 -0.01 0 0.01 0.001)))
             (let ((values (map (lambda (r)
                                  (deleting-let ((swap (make-swap 
                                                        today length r spread)
                                                       delete-Instrument))
                                    (Instrument-NPV swap)))
                                '(0.03 0.04 0.05 0.06 0.07)))) ; rates
               ; We're paying fixed - NPV must decrease with rate
               (assert (sorted? values >)
                       "NPV is increasing with the fixed rate "
                       "in a simple swap paying fixed" cr
                       "length: " length cr
                       "spread: " spread))))
          ((equal? tag 'spread-dep)
           ; check dependency on floating spread
           (for-each-combination ((length '(1 2 5 10 20))
                                  (rate '(0.04 0.05 0.06 0.07)))
             (let ((values (map (lambda (s)
                                  (deleting-let ((swap (make-swap today length 
                                                                  rate s)
                                                       delete-Instrument))
                                    (Instrument-NPV swap)))
                                ; spreads
                                '(-0.01 -0.002 -0.001 0 0.001 0.002 0.01))))
               ; We're paying fixed - NPV must increase with spread
               (assert (sorted? values <)
                       "NPV is decreasing with the spread "
                       "in a simple swap paying fixed" cr
                       "length: " length cr
                       "rate: " rate))))
          ((equal? tag 'cached)
           ; check cached value recalculation
           (deleting-let* ((today (new-Date 17 6 2002) delete-Date)
                           (swap (make-swap today 10 0.06 0.001) 
                                 delete-Instrument))
             (let ((cached-NPV -5.883663676727))
               (check-expected (Instrument-NPV swap) cached-NPV 1.0e-11
                               "failed to reproduce cached "
                               "simple swap value")))))))

(define (SimpleSwap-fair-rate-test)
  (SimpleSwap-test 'fair-rate))
(define (SimpleSwap-fair-spread-test)
  (SimpleSwap-test 'fair-spread))
(define (SimpleSwap-rate-dependency-test)
  (SimpleSwap-test 'rate-dep))
(define (SimpleSwap-spread-dependency-test)
  (SimpleSwap-test 'spread-dep))
(define (SimpleSwap-cached-value-test)
  (SimpleSwap-test 'cached))

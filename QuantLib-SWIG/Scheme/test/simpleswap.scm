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

(define-macro with-todays-date
  (lambda (date . body)
    `(deleting-let ((today ,date delete-Date))
       ,@body)))

(define-macro with-simple-swap-test-parameters
  (lambda body 
    `(let ((pay-fixed #t)
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
                                   delete-Date))
         (deleting-let* ((curve-day-count (new-DayCounter "act/365")
                                          delete-DayCounter)
                         (term-structure (new-FlatForward today settlement 0.05
                                                          curve-day-count)
                                         delete-TermStructure))
           (TermStructureHandle-link-to! euribor-handle term-structure))
         ,@body))))

(define-macro with-test-swap
  (lambda (length fixed-rate floating-spread . body)
    `(with-simple-swap-test-parameters
       (deleting-let ((swap (new-SimpleSwap pay-fixed settlement 
                                            ,length "years" calendar
                                            rolling-convention nominal 
                                            fixed-frequency ,fixed-rate
                                            fixed-adj fixed-day-count 
                                            floating-frequency index
                                            fixing-days ,floating-spread 
                                            euribor-handle)
                            delete-Instrument))
         ,@body))))


(define (SimpleSwap-fair-rate-test)
  (with-todays-date (Date-todays-date)
    (let ((fixed-rate 0.0))
      (for-each-combination ((length '(1 2 5 10 20))
                             (spread '(-0.001 -0.01 0 0.01 0.001)))
        (with-test-swap length fixed-rate spread
          (let ((fair-rate (SimpleSwap-fair-rate swap)))
            (with-test-swap length fair-rate spread
              (check-expected (Instrument-NPV swap) 0.0 1.0e-10
                              "Recalculating NPV with fair rate"))))))))
(define (SimpleSwap-fair-spread-test)
  (with-todays-date (Date-todays-date)
    (let ((spread 0.0))
      (for-each-combination ((length '(1 2 5 10 20))
                             (rate '(0.04 0.05 0.06 0.07)))
        (with-test-swap length rate spread
          (let ((fair-spread (SimpleSwap-fair-spread swap)))
            (with-test-swap length rate fair-spread
              (check-expected (Instrument-NPV swap) 0.0 1.0e-10
                              "Recalculating NPV with fair spread"))))))))
(define (SimpleSwap-rate-dependency-test)
  (with-todays-date (Date-todays-date)
    (for-each-combination ((length '(1 2 5 10 20))
                           (spread '(-0.001 -0.01 0 0.01 0.001)))
      (let ((values (map (lambda (r)
                           (with-test-swap length r spread
                             (Instrument-NPV swap)))
                         '(0.03 0.04 0.05 0.06 0.07)))) ; rates
        ; We're paying fixed - NPV must decrease with rate
        (assert (sorted? values >)
                "NPV is increasing with the fixed rate "
                "in a simple swap paying fixed")))))
(define (SimpleSwap-spread-dependency-test)
  (with-todays-date (Date-todays-date)
    (for-each-combination ((length '(1 2 5 10 20))
                           (rate '(0.04 0.05 0.06 0.07)))
      (let ((values (map (lambda (s)
                           (with-test-swap length rate s
                             (Instrument-NPV swap)))
                         '(-0.01 -0.002 -0.001 0 0.001 0.002 0.01)))) ; spreads
        ; We're paying fixed - NPV must increase with spread
        (assert (sorted? values <)
                "NPV is decreasing with the spread "
                "in a simple swap paying fixed")))))
(define (SimpleSwap-cached-value-test)
  (with-todays-date (new-Date 17 6 2002)
    (with-test-swap 10 0.06 0.001
      (let ((cached-NPV -5.883663676727))
        (check-expected (Instrument-NPV swap) cached-NPV 1.0e-11
                        "failed to reproduce cached simple swap value")))))



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

(define-macro with-test-structure
  (lambda body 
    `(let ((settlement-days 2)
           (deposit-data '((1  "month" 4.581)
                           (2 "months" 4.573)
                           (3 "months" 4.557)
                           (6 "months" 4.496)
                           (9 "months" 4.490)))
           (swap-data '((1 4.54)
                        (5 4.99)
                        (10 5.47)
                        (20 5.89)
                        (30 5.96))))
       (deleting-let* ((today (Date-todays-date) delete-Date)
                       (calendar (new-Calendar "TARGET") delete-Calendar)
                       (settlement (Calendar-advance calendar today
                                                     settlement-days "days")
                                   delete-Date)
                       (day-counter (new-DayCounter "act/360")
                                    delete-DayCounter)
                       (day-counter-2 (new-DayCounter "30/360")
                                      delete-DayCounter)
                       (deposits (map
                                  (lambda (datum)
                                    (let-at-once ((n units rate) datum)
                                      (new-DepositRateHelper
                                       (new-MarketElementHandle
                                        (new-SimpleMarketElement (/ rate 100)))
                                       0 n units calendar 
                                       "mf" day-counter)))
                                  deposit-data)
                                 (lambda (l)
                                   (for-each delete-RateHelper l)))
                       (swaps (map 
                               (lambda (datum)
                                 (let-at-once ((years rate) datum)
                                   (new-SwapRateHelper
                                    (new-MarketElementHandle
                                     (new-SimpleMarketElement (/ rate 100)))
                                    0 years calendar
                                    "mf" 1 #f day-counter-2 2)))
                               swap-data)
                              (lambda (l)
                                (for-each delete-RateHelper l)))
                       (term-structure (new-PiecewiseFlatForward
                                        settlement 
                                        (append deposits swaps)
                                        day-counter)
                                       delete-TermStructure)
                       (handle (new-TermStructureHandle term-structure)
                               delete-TermStructureHandle))
         ,@body))))

(define (Implied-term-structure-consistency-test)
  "Testing consistency of implied term structure"
  (with-test-structure
    (deleting-let* ((new-settlement (Date-plus-years 
                                     (TermStructure-settlement-date 
                                      term-structure)
                                     3)
                                    delete-Date)
                    (test-date (Date-plus-years new-settlement 5) delete-Date)
                    (implied (new-ImpliedTermStructure handle new-settlement)
                             delete-TermStructure))
      (let ((tolerance 1.0e-10)
            (base-discount (TermStructure-discount term-structure
                                                   new-settlement))
            (discount (TermStructure-discount term-structure test-date))
            (implied-discount (TermStructure-discount implied test-date)))
        (check-expected (* base-discount implied-discount) discount
                        tolerance
                        "discount from implied curve")))))

(define (Implied-term-structure-observability-test)
  "Testing observability of implied term structure"
  (with-test-structure
   (let ((flag #f))
     (deleting-let* ((new-settlement (Date-plus-years 
                                      (TermStructure-settlement-date 
                                       term-structure)
                                      3)
                                     delete-Date)
                     (implied (new-ImpliedTermStructure handle new-settlement)
                              delete-TermStructure)
                     (obs (new-Observer (lambda () (set! flag #t)))
                          delete-Observer))
       (deleting-let ((temp (TermStructure->Observable implied)
                            delete-Observable))
         (Observer-register-with obs temp))
       (deleting-let ((new-term-structure (new-FlatForward settlement
                                                           0.05 day-counter)
                                          delete-TermStructure))
         (TermStructureHandle-link-to! handle new-term-structure))
       (if (not flag)
           (error "Observer was not notified of term structure change"))))))
             
(define (Forward-spreaded-term-structure-consistency-test)
  "Testing consistency of forward-spreaded term structure"
  (with-test-structure
    (deleting-let* ((me (new-SimpleMarketElement 0.01)
                        delete-MarketElement)
                    (h (new-MarketElementHandle me)
                       delete-MarketElementHandle)
                    (test-date (Date-plus-years (TermStructure-settlement-date
                                                 term-structure) 5)
                               delete-Date)
                    (spreaded (new-ForwardSpreadedTermStructure handle h)
                              delete-TermStructure))
      (let ((tolerance 1.0e-10)
            (forward (TermStructure-instantaneous-forward term-structure 
                                                          test-date))
            (spreaded-forward (TermStructure-instantaneous-forward spreaded 
                                                                   test-date)))
        (check-expected (- spreaded-forward (MarketElement-value me)) forward 
                        tolerance
                        "forward from spreaded curve")))))

(define (Forward-spreaded-term-structure-observability-test)
  "Testing observability of forward-spreaded term structure"
  (with-test-structure
   (let ((flag #f)
         (tolerance 1.0e-10))
     (deleting-let* ((me (new-SimpleMarketElement 0.01)
                         delete-MarketElement)
                     (h (new-MarketElementHandle me)
                        delete-MarketElementHandle)
                     (spreaded (new-ForwardSpreadedTermStructure handle h)
                               delete-TermStructure)
                     (obs (new-Observer (lambda () (set! flag #t)))
                          delete-Observer))
       (deleting-let ((temp (TermStructure->Observable spreaded)
                            delete-Observable))
         (Observer-register-with obs temp))
       (deleting-let ((new-term-structure (new-FlatForward settlement 
                                                           0.05 day-counter)
                                          delete-TermStructure))
         (TermStructureHandle-link-to! handle new-term-structure))
       (if (not flag)
           (error "Observer was not notified of term structure change"))
       (set! flag #f)
       (SimpleMarketElement-value-set! me 0.005)
       (if (not flag)
           (error "Observer was not notified of spread change"))))))

(define (Zero-spreaded-term-structure-consistency-test)
  "Testing consistency of zero-spreaded term structure"
  (with-test-structure
    (deleting-let* ((me (new-SimpleMarketElement 0.01)
                        delete-MarketElement)
                    (h (new-MarketElementHandle me)
                       delete-MarketElementHandle)
                    (test-date (Date-plus-years (TermStructure-settlement-date
                                                 term-structure) 5)
                               delete-Date)
                    (spreaded (new-ZeroSpreadedTermStructure handle h)
                              delete-TermStructure))
      (let ((tolerance 1.0e-10)
            (zero (TermStructure-zero-yield term-structure test-date))
            (spreaded-zero (TermStructure-zero-yield spreaded test-date)))
        (check-expected (- spreaded-zero (MarketElement-value me)) zero 
                        tolerance
                        "zero yield from spreaded curve")))))

(define (Zero-spreaded-term-structure-observability-test)
  "Testing observability of zero-spreaded term structure"
  (with-test-structure
   (let ((flag #f)
         (tolerance 1.0e-10))
     (deleting-let* ((me (new-SimpleMarketElement 0.01)
                         delete-MarketElement)
                     (h (new-MarketElementHandle me)
                        delete-MarketElementHandle)
                     (spreaded (new-ZeroSpreadedTermStructure handle h)
                               delete-TermStructure)
                     (obs (new-Observer (lambda () (set! flag #t)))
                          delete-Observer))
       (deleting-let ((temp (TermStructure->Observable spreaded)
                            delete-Observable))
         (Observer-register-with obs temp))
       (deleting-let ((new-term-structure (new-FlatForward settlement 
                                                           0.05 day-counter)
                                          delete-TermStructure))
         (TermStructureHandle-link-to! handle new-term-structure))
       (if (not flag)
           (error "Observer was not notified of term structure change"))
       (set! flag #f)
       (SimpleMarketElement-value-set! me 0.005)
       (if (not flag)
           (error "Observer was not notified of spread change"))))))


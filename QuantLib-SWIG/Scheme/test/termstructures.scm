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

(define (TermStructure-test tag)
  (define (make-test-structure-handle)
    (let ((settlement-days 2)
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
                                      n units calendar "mf" day-counter)))
                                 deposit-data)
                                (lambda (l)
                                  (for-each delete-RateHelper l)))
                      (swaps (map 
                              (lambda (datum)
                                (let-at-once ((years rate) datum)
                                  (new-SwapRateHelper
                                   (new-MarketElementHandle
                                    (new-SimpleMarketElement (/ rate 100)))
                                   years calendar "mf" 1 #f day-counter-2 2)))
                              swap-data)
                             (lambda (l)
                               (for-each delete-RateHelper l)))
                      (term-structure (new-PiecewiseFlatForward
                                       today settlement 
                                       (append deposits swaps)
                                       day-counter)
                                      delete-TermStructure))
        (new-TermStructureHandle term-structure))))
  ; setup
  (deleting-let ((term-structure (make-test-structure-handle)
                                 delete-TermStructureHandle))
    (cond ((equal? tag 'implied)
           ; check implied term structure consistency
           (deleting-let* ((calendar (new-Calendar "TARGET")
                                     delete-Calendar)
                           (new-today 
                            (Date-plus-years 
                             (TermStructureHandle-todays-date 
                              term-structure)
                             3)
                            delete-Date)
                           (new-settlement 
                            (Calendar-advance
                             calendar new-today 2 "days")
                            delete-Date)
                           (test-date (Date-plus-years new-settlement 5) 
                                      delete-Date)
                           (implied (new-ImpliedTermStructure term-structure
                                                              new-today 
                                                              new-settlement)
                                    delete-TermStructure))
             (let ((tolerance 1.0e-10)
                   (base-discount (TermStructureHandle-discount 
                                   term-structure new-settlement))
                   (discount (TermStructureHandle-discount 
                              term-structure test-date))
                   (implied-discount (TermStructure-discount 
                                      implied test-date)))
               (check-expected (* base-discount implied-discount) discount
                               tolerance
                               "discount from implied curve"))))
          ((equal? tag 'implied-obs)
           ; check implied term structure observability
           (let ((flag #f))
             (deleting-let* ((calendar (new-Calendar "TARGET")
                                     delete-Calendar)
                             (today (TermStructureHandle-todays-date 
                                     term-structure)
                                    delete-Date)
                             (settlement (TermStructureHandle-settlement-date 
                                          term-structure)
                                         delete-Date)
                             (new-today (Date-plus-years today 3)
                                        delete-Date)
                             (new-settlement 
                              (Calendar-advance
                               calendar new-today 2 "days")
                              delete-Date)
                             (day-counter (TermStructureHandle-day-counter
                                           term-structure)
                                          delete-DayCounter)
                             (implied (new-ImpliedTermStructure term-structure 
                                                                new-today
                                                                new-settlement)
                                      delete-TermStructure)
                             (obs (new-Observer (lambda () (set! flag #t)))
                                  delete-Observer))
               (deleting-let ((temp (TermStructure->Observable implied)
                                    delete-Observable))
                 (Observer-register-with obs temp))
               (deleting-let ((new-term-structure (new-FlatForward today
                                                                   settlement
                                                                   0.05 
                                                                   day-counter)
                                                  delete-TermStructure))
                 (TermStructureHandle-link-to! term-structure 
                                               new-term-structure))
               (if (not flag)
                   (error 
                    "Observer was not notified of term structure change")))))
          ((equal? tag 'fw-spreaded)
           ; check forward-spreaded term structure consistency
           (deleting-let* ((me (new-SimpleMarketElement 0.01)
                               delete-MarketElement)
                           (h (new-MarketElementHandle me)
                              delete-MarketElementHandle)
                           (test-date (Date-plus-years 
                                       (TermStructureHandle-settlement-date
                                        term-structure) 
                                       5)
                                      delete-Date)
                           (spreaded (new-ForwardSpreadedTermStructure 
                                      term-structure h)
                                     delete-TermStructure))
             (let ((tolerance 1.0e-10)
                   (forward (TermStructureHandle-instantaneous-forward 
                             term-structure test-date))
                   (spreaded-forward (TermStructure-instantaneous-forward 
                                      spreaded test-date)))
               (check-expected (- spreaded-forward (MarketElement-value me)) 
                               forward tolerance
                               "forward from spreaded curve"))))
          ((equal? tag 'fw-spreaded-obs)
           ; check forward-spreaded term structure observability
           (let ((flag #f)
                 (tolerance 1.0e-10))
             (deleting-let* ((today (TermStructureHandle-todays-date 
                                     term-structure)
                                    delete-Date)
                             (settlement (TermStructureHandle-settlement-date 
                                          term-structure)
                                         delete-Date)
                             (day-counter (TermStructureHandle-day-counter
                                           term-structure)
                                          delete-DayCounter)
                             (me (new-SimpleMarketElement 0.01)
                                 delete-MarketElement)
                             (h (new-MarketElementHandle me)
                                delete-MarketElementHandle)
                             (spreaded (new-ForwardSpreadedTermStructure 
                                        term-structure h)
                                       delete-TermStructure)
                             (obs (new-Observer (lambda () (set! flag #t)))
                                  delete-Observer))
               (deleting-let ((temp (TermStructure->Observable spreaded)
                                    delete-Observable))
                 (Observer-register-with obs temp))
               (deleting-let ((new-term-structure (new-FlatForward today
                                                                   settlement 
                                                                   0.05 
                                                                   day-counter)
                                                  delete-TermStructure))
                 (TermStructureHandle-link-to! term-structure 
                                               new-term-structure))
               (if (not flag)
                   (error 
                    "Observer was not notified of term structure change"))
               (set! flag #f)
               (SimpleMarketElement-value-set! me 0.005)
               (if (not flag)
                   (error "Observer was not notified of spread change")))))
          ((equal? tag 'z-spreaded)
           ; check zero-spreaded term structure consistency
           (deleting-let* ((me (new-SimpleMarketElement 0.01)
                               delete-MarketElement)
                           (h (new-MarketElementHandle me)
                              delete-MarketElementHandle)
                           (test-date (Date-plus-years 
                                       (TermStructureHandle-settlement-date
                                        term-structure) 
                                       5)
                                      delete-Date)
                           (spreaded (new-ZeroSpreadedTermStructure 
                                      term-structure h)
                                     delete-TermStructure))
             (let ((tolerance 1.0e-10)
                   (zero (TermStructureHandle-zero-yield term-structure 
                                                         test-date))
                   (spreaded-zero (TermStructure-zero-yield spreaded 
                                                            test-date)))
               (check-expected (- spreaded-zero (MarketElement-value me)) zero 
                               tolerance
                               "zero yield from spreaded curve"))))
          ((equal? tag 'z-spreaded-obs)
           ; check zero-spreaded term structure observability
           (let ((flag #f)
                 (tolerance 1.0e-10))
             (deleting-let* ((today (TermStructureHandle-todays-date 
                                     term-structure)
                                    delete-Date)
                             (settlement (TermStructureHandle-settlement-date 
                                          term-structure)
                                         delete-Date)
                             (day-counter (TermStructureHandle-day-counter
                                           term-structure)
                                          delete-DayCounter)
                             (me (new-SimpleMarketElement 0.01)
                                 delete-MarketElement)
                             (h (new-MarketElementHandle me)
                                delete-MarketElementHandle)
                             (spreaded (new-ZeroSpreadedTermStructure 
                                        term-structure h)
                                       delete-TermStructure)
                             (obs (new-Observer (lambda () (set! flag #t)))
                                  delete-Observer))
               (deleting-let ((temp (TermStructure->Observable spreaded)
                                    delete-Observable))
                 (Observer-register-with obs temp))
               (deleting-let ((new-term-structure (new-FlatForward today
                                                                   settlement 
                                                                   0.05 
                                                                   day-counter)
                                                  delete-TermStructure))
                 (TermStructureHandle-link-to! term-structure 
                                               new-term-structure))
               (if (not flag)
                   (error 
                    "Observer was not notified of term structure change"))
               (set! flag #f)
               (SimpleMarketElement-value-set! me 0.005)
               (if (not flag)
                   (error "Observer was not notified of spread change"))))))))

(define (Implied-term-structure-consistency-test)
  (TermStructure-test 'implied))
(define (Implied-term-structure-observability-test)
  (TermStructure-test 'implied-obs))
(define (Forward-spreaded-term-structure-consistency-test)
  (TermStructure-test 'fw-spreaded))
(define (Forward-spreaded-term-structure-observability-test)
  (TermStructure-test 'fw-spreaded-obs))
(define (Zero-spreaded-term-structure-consistency-test)
  (TermStructure-test 'z-spreaded))
(define (Zero-spreaded-term-structure-observability-test)
  (TermStructure-test 'z-spreaded-obs))

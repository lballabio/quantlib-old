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
    `(deleting-let* ((today (Date-todays-date) delete-Date)
                     (calendar (new-Calendar "TARGET") delete-Calendar)
                     (settlement (Calendar-advance calendar today 2 "days")
                                 delete-Date)
                     (day-counter (new-DayCounter "act/365")
                                  delete-DayCounter)
                     (term-structure (new-FlatForward "EUR" day-counter
                                                      today settlement 0.05)
                                     delete-TermStructure)
                     (handle (new-TermStructureHandle term-structure)
                             delete-TermStructureHandle))
       ,@body)))

(define (Implied-term-structure-consistency-test)
  (with-test-structure
    (deleting-let* ((new-today (Date-plus-years 
                                (TermStructure-todays-date term-structure)
                                3)
                               delete-Date)
                    (new-settlement (Calendar-advance calendar new-today 
                                                      2 "days")
                                    delete-Date)
                    (test-date (Date-plus-years new-settlement 5) delete-Date)
                    (implied (new-ImpliedTermStructure handle new-today 
                                                       new-settlement)
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
  (with-test-structure
   (let ((flag #f))
     (deleting-let* ((new-today (Date-plus-years 
                                 (TermStructure-todays-date term-structure)
                                 3)
                                delete-Date)
                     (new-settlement (Calendar-advance calendar new-today 
                                                       2 "days")
                                     delete-Date)
                     (implied (new-ImpliedTermStructure handle new-today 
                                                        new-settlement)
                              delete-TermStructure)
                     (obs (new-Observer (lambda () (set! flag #t)))
                          delete-Observer))
       (deleting-let ((temp (TermStructure->Observable implied)
                            delete-Observable))
         (Observer-register-with obs temp))
       (deleting-let ((new-term-structure (new-FlatForward "EUR" day-counter
                                                           today settlement 
                                                           0.05)
                                          delete-TermStructure))
         (TermStructureHandle-link-to! handle new-term-structure))
       (if (not flag)
           (error "Observer was not notified of term structure change"))))))
             
(define (Forward-spreaded-term-structure-consistency-test)
  (with-test-structure
    (deleting-let* ((me (new-SimpleMarketElement 0.01)
                        delete-MarketElement)
                    (h (new-MarketElementHandle me)
                       delete-MarketElementHandle)
                    (test-date (Date-plus-years (TermStructure-todays-date
                                                 term-structure) 5)
                               delete-Date)
                    (spreaded (new-ForwardSpreadedTermStructure handle h)
                              delete-TermStructure))
      (let ((tolerance 1.0e-10)
            (forward (TermStructure-forward term-structure test-date))
            (spreaded-forward (TermStructure-forward spreaded test-date)))
        (check-expected (- spreaded-forward (MarketElement-value me)) forward 
                        tolerance
                        "forward from spreaded curve")))))

(define (Forward-spreaded-term-structure-observability-test)
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
       (deleting-let ((new-term-structure (new-FlatForward "EUR" day-counter
                                                           today settlement 
                                                           0.05)
                                          delete-TermStructure))
         (TermStructureHandle-link-to! handle new-term-structure))
       (if (not flag)
           (error "Observer was not notified of term structure change"))
       (set! flag #f)
       (SimpleMarketElement-value-set! me 0.005)
       (if (not flag)
           (error "Observer was not notified of spread change"))))))

(define (Zero-spreaded-term-structure-consistency-test)
  (with-test-structure
    (deleting-let* ((me (new-SimpleMarketElement 0.01)
                        delete-MarketElement)
                    (h (new-MarketElementHandle me)
                       delete-MarketElementHandle)
                    (test-date (Date-plus-years (TermStructure-todays-date
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
       (deleting-let ((new-term-structure (new-FlatForward "EUR" day-counter
                                                           today settlement 
                                                           0.05)
                                          delete-TermStructure))
         (TermStructureHandle-link-to! handle new-term-structure))
       (if (not flag)
           (error "Observer was not notified of term structure change"))
       (set! flag #f)
       (SimpleMarketElement-value-set! me 0.005)
       (if (not flag)
           (error "Observer was not notified of spread change"))))))


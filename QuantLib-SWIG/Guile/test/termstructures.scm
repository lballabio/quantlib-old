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

(use-modules (QuantLib))
(load "common.scm")

(define-macro with-test-structure
  (lambda body 
    `(let* ((today (Date-todays-date))
            (calendar (new-Calendar "TARGET"))
            (settlement (Calendar-advance calendar today 2 "days"))
            (day-counter (new-DayCounter "act/365"))
            (term-structure (new-FlatForward "EUR" day-counter
                                             today settlement 0.05))
            (handle (new-TermStructureHandle term-structure)))
       ,@body
       (delete-TermStructure term-structure)
       (delete-DayCounter day-counter)
       (delete-Date settlement)
       (delete-Calendar calendar)
       (delete-Date today))))

(define (Term-structure-test-1)
  (with-test-structure
    (let* ((tolerance 1.0e-10)
           (new-today (Date-plus-years 
                       (TermStructure-todays-date term-structure)
                       3))
           (new-settlement (Calendar-advance calendar new-today 2 "days"))
           (test-date (Date-plus-years new-settlement 5))
           (implied (new-ImpliedTermStructure handle new-today 
                                              new-settlement))
           (base-discount (TermStructure-discount term-structure
                                                  new-settlement))
           (discount (TermStructure-discount term-structure test-date))
           (implied-discount (TermStructure-discount implied test-date)))
      (check "discount from implied curve"
             (* base-discount implied-discount)
             discount
             tolerance)
      (delete-TermStructure implied)
      (delete-Date test-date)
      (delete-Date new-settlement)
      (delete-Date new-today))))

(define (Term-structure-test-2)
  (with-test-structure
   (let ((flag #f))
     (let* ((new-today (Date-plus-years 
                        (TermStructure-todays-date term-structure)
                        3))
            (new-settlement (Calendar-advance calendar new-today 2 "days"))
            (implied (new-ImpliedTermStructure handle new-today 
                                               new-settlement))
            (obs (new-Observer (lambda () (set! flag #t)))))
       (let ((temp (TermStructure->Observable implied)))
         (Observer-register-with obs temp)
         (delete-Observable temp))
       (TermStructureHandle-link-to! handle term-structure)
       (if (not flag)
           (error "Observer was not notified of term structure change"))
       (delete-Observer obs)
       (delete-TermStructure implied)
       (delete-Date new-settlement)
       (delete-Date new-today)))))
             
(define (Term-structure-test-3)
  (with-test-structure
   (let* ((tolerance 1.0e-10)
          (me (new-SimpleMarketElement 0.01))
          (h (new-MarketElementHandle me))
          (test-date (Date-plus-years (TermStructure-todays-date
                                       term-structure) 5))
          (spreaded (new-ForwardSpreadedTermStructure handle h))
          (forward (TermStructure-forward term-structure test-date))
          (spreaded-forward (TermStructure-forward spreaded test-date)))
     (check "forward from spreaded curve"
            (- spreaded-forward (MarketElement-value me))
            forward
            tolerance)
     (delete-TermStructure spreaded)
     (delete-Date test-date)
     (delete-MarketElementHandle h)
     (delete-MarketElement me))))

(define (Term-structure-test-4)
  (with-test-structure
   (let ((flag #f))
     (let* ((tolerance 1.0e-10)
            (me (new-SimpleMarketElement 0.01))
            (h (new-MarketElementHandle me))
            (spreaded (new-ForwardSpreadedTermStructure handle h))
            (obs (new-Observer (lambda () (set! flag #t)))))
       (let ((temp (TermStructure->Observable spreaded)))
         (Observer-register-with obs temp)
         (delete-Observable temp))
       (TermStructureHandle-link-to! handle term-structure)
       (if (not flag)
           (error "Observer was not notified of term structure change"))
       (set! flag #f)
       (SimpleMarketElement-value-set! me 0.005)
       (if (not flag)
           (error "Observer was not notified of spread change"))
       (delete-Observer obs)
       (delete-TermStructure spreaded)
       (delete-MarketElementHandle h)
       (delete-MarketElement me)))))

(define (Term-structure-test-5)
  (with-test-structure
   (let* ((tolerance 1.0e-10)
          (me (new-SimpleMarketElement 0.01))
          (h (new-MarketElementHandle me))
          (test-date (Date-plus-years (TermStructure-todays-date
                                       term-structure) 5))
          (spreaded (new-ZeroSpreadedTermStructure handle h))
          (zero (TermStructure-zero-yield term-structure test-date))
          (spreaded-zero (TermStructure-zero-yield spreaded test-date)))
     (check "zero yield from spreaded curve"
            (- spreaded-zero (MarketElement-value me))
            zero
            tolerance)
     (delete-TermStructure spreaded)
     (delete-Date test-date)
     (delete-MarketElementHandle h)
     (delete-MarketElement me))))

(define (Term-structure-test-6)
  (with-test-structure
   (let ((flag #f))
     (let* ((tolerance 1.0e-10)
            (me (new-SimpleMarketElement 0.01))
            (h (new-MarketElementHandle me))
            (spreaded (new-ZeroSpreadedTermStructure handle h))
            (obs (new-Observer (lambda () (set! flag #t)))))
       (let ((temp (TermStructure->Observable spreaded)))
         (Observer-register-with obs temp)
         (delete-Observable temp))
       (TermStructureHandle-link-to! handle term-structure)
       (if (not flag)
           (error "Observer was not notified of term structure change"))
       (set! flag #f)
       (SimpleMarketElement-value-set! me 0.005)
       (if (not flag)
           (error "Observer was not notified of spread change"))
       (delete-Observer obs)
       (delete-TermStructure spreaded)
       (delete-MarketElementHandle h)
       (delete-MarketElement me)))))


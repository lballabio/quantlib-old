; Copyright (C) 2002, 2003 RiskMap srl
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
; program; if not, please email quantlib-dev@lists.sf.net
;
; The QuantLib License is also available at http://quantlib.org/license.html
; The members of the QuantLib Group are listed in the QuantLib License

(load "common.scm")
(load "unittest.scm")
(use-modules (QuantLib))

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
      (let* ((real-today (Date-todays-date))
             (calendar (new-Calendar "TARGET"))
             (today (Calendar-adjust calendar real-today))
             (settlement (Calendar-advance calendar today
                                           settlement-days "days"))
             (day-counter (new-DayCounter "act/360"))
             (day-counter-2 (new-DayCounter "30/360"))
             (deposits (map
                        (lambda (datum)
                          (let-at-once ((n units rate) datum)
                            (new-DepositRateHelper
                             (new-QuoteHandle (new-SimpleQuote (/ rate 100)))
                             n units settlement-days
                             calendar "mf" day-counter)))
                        deposit-data))
             (swaps (map 
                     (lambda (datum)
                       (let-at-once ((years rate) datum)
                         (new-SwapRateHelper
                          (new-QuoteHandle (new-SimpleQuote (/ rate 100)))
                          years "years" settlement-days
                          calendar 1 "unadjusted" day-counter-2
                          2 "mf")))
                     swap-data))
             (term-structure (new-PiecewiseFlatForward
                              today settlement 
                              (append deposits swaps)
                              day-counter)))
        (new-TermStructureHandle term-structure))))
  ; setup
  (let ((term-structure (make-test-structure-handle)))
    (cond ((equal? tag 'implied-obs)
           ; check implied term structure observability
           (let ((flag #f))
             (let* ((calendar (new-Calendar "TARGET"))
                    (today (TermStructureHandle-todays-date 
                            term-structure))
                    (settlement (TermStructureHandle-reference-date 
                                 term-structure))
                    (new-today (Date-plus-years today 3))
                    (new-settlement (Calendar-advance
                                     calendar new-today 2 "days"))
                    (day-counter (TermStructureHandle-day-counter
                                  term-structure))
                    (implied (new-ImpliedTermStructure term-structure 
                                                       new-today
                                                       new-settlement))
                    (obs (new-Observer (lambda () (set! flag #t)))))
               (let ((temp (TermStructure->Observable implied)))
                 (Observer-register-with obs temp))
               (let ((new-term-structure (new-FlatForward today
                                                          settlement
                                                          0.05 
                                                          day-counter)))
                 (TermStructureHandle-link-to! term-structure 
                                               new-term-structure))
               (check flag
                      "Observer was not notified of term structure change"))))
          ((equal? tag 'fw-spreaded-obs)
           ; check forward-spreaded term structure observability
           (let ((flag #f)
                 (tolerance 1.0e-10))
             (let* ((today (TermStructureHandle-todays-date 
                            term-structure))
                    (settlement (TermStructureHandle-reference-date 
                                 term-structure))
                    (day-counter (TermStructureHandle-day-counter
                                  term-structure))
                    (me (new-SimpleQuote 0.01))
                    (h (new-QuoteHandle me))
                    (spreaded (new-ForwardSpreadedTermStructure 
                               term-structure h))
                    (obs (new-Observer (lambda () (set! flag #t)))))
               (let ((temp (TermStructure->Observable spreaded)))
                 (Observer-register-with obs temp))
               (let ((new-term-structure (new-FlatForward today
                                                          settlement 
                                                          0.05 
                                                          day-counter)))
                 (TermStructureHandle-link-to! term-structure 
                                               new-term-structure))
               (and
                (check flag
                       "Observer was not notified of term structure change")
                (begin
                  (set! flag #f)
                  (SimpleQuote-value-set! me 0.005)
                  (check flag
                         "Observer was not notified of spread change"))))))
          ((equal? tag 'z-spreaded-obs)
           ; check zero-spreaded term structure observability
           (let ((flag #f)
                 (tolerance 1.0e-10))
             (let* ((today (TermStructureHandle-todays-date 
                            term-structure))
                    (settlement (TermStructureHandle-reference-date 
                                 term-structure))
                    (day-counter (TermStructureHandle-day-counter
                                  term-structure))
                    (me (new-SimpleQuote 0.01))
                    (h (new-QuoteHandle me))
                    (spreaded (new-ZeroSpreadedTermStructure 
                               term-structure h))
                    (obs (new-Observer (lambda () (set! flag #t)))))
               (let ((temp (TermStructure->Observable spreaded)))
                 (Observer-register-with obs temp))
               (let ((new-term-structure (new-FlatForward today
                                                          settlement 
                                                          0.05 
                                                          day-counter)))
                 (TermStructureHandle-link-to! term-structure 
                                               new-term-structure))
               (and
                (check flag
                       "Observer was not notified of term structure change")
                (begin
                  (set! flag #f)
                  (SimpleQuote-value-set! me 0.005)
                  (check flag
                         "Observer was not notified of spread change")))))))))

(greg-assert/display
 "Testing observability of implied term structure"
 (TermStructure-test 'implied-obs))

(greg-assert/display
 "Testing observability of forward-spreaded term structure"
 (TermStructure-test 'fw-spreaded-obs))

(greg-assert/display
 "Testing observability of zero-spreaded term structure"
 (TermStructure-test 'z-spreaded-obs))


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
; program; if not, please email ferdinando@ametrano.net
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
      (deleting-let* ((real-today (Date-todays-date) delete-Date)
                      (calendar (new-Calendar "TARGET") delete-Calendar)
                      (today (Calendar-roll calendar real-today) delete-Date)
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
                                      n units settlement-days
                                      calendar "mf" day-counter)))
                                 deposit-data)
                                (lambda (l)
                                  (for-each delete-RateHelper l)))
                      (swaps (map 
                              (lambda (datum)
                                (let-at-once ((years rate) datum)
                                  (new-SwapRateHelper
                                   (new-MarketElementHandle
                                    (new-SimpleMarketElement (/ rate 100)))
                                   years "years" settlement-days
                                   calendar "mf" 1 #f day-counter-2 2)))
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
    (cond ((equal? tag 'implied-obs)
           ; check implied term structure observability
           (let ((flag #f))
             (deleting-let* ((calendar (new-Calendar "TARGET")
                                     delete-Calendar)
                             (today (TermStructureHandle-todays-date 
                                     term-structure)
                                    delete-Date)
                             (settlement (TermStructureHandle-reference-date 
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
               (check flag
                      "Observer was not notified of term structure change"))))
          ((equal? tag 'fw-spreaded-obs)
           ; check forward-spreaded term structure observability
           (let ((flag #f)
                 (tolerance 1.0e-10))
             (deleting-let* ((today (TermStructureHandle-todays-date 
                                     term-structure)
                                    delete-Date)
                             (settlement (TermStructureHandle-reference-date 
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
               (and
                (check flag
                       "Observer was not notified of term structure change")
                (begin
                  (set! flag #f)
                  (SimpleMarketElement-value-set! me 0.005)
                  (check flag
                         "Observer was not notified of spread change"))))))
          ((equal? tag 'z-spreaded-obs)
           ; check zero-spreaded term structure observability
           (let ((flag #f)
                 (tolerance 1.0e-10))
             (deleting-let* ((today (TermStructureHandle-todays-date 
                                     term-structure)
                                    delete-Date)
                             (settlement (TermStructureHandle-reference-date 
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
               (and
                (check flag
                       "Observer was not notified of term structure change")
                (begin
                  (set! flag #f)
                  (SimpleMarketElement-value-set! me 0.005)
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


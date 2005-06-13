
; Copyright (C) 2005 Aurelien Chanudet
;
; This file is part of QuantLib, a free-software/open-source library
; for financial quantitative analysts and developers - http://quantlib.org/
;
; QuantLib is free software: you can redistribute it and/or modify it under the
; terms of the QuantLib license.  You should have received a copy of the
; license along with this program; if not, please email quantlib-dev@lists.sf.net
; The license is also available online at http://quantlib.org/html/license.html
;
; This program is distributed in the hope that it will be useful, but WITHOUT
; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
; FOR A PARTICULAR PURPOSE.  See the license for more details.

(use-modules (ice-9 format))

(dynamic-call "init_qladdin" (dynamic-link "libQuantLibAddinGuile"))

(define false #f)

(define (call-func func . args)
    (func args))

(call-func qlConsole 1 5)

(define deposit-quotes '((  1 . 0.020800 )
                         (  3 . 0.021500 )
                         (  6 . 0.022120 )
                         ( 12 . 0.023920 )))

(define swap-quotes    '((  2 . 0.027310 )
                         (  3 . 0.030005 )
                         (  4 . 0.032365 )
                         (  5 . 0.034385 )
                         (  6 . 0.036165 )
                         (  7 . 0.037750 )
                         (  8 . 0.039100 )
                         (  9 . 0.040230 )
                         ( 10 . 0.041170 )
                         ( 12 . 0.042670 )
                         ( 15 . 0.044365 )
                         ( 20 . 0.046145 )
                         ( 30 . 0.047280 )))

(define (make-deposit-rate-helper q maturity)
    (let ((handle (format false "handleDeposit~d" maturity)))
        (call-func qlDepositRateHelper
                   handle
                   q
                   maturity
                   "Months"
                   2
                   "TARGET"
                   "ModifiedFollowing"
                   "Actual360")
        handle))

(define (make-swap-rate-helper q maturity)
    (let ((handle (format false "swapDeposit~d" maturity)))
        (call-func qlSwapRateHelper
                   handle
                   q
                   maturity
                   "Years"
                   2
                   "TARGET"
                   "Annual"
                   "Unadjusted"
                   "Thirty360"
                   "Semiannual"
                   "ModifiedFollowing")
        handle))

(define (make-rate-helpers func quotes)
    (let lp ((rate-helpers '()) (quotes quotes))
        (if (null? quotes)
            rate-helpers
            (lp (cons (func (cdar quotes) (caar quotes)) rate-helpers) (cdr quotes)))))

(define evaluation 38434) ; 23 March 2005
(define settlement 38436) ; 25 March 2005

(define rate-helpers (append (make-rate-helpers make-deposit-rate-helper deposit-quotes)
                             (make-rate-helpers make-swap-rate-helper swap-quotes)))

(call-func qlPiecewiseFlatForward "myTermStructure"
                                  evaluation
                                  settlement
                                  rate-helpers
                                  "Actual360")

(call-func qlHullWhite "myShortRateModel" "myTermStructure" 0.1 0.001)

(call-func qlAnalyticCapFloorEngine "myPricer" "myShortRateModel")

(define start 38600)

(define (make-collar handle pricer)
    (call-func qlCapFloor          ; constructor
               handle              ; object handle
               start               ; start of capping period
               5                   ; capping period length
               "Years"             ; time units
               "ModifiedFollowing" ; business day convention
               "Semiannual"        ; capping frequency (semiannual)
               2                   ; fixing days
               "myTermStructure"   ; term structure
               100000.0            ; nominal
               0.04                ; cap srike
               0.02                ; floor strike
               pricer              ; pricer
               "Collar"            ; option type
               0))                 ; no amortisation

(make-collar "myCollar" "myPricer")

(call-func qlLogObject "myCollar")



; Copyright (C) 2006 Eric Ehlers
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

(dynamic-call "init_qladdin" (dynamic-link "libQuantLibAddinGuile"))

(define false #f)

(define (print . args)
    (for-each (lambda (item) (display item)) args))

(define (call-func func . args)
    (func args))

(define (date day month year)
    (qlDate (list day month year)))

(call-func ohSetConsole 1 5)

; -- qlSetEvaluationDate

(define evaluationDate (date 15 "May" 1998))

(call-func qlSetEvaluationDate evaluationDate)

; -- qlBlackConstantVol

(define settlementDate (date 17 "May" 1998))

(call-func qlBlackConstantVol   ; constructor
           "my_blackconstantvol"; object ID
           settlementDate       ; settlement date
           0.20                 ; volatility
           "Actual/365 (Fixed)"); day counter

; -- qlGeneralizedBlackScholesProcess

(call-func qlGeneralizedBlackScholesProcess   ; constructor
           "my_stochastic"      ; object ID
           "my_blackconstantvol"; black constant vol
           36                   ; underlying
           "Actual/365 (Fixed)" ; day counter
           settlementDate       ; settlement date
           0.06                 ; risk free rate
           0.00)                ; dividend yield

; -- qlEuropeanExercise

(define exerciseDate (date 17 "May" 1999))

(call-func qlEuropeanExercise   ; constructor
           "my_exercise"        ; object ID
           exerciseDate)        ; exercise date

; -- qlStrikedTypePayoff

(call-func qlStrikedTypePayoff  ; constructor
           "my_payoff"          ; object ID
           "vanilla"            ; payoff
           "put"                ; call/put
           40.0)                ; strike

; -- qlPricingEngine

(call-func qlPricingEngine      ; constructor
           "my_engine"          ; object ID
           "AE")                ; analytic european

; -- qlVanillaOption

(call-func qlVanillaOption      ; constructor
           "my_option"          ; object ID
           "my_stochastic"      ; stochastic object
           "my_payoff"          ; payoff object
           "my_exercise"        ; exercise object
           "my_engine")         ; engine object

; -- NPV

(print "npv : " (call-func qlNPV "my_option") "\n")


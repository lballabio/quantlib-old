
; Copyright (C) 2005, 2006 Eric Ehlers
; Copyright (C) 2005 Aurelien Chanudet
; Copyright (C) 2005 Plamen Neykov
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

(define (call-func func . args)
    (func args))

(define (date day month year)
    (qlDate (list day month year)))

(call-func ohSetConsole 1 5)

(define false #f)
(define true  #t)

(define (print . args)
    (for-each (lambda (item) (display item)) args))


; -- Cash

(define deposits (list
    '( "O/N" 0.02065  1 "Days"   0 "TARGET" "ModifiedFollowing" "Actual365Fixed" )
    '( "T/N" 0.02070  1 "Days"   1 "TARGET" "ModifiedFollowing" "Actual365Fixed" )
    '(  "1W" 0.02070  1 "Weeks"  2 "TARGET" "ModifiedFollowing" "Actual365Fixed" )
    '(  "1M" 0.02085  1 "Months" 2 "TARGET" "ModifiedFollowing" "Actual365Fixed" )
    '(  "2M" 0.02105  2 "Months" 2 "TARGET" "ModifiedFollowing" "Actual365Fixed" )
    '(  "3M" 0.02125  3 "Months" 2 "TARGET" "ModifiedFollowing" "Actual365Fixed" )
    '(  "6M" 0.02165  6 "Months" 2 "TARGET" "ModifiedFollowing" "Actual365Fixed" )
    '(  "9M" 0.02215  9 "Months" 2 "TARGET" "ModifiedFollowing" "Actual365Fixed" )
    '( "12M" 0.02275 12 "Months" 2 "TARGET" "ModifiedFollowing" "Actual365Fixed" )))

(for-each qlDepositRateHelper deposits)


; -- Futures

(define futures (list
    '( "M5" 97.830 "M5" 3 "Actual360" "ModifiedFollowing" "TARGET" 2000 )
    '( "U5" 97.735 "U5" 3 "Actual360" "ModifiedFollowing" "TARGET" 2000 )
    '( "Z5" 97.610 "Z5" 3 "Actual360" "ModifiedFollowing" "TARGET" 2000 )
    '( "H6" 97.490 "H6" 3 "Actual360" "ModifiedFollowing" "TARGET" 2000 )
    '( "M6" 97.355 "M6" 3 "Actual360" "ModifiedFollowing" "TARGET" 2000 )
    '( "U6" 97.235 "U6" 3 "Actual360" "ModifiedFollowing" "TARGET" 2000 )
    '( "Z6" 97.120 "Z6" 3 "Actual360" "ModifiedFollowing" "TARGET" 2000 )
    '( "H7" 97.050 "H7" 3 "Actual360" "ModifiedFollowing" "TARGET" 2000 )
    '( "M7" 96.975 "M7" 3 "Actual360" "ModifiedFollowing" "TARGET" 2000 )
    '( "U7" 96.900 "U7" 3 "Actual360" "ModifiedFollowing" "TARGET" 2000 )
    '( "Z7" 96.810 "Z7" 3 "Actual360" "ModifiedFollowing" "TARGET" 2000 )
    '( "H8" 96.755 "H8" 3 "Actual360" "ModifiedFollowing" "TARGET" 2000 )))

(for-each qlFuturesRateHelper futures)


; -- Swaps

(define swaps (list
    '(  "2Y" 0.02552  2 "Years" 2 "TARGET" "Annual" "ModifiedFollowing" "Thirty360"
                                           "Semiannual" "ModifiedFollowing" "Thirty360" )
    '(  "3Y" 0.02748  3 "Years" 2 "TARGET" "Annual" "ModifiedFollowing" "Thirty360"
                                           "Semiannual" "ModifiedFollowing" "Thirty360" )
    '(  "4Y" 0.02919  4 "Years" 2 "TARGET" "Annual" "ModifiedFollowing" "Thirty360"
                                           "Semiannual" "ModifiedFollowing" "Thirty360" )
    '(  "5Y" 0.03070  5 "Years" 2 "TARGET" "Annual" "ModifiedFollowing" "Thirty360"
                                           "Semiannual" "ModifiedFollowing" "Thirty360" )
    '(  "6Y" 0.03212  6 "Years" 2 "TARGET" "Annual" "ModifiedFollowing" "Thirty360"
                                           "Semiannual" "ModifiedFollowing" "Thirty360" )
    '(  "7Y" 0.03342  7 "Years" 2 "TARGET" "Annual" "ModifiedFollowing" "Thirty360"
                                           "Semiannual" "ModifiedFollowing" "Thirty360" )
    '(  "8Y" 0.03457  8 "Years" 2 "TARGET" "Annual" "ModifiedFollowing" "Thirty360"
                                           "Semiannual" "ModifiedFollowing" "Thirty360" )
    '(  "9Y" 0.03560  9 "Years" 2 "TARGET" "Annual" "ModifiedFollowing" "Thirty360"
                                           "Semiannual" "ModifiedFollowing" "Thirty360" )
    '( "10Y" 0.03648 10 "Years" 2 "TARGET" "Annual" "ModifiedFollowing" "Thirty360"
                                           "Semiannual" "ModifiedFollowing" "Thirty360" )
    '( "11Y" 0.03742 11 "Years" 2 "TARGET" "Annual" "ModifiedFollowing" "Thirty360"
                                           "Semiannual" "ModifiedFollowing" "Thirty360" )
    '( "12Y" 0.03789 12 "Years" 2 "TARGET" "Annual" "ModifiedFollowing" "Thirty360"
                                           "Semiannual" "ModifiedFollowing" "Thirty360" )
    '( "15Y" 0.03944 15 "Years" 2 "TARGET" "Annual" "ModifiedFollowing" "Thirty360"
                                           "Semiannual" "ModifiedFollowing" "Thirty360" )
    '( "20Y" 0.04100 20 "Years" 2 "TARGET" "Annual" "ModifiedFollowing" "Thirty360"
                                           "Semiannual" "ModifiedFollowing" "Thirty360" )
    '( "25Y" 0.04173 25 "Years" 2 "TARGET" "Annual" "ModifiedFollowing" "Thirty360"
                                           "Semiannual" "ModifiedFollowing" "Thirty360" )
    '( "30Y" 0.04206 30 "Years" 2 "TARGET" "Annual" "ModifiedFollowing" "Thirty360"
                                           "Semiannual" "ModifiedFollowing" "Thirty360" )
    '( "40Y" 0.04233 40 "Years" 2 "TARGET" "Annual" "ModifiedFollowing" "Thirty360"
                                           "Semiannual" "ModifiedFollowing" "Thirty360" )
    '( "50Y" 0.04238 50 "Years" 2 "TARGET" "Annual" "ModifiedFollowing" "Thirty360"
                                           "Semiannual" "ModifiedFollowing" "Thirty360" )))

(for-each qlSwapRateHelper swaps)


; -- Bootstrap Term Structure

(define evaluation (date 07 "April" 2005))

(call-func qlSetEvaluationDate evaluation)

(define settlement (date 07 "April" 2005))

(define rh (append (map car deposits) (map car futures) (map car swaps)))

(call-func qlPiecewiseFlatForward "testYC" settlement rh "Actual360")


; -- Interrogate Object

(define (arith-sequence start step count)
    (define (f acc start nleft)
        (if (= nleft 0)
            (reverse acc)
            (f (cons start acc) (+ start step) (- nleft 1))))
    (f '() start count))

(define dates (map (lambda (year) (date 11 "April" year)) (arith-sequence 2005 2 28)))

; (for-each (lambda (date) (print (call-func qlGetDf "testYC" date true 0) "\n")) dates)


; -- Evaluate Swap

(define historic (list
    (cons (date  7 "Apr" 2004) 0.03)
    (cons (date  8 "Apr" 2004) 0.03)
    (cons (date  9 "Apr" 2004) 0.03)
    (cons (date 10 "Apr" 2004) 0.03)
    (cons (date 11 "Apr" 2004) 0.03)
    (cons (date 12 "Apr" 2004) 0.03)
    (cons (date  7 "Oct" 2004) 0.03)
    (cons (date  8 "Oct" 2004) 0.03)
    (cons (date  9 "Oct" 2004) 0.03)
    (cons (date 10 "Oct" 2004) 0.03)
    (cons (date 11 "Oct" 2004) 0.03)
    (cons (date 12 "Oct" 2004) 0.03)))

(define dates   (map car historic))
(define fixings (map cdr historic))

(call-func qlXibor              ; constructor
           "idx"                ; object ID
           "Euribor"            ; index name
           "EUR"                ; currency
           6                    ; tenor
           "Months"             ; time units
           "TARGET"             ; calendard
           "ModifiedFollowing"  ; businesss day convention
           "Actual365Fixed"     ; day counting convention
           2                    ; fixing days
           "testYC"             ; forecasting term structure
           dates                ; past dates
           fixings)             ; past fixings

(call-func qlVanillaSwap        ; constructor
           "swp"                ; object ID
           (date 11 "Apr" 2005) ; start date
           (date 11 "Apr" 2015) ; maturity date
           1000000              ; nominal
           true                 ; pay fixed
           0.03648              ; fixed rate
           "TARGET"             ; calendar
           "Annual"             ; fixed leg frequency
           "ModifiedFollowing"  ; fixed leg business day convention
           "Thirty360"          ; fixed leg day counting convention
           false                ; don't build fixed leg schedule backward         
           false                ; long last
           "Semiannual"         ; floating leg frequency
           "Thirty360"          ; floating leg day counting convention
           "idx"                ; underlying index
           false                ; don't build floating leg schedule backward
           false                ; long last
           0.0                  ; floating leg additive spread
           "testYC")            ; discounting term structure

(print "npv         : " (call-func qlNPV "swp") "\n")


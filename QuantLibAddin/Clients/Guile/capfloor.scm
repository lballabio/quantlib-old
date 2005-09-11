
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

(dynamic-call "init_qladdin" (dynamic-link "libQuantLibAddinGuile"))

(define false #f)

(define (call-func func . args)
    (func args))

(define (date day month year)
    (qlDate (list day month year)))

(call-func qlConsole 1 5)

; -- Term Structure

(define cash-helpers (list
    '( "1M"	0.021000 1 "Months" 2 "NullCalendar" "Unadjusted" "Simple" )
    '( "3M"	0.021200 3 "Months" 2 "NullCalendar" "Unadjusted" "Simple" )
    '( "6M"	0.021400 6 "Months" 2 "NullCalendar" "Unadjusted" "Simple" )))

(for-each qlDepositRateHelper cash-helpers)

(define swap-helpers
    (let ((freq "Annual")
          (conv "Unadjusted")) (list
    (list "1Y"  0.021990  1 "Years" 2 "NullCalendar" freq conv "Simple" freq conv )
    (list "2Y"  0.023200  2 "Years" 2 "NullCalendar" freq conv "Simple" freq conv )
    (list "3Y"  0.024680  3 "Years" 2 "NullCalendar" freq conv "Simple" freq conv )
    (list "4Y"  0.026180  4 "Years" 2 "NullCalendar" freq conv "Simple" freq conv )
    (list "5Y"  0.027590  5 "Years" 2 "NullCalendar" freq conv "Simple" freq conv )
    (list "6Y"  0.028920  6 "Years" 2 "NullCalendar" freq conv "Simple" freq conv )
    (list "7Y"  0.030150  7 "Years" 2 "NullCalendar" freq conv "Simple" freq conv )
    (list "8Y"  0.031290  8 "Years" 2 "NullCalendar" freq conv "Simple" freq conv )
    (list "9Y"  0.032290  9 "Years" 2 "NullCalendar" freq conv "Simple" freq conv )
    (list "10Y" 0.033170 10 "Years" 2 "NullCalendar" freq conv "Simple" freq conv )
    (list "11Y" 0.033930 11 "Years" 2 "NullCalendar" freq conv "Simple" freq conv )
    (list "12Y" 0.034590 12 "Years" 2 "NullCalendar" freq conv "Simple" freq conv )
    (list "13Y" 0.035160 13 "Years" 2 "NullCalendar" freq conv "Simple" freq conv )
    (list "14Y" 0.035660 14 "Years" 2 "NullCalendar" freq conv "Simple" freq conv )
    (list "15Y" 0.036110 15 "Years" 2 "NullCalendar" freq conv "Simple" freq conv )
    (list "16Y" 0.036520 16 "Years" 2 "NullCalendar" freq conv "Simple" freq conv )
    (list "17Y" 0.036880 17 "Years" 2 "NullCalendar" freq conv "Simple" freq conv )
    (list "18Y" 0.037190 18 "Years" 2 "NullCalendar" freq conv "Simple" freq conv )
    (list "19Y" 0.037450 19 "Years" 2 "NullCalendar" freq conv "Simple" freq conv )
    (list "20Y" 0.037670 20 "Years" 2 "NullCalendar" freq conv "Simple" freq conv ))))

(for-each qlSwapRateHelper swap-helpers)

(define evaluation (date 27 "July" 2005))

(define settlement (date 29 "July" 2005))

(define hh (append (map car cash-helpers) (map car swap-helpers)))

(call-func qlPiecewiseFlatForward "YC" evaluation settlement hh "Simple")

; -- Euribor Index

(define dates   '())
(define fixings '())

(call-func qlXibor              ; constructor
           "IDX"                ; handle
           "Euribor"            ; index name
           "EUR"                ; currency
           12                   ; tenor
           "Months"             ; time units
           "NullCalendar"       ; calendar
           "Unadjusted"         ; businesss day convention
           "Simple"             ; day counting convention
           2                    ; fixing days
           "YC"                 ; forecasting term structure
           dates                ; past dates
           fixings)             ; past fixings

; -- Cap/Floor

(call-func qlHullWhite "HW" "YC" 0.1 0.005)

(call-func qlAnalyticCapFloorEngine "engine" "HW")

(define (arith-sequence start delta steps)
    (define (h start nleft acc) ; tail-recursive helper function
        (if (= nleft 0)
            acc
            (h (+ start delta) (- nleft 1) (cons start acc))))
    (h start steps '()))

(define nominals (arith-sequence    1 0 6))
(define spreads  (arith-sequence    0 0 6))
(define strikes  (arith-sequence 0.04 0 6))

(define start (date 29 "July" 2005))
(define end   (date 29 "July" 2011))

(call-func qlSchedule
    "schedule" "NullCalendar" start end "Annual" "Unadjusted" false false)

(call-func qlFixedRateCouponVector
    "fixedLeg" "schedule" "Unadjusted" nominals strikes "Simple")

(call-func qlFloatingRateCouponVector
    "floatLeg" "schedule" nominals "IDX" spreads)

(define (make-option handle option)
    (call-func qlCapFloor
               handle
               "floatLeg"
               "YC"
               strikes
               strikes
               "engine"
               option))

(make-option "CAP"   "CAP")
(make-option "FLOOR" "FLOOR")

(define cPremium (call-func qlNPV "CAP"   1))
(define fPremium (call-func qlNPV "FLOOR" 1))
(define sPremium (- fPremium cPremium))

(print "cap         : " cPremium "\n"
       "floor       : " fPremium "\n"
       "floor - cap : " sPremium "\n")

(call-func qlSwap "SWP" "floatLeg" "fixedLeg" "YC")

(print "swap        : " (call-func qlNPV "SWP" 1) "\n")

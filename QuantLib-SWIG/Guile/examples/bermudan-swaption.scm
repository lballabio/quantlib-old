
; Copyright (C) 2004 StatPro Italia srl
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

(use-modules (QuantLib))
(load "tabulate.scm")

(define swaption-vols
  (list ; maturity,          length,             volatility
   (list (new-Period 1 "Year")  (new-Period 1 "Year")  0.1810)
   (list (new-Period 1 "Year")  (new-Period 3 "Years")  0.1590)
   (list (new-Period 1 "Year")  (new-Period 5 "Years")  0.1400)
   (list (new-Period 1 "Year")  (new-Period 10 "Years") 0.1220)
   (list (new-Period 2 "Years") (new-Period 5 "Years") 0.1290)
   (list (new-Period 2 "Years") (new-Period 7 "Years") 0.1230)
   (list (new-Period 3 "Years") (new-Period 5 "Years") 0.1201)
   (list (new-Period 4 "Years") (new-Period 4 "Years") 0.1189)
   (list (new-Period 4 "Years") (new-Period 5 "Years") 0.1146)
   (list (new-Period 5 "Years") (new-Period 3 "Years") 0.1183)
   (list (new-Period 5 "Years") (new-Period 5 "Years") 0.1108)
   (list (new-Period 7 "Years") (new-Period 2 "Years") 0.1110)
   (list (new-Period 7 "Years") (new-Period 5 "Years") 0.1040)
   (list (new-Period 10 "Years") (new-Period 1 "Year")  0.1109)
   (list (new-Period 10 "Years") (new-Period 5 "Years")  0.0977)))

(define (format-vol v digits)
  (format #f "~a %" (round-with-digits (* v 100) digits)))

(define (format-price p digits)
  (format #f "~a %" (round-with-digits p digits)))

(define (average xs)
  (define (sum xs accum)
    (if (null? xs)
        accum
        (sum (cdr xs) (+ accum (car xs)))))
  (/ (sum xs 0.0) (length xs)))

(define (insert-unique x xs)
  (cond ((null? xs) (list x))
        ((= x (car xs)) xs)
        ((< x (car xs)) (cons x xs))
        (else (cons (car xs) (insert-unique x (cdr xs))))))
(define (insert-many-unique ys xs)
  (if (null? ys)
      xs
      (insert-many-unique (cdr ys) (insert-unique (car ys) xs))))
(define (collect-times helpers)
  (define (collect-iter hs ts)
    (if (null? hs)
        ts
        (collect-iter (cdr hs) (insert-many-unique
                                (vector->list (SwaptionHelper-times (car hs)))
                                ts))))
  (collect-iter helpers '()))


(define (calibrate model helpers l name)
  (let* ((fmt '((12 r) (12 r) (12 r) (12 r) (12 r)))
         (sep " |")
         (header (tabulate fmt sep
                           "maturity" "length" "volatility" "implied" "error"))
         (rule (make-string (string-length header) #\-))
         (dblrule (make-string (string-length header) #\=)))
    (newline)
    (display dblrule) (newline)
    (display name) (newline)
    (display rule) (newline)

    (let ((method (new-Simplex l 1.0e-9)))
      (OptimizationMethod-end-criteria-set! method
                                            (new-EndCriteria 1000 1.0e-7))
      (ShortRateModel-calibrate! model helpers method)

      (display (format #f "Parameters: ~a" (Array->string
                                            (ShortRateModel-params model))))
      (newline)
      (display rule) (newline)
      (display header) (newline)
      (display rule) (newline)

      (let ((errors
             (map (lambda (swaption helper)
                    (let* ((maturity (car swaption))
                           (length (cadr swaption))
                           (vol (caddr swaption))
                           (NPV (CalibrationHelper-model-value helper))
                           (implied (CalibrationHelper-implied-volatility
                                     helper NPV 1.0e-4 1000 0.05 0.50))
                           (error (- implied vol)))
                      (display (tabulate fmt sep
                                         (Period->string maturity)
                                         (Period->string length)
                                         (format-vol vol 4)
                                         (format-vol implied 4)
                                         (format-vol error 4)))
                      (newline)
                      (abs error)))
                  swaption-vols helpers)))
        (display rule) (newline)
        (display (tabulate (list (list (string-length header) 'r)) ""
                           (string-append "Average error: "
                                          (format-vol (average errors) 4))))
        (newline)
        (display dblrule) (newline)))))


(define todays-date (new-Date 15 2 2002))
(Settings-evaluation-date-set! (Settings-instance) todays-date)
(define calendar (new-TARGET))
(define settlement-date (new-Date 19 2 2002))

(define settlement-days 2)
(define day-counter (new-Thirty360))
(define deposit-helpers
  (map (lambda (depo-data)
         (apply (lambda (n unit rate)
                  (new-DepositRateHelper rate n unit settlement-days
                                         calendar "mf" day-counter))
                depo-data))
       '((1 "week"   0.03295)
         (1 "month"  0.0331)
         (3 "months" 0.0329)
         (6 "months" 0.0333)
         (9 "months" 0.0341)
         (1 "year"   0.0353))))

(define fixed-leg-frequency 1)
(define fixed-leg-adjustment "unadjusted")
(define fixed-leg-day-counter (new-Thirty360))
(define floating-leg-frequency 2)
(define floating-leg-adjustment "mf")
(define swap-helpers
  (map (lambda (swap-data)
         (apply (lambda (n unit rate)
                  (new-SwapRateHelper rate n unit settlement-days calendar
                                      fixed-leg-frequency fixed-leg-adjustment
                                      fixed-leg-day-counter
                                      floating-leg-frequency
                                      floating-leg-adjustment))
                swap-data))
       '((2  "years" 0.04875)
         (3  "years" 0.0438)
         (5  "years" 0.0474325)
         (10 "years" 0.051825)
         (20 "years" 0.0545125))))

(define term-structure (new-YieldTermStructureHandle))
(YieldTermStructureHandle-link-to! term-structure
                                   (new-PiecewiseFlatForward
                                    settlement-date
                                    (append deposit-helpers swap-helpers)
                                    (new-Actual360)))


; define the ATM/OTM/ITM swaps

(define pay-fixed #t)
(define fixing-days 2)
(define swap-start (Date-advance settlement-date 1 "year"))
(define index (new-Euribor 6 "months" term-structure))
(define swap-length 5)
(define swap-end (Date-advance swap-start swap-length "years"))

(define fixed-schedule (new-Schedule calendar swap-start swap-end
                                     fixed-leg-frequency fixed-leg-adjustment))
(define floating-schedule (new-Schedule calendar swap-start swap-end
                                        floating-leg-frequency
                                        floating-leg-adjustment))

(define atm-rate (SimpleSwap-fair-rate
                  (new-SimpleSwap pay-fixed 100.0 fixed-schedule 0.0
                                  fixed-leg-day-counter floating-schedule
                                  index fixing-days 0.0 term-structure)))

(define atm-swap (new-SimpleSwap pay-fixed 100.0 fixed-schedule atm-rate
                                 fixed-leg-day-counter floating-schedule
                                 index fixing-days 0.0 term-structure))
(define otm-swap (new-SimpleSwap pay-fixed 100.0 fixed-schedule
                                 (* 1.2 atm-rate)
                                 fixed-leg-day-counter floating-schedule
                                 index fixing-days 0.0 term-structure))
(define itm-swap (new-SimpleSwap pay-fixed 100.0 fixed-schedule
                                 (* 0.8 atm-rate)
                                 fixed-leg-day-counter floating-schedule
                                 index fixing-days 0.0 term-structure))

(define helpers
  (map (lambda (swaption-data)
         (apply
          (lambda (maturity length vol)
            (new-SwaptionHelper maturity length
                                (new-QuoteHandle (new-SimpleQuote vol))
                                index term-structure))
          swaption-data))
       swaption-vols))

(define grid (new-TimeGrid (collect-times helpers)))

(define HW (new-HullWhite term-structure))
(define HW2 (new-HullWhite term-structure))
(define BK (new-BlackKarasinski term-structure))

(display "Calibrating...") (newline)

(for-each (lambda (h) (CalibrationHelper-pricing-engine-set!
                       h (new-JamshidianSwaptionEngine HW)))
          helpers)
(calibrate HW helpers 0.05 "Hull-White (analytic formulae)")

(for-each (lambda (h) (CalibrationHelper-pricing-engine-set!
                       h (new-TreeSwaptionEngine HW2 grid)))
          helpers)
(calibrate HW2 helpers 0.05 "Hull-White (numerical calibration)")

(for-each (lambda (h) (CalibrationHelper-pricing-engine-set!
                       h (new-TreeSwaptionEngine BK grid)))
          helpers)
(calibrate BK helpers 0.05 "Black-Karasinski (numerical calibration)")


; price Bermudan swaptions on defined swaps

(define schedule (new-Schedule calendar swap-start swap-end 1 "mf"))
(define bermudan-dates (Schedule-map schedule (lambda (d) d)))
(define exercise (new-BermudanExercise
                  (reverse (cdr (reverse bermudan-dates)))))

(define fmt '((17 r) (17 r) (17 r) (17 r)))
(define sep " |")
(define header (tabulate fmt sep
                         "model" "in-the-money"
                         "at-the-money" "out-of-the-money"))
(define rule (make-string (string-length header) #\-))
(define dblrule (make-string (string-length header) #\=))

(newline)
(display dblrule) (newline)
(display "Pricing Bermudan swaptions...") (newline)
(display rule) (newline)
(display header) (newline)
(display rule) (newline)

(define atm-swaption (new-Swaption atm-swap exercise term-structure
                                   (new-TreeSwaptionEngine HW 100)))
(define otm-swaption (new-Swaption otm-swap exercise term-structure
                                   (new-TreeSwaptionEngine HW 100)))
(define itm-swaption (new-Swaption itm-swap exercise term-structure
                                   (new-TreeSwaptionEngine HW 100)))

(display (tabulate fmt sep
                   "HW analytic"
                   (format-price (Instrument-NPV itm-swaption) 2)
                   (format-price (Instrument-NPV atm-swaption) 2)
                   (format-price (Instrument-NPV otm-swaption) 2)))
(newline)

(Instrument-pricing-engine-set! atm-swaption
                                (new-TreeSwaptionEngine HW2 100))
(Instrument-pricing-engine-set! otm-swaption
                                (new-TreeSwaptionEngine HW2 100))
(Instrument-pricing-engine-set! itm-swaption
                                (new-TreeSwaptionEngine HW2 100))

(display (tabulate fmt sep
                   "HW numerical"
                   (format-price (Instrument-NPV itm-swaption) 2)
                   (format-price (Instrument-NPV atm-swaption) 2)
                   (format-price (Instrument-NPV otm-swaption) 2)))
(newline)

(Instrument-pricing-engine-set! atm-swaption
                                (new-TreeSwaptionEngine BK 100))
(Instrument-pricing-engine-set! otm-swaption
                                (new-TreeSwaptionEngine BK 100))
(Instrument-pricing-engine-set! itm-swaption
                                (new-TreeSwaptionEngine BK 100))

(display (tabulate fmt sep
                   "BK numerical"
                   (format-price (Instrument-NPV itm-swaption) 2)
                   (format-price (Instrument-NPV atm-swaption) 2)
                   (format-price (Instrument-NPV otm-swaption) 2)))
(newline)

(display dblrule) (newline)


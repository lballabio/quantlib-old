
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

; utility functions

(define (display/cr x)
  (display x)
  (newline))

(define (i-th i l)
  (if (= n 0)
      (car l)
      (i-th (- n 1) (cdr l))))

(define (first n l)
  (if (= n 0)
      '()
      (cons (car l) (first (- n 1) (cdr l)))))
(define (trim-front n l)
  (if (= n 0)
      l
      (trim-front (- n 1) (cdr l))))
(define (trim-back n l)
  (reverse (trim-front n (reverse l))))

(define (format-rate v digits)
  (format #f "~a %" (round-with-digits (* v 100) digits)))

(define (format-price p digits)
  (format #f "~a %" (round-with-digits p digits)))


; global data
(define calendar (new-TARGET))
(define todays-date (new-Date 6 11 2001))
(Settings-evaluation-date-set! (Settings-instance) todays-date)
(define settlement-date (new-Date 8 11 2001))

; market values

(define deposit-data
  (list (list 1 (Weeks)  (new-SimpleQuote 0.0382))
        (list 1 (Months) (new-SimpleQuote 0.0372))
        (list 3 (Months) (new-SimpleQuote 0.0363))
        (list 6 (Months) (new-SimpleQuote 0.0353))
        (list 9 (Months) (new-SimpleQuote 0.0348))
        (list 1 (Years)  (new-SimpleQuote 0.0345))))

(define fra-data
  (list (list 3 6 (new-SimpleQuote 0.037125))
        (list 6 9 (new-SimpleQuote 0.037125))
        (list 9 12 (new-SimpleQuote 0.037125))))

(define futures-data
  (list (list (new-Date 19 12 2001) (new-SimpleQuote 96.2875))
        (list (new-Date 20 3 2002) (new-SimpleQuote 96.7875))
        (list (new-Date 19 6 2002) (new-SimpleQuote 96.9875))
        (list (new-Date 18 9 2002) (new-SimpleQuote 96.6875))
        (list (new-Date 18 12 2002) (new-SimpleQuote 96.4875))
        (list (new-Date 19 3 2003) (new-SimpleQuote 96.3875))
        (list (new-Date 18 6 2003) (new-SimpleQuote 96.2875))
        (list (new-Date 17 9 2003) (new-SimpleQuote 96.0875))))

(define 5-years-swap (new-SimpleQuote 0.0443))
(define swap-data
  (list (list  2 (Years) (new-SimpleQuote 0.037125))
        (list  3 (Years) (new-SimpleQuote 0.0398))
        (list  5 (Years) 5-years-swap)
        (list 10 (Years) (new-SimpleQuote 0.05165))
        (list 15 (Years) (new-SimpleQuote 0.055175))))

; build rate helpers

(define depo-day-counter (new-Actual360))
(define depo-settlement-days 2)
(define deposit-helpers
  (map (lambda (datum)
         (apply (lambda (n units quote)
                  (new-DepositRateHelper (new-QuoteHandle quote)
                                         n units depo-settlement-days
                                         calendar (ModifiedFollowing)
                                         depo-day-counter))
                datum))
       deposit-data))

(define fra-day-counter (new-Actual360))
(define fra-settlement-days 2)
(define fra-helpers
  (map (lambda (datum)
         (apply (lambda (n m quote)
                  (new-FraRateHelper (new-QuoteHandle quote)
                                     n m fra-settlement-days
                                     calendar (ModifiedFollowing)
                                     fra-day-counter))
                datum))
       fra-data))

(define futures-day-counter (new-Actual360))
(define futures-months 3)
(define futures-helpers
  (map (lambda (datum)
         (apply (lambda (date quote)
                  (new-FuturesRateHelper (new-QuoteHandle quote)
                                         date futures-months
                                         calendar (ModifiedFollowing)
                                         futures-day-counter))
                datum))
       futures-data))

(define swap-settlement-days 2)
(define fixed-leg-frequency (Annual))
(define fixed-leg-adjustment (Unadjusted))
(define fixed-leg-day-counter (new-Thirty360))
(define floating-leg-frequency (Semiannual))
(define floating-leg-adjustment (ModifiedFollowing))
(define swap-helpers
  (map (lambda (datum)
         (apply (lambda (n units quote)
                  (new-SwapRateHelper (new-QuoteHandle quote)
                                      n units swap-settlement-days calendar
                                      fixed-leg-frequency fixed-leg-adjustment
                                      fixed-leg-day-counter
                                      floating-leg-frequency
                                      floating-leg-adjustment))
                datum))
       swap-data))

; term structure handles

(define discount-term-structure (new-YieldTermStructureHandle))
(define forecast-term-structure (new-YieldTermStructureHandle))

; term-structure construction

(define depo-futures-swap-curve (new-PiecewiseFlatForward
                                 settlement-date
                                 (append
                                  (first 2 deposit-helpers)
                                  futures-helpers
                                  (trim-front 1 swap-helpers))
                                 (new-Actual360)))

(define depo-fra-swap-curve (new-PiecewiseFlatForward
                             settlement-date
                             (append
                              (first 3 deposit-helpers)
                              fra-helpers
                              swap-helpers)
                             (new-Actual360)))

; swaps to be priced

(define nominal 1000000)
(define length 5)
(define maturity (Calendar-advance calendar settlement-date length (Years)))
(define pay-fixed #t)

(define fixed-rate 0.04)
(define spread 0.0)
(define index-fixing-days 2)
(define index (new-Euribor 6 (Months) forecast-term-structure))

(define fixed-schedule (new-Schedule calendar settlement-date maturity
                                     fixed-leg-frequency fixed-leg-adjustment))
(define floating-schedule (new-Schedule calendar settlement-date maturity
                                        floating-leg-frequency
                                        floating-leg-adjustment))

(define spot (new-SimpleSwap pay-fixed nominal
                             fixed-schedule fixed-rate fixed-leg-day-counter
                             floating-schedule index index-fixing-days spread
                             discount-term-structure))

(define forward-start (Calendar-advance calendar settlement-date 1 (Years)))
(define forward-end (Calendar-advance calendar forward-start length (Years)))
(define fixed-fwd-schedule (new-Schedule calendar forward-start forward-end
                                         fixed-leg-frequency
                                         fixed-leg-adjustment))
(define floating-fwd-schedule (new-Schedule calendar forward-start forward-end
                                            floating-leg-frequency
                                            floating-leg-adjustment))
(define forward (new-SimpleSwap pay-fixed nominal
                                fixed-fwd-schedule fixed-rate
                                fixed-leg-day-counter
                                floating-fwd-schedule
                                index index-fixing-days spread
                                discount-term-structure))

; display results

(define fmt '((14 r) (17 r) (11 r) (15 r)))
(define sep " | ")

(define header (tabulate fmt sep
                         "term structure"
                         "net present value"
                         "fair spread"
                         "fair fixed rate"))
(define (rule)
  (let ((r (make-string (string-length header) #\-)))
    (display/cr r)))
(define (dblrule)
  (let ((r (make-string (string-length header) #\=)))
    (display/cr r)))
(define (tab)
  (let ((t (make-string 8 #\space)))
    (display t)))

(define (report swap name)
  (display/cr (tabulate fmt sep
                        name
                        (format-price (Instrument-NPV swap) 2)
                        (format-rate (SimpleSwap-fair-spread swap) 4)
                        (format-rate (SimpleSwap-fair-rate swap) 4))))

; price the spot swap

(dblrule)
(display/cr (format #f "5-year market swap-rate = ~a"
                    (format-rate (Quote-value 5-years-swap) 2)))
(dblrule)

(tab)
(display/cr (format #f "5-years swap paying ~a"
                    (format-rate fixed-rate 2)))
(display/cr header)
(rule)

(YieldTermStructureHandle-link-to! discount-term-structure
                                   depo-futures-swap-curve)
(YieldTermStructureHandle-link-to! forecast-term-structure
                                   depo-futures-swap-curve)
(report spot "depo-fut-swap")

(YieldTermStructureHandle-link-to! discount-term-structure
                                   depo-fra-swap-curve)
(YieldTermStructureHandle-link-to! forecast-term-structure
                                   depo-fra-swap-curve)
(report spot "depo-fra-swap")

(rule)

; price the 1-year forward swap

(tab)
(display/cr (format #f "5-years 1-year forward swap paying ~a"
                    (format-rate fixed-rate 2)))
(rule)

(YieldTermStructureHandle-link-to! discount-term-structure
                                   depo-futures-swap-curve)
(YieldTermStructureHandle-link-to! forecast-term-structure
                                   depo-futures-swap-curve)
(report forward "depo-fut-swap")

(YieldTermStructureHandle-link-to! discount-term-structure
                                   depo-fra-swap-curve)
(YieldTermStructureHandle-link-to! forecast-term-structure
                                   depo-fra-swap-curve)
(report forward "depo-fra-swap")

; modify the 5-years swap rate and reprice

(SimpleQuote-value-set! 5-years-swap 0.046)

(dblrule)
(display/cr (format #f "5-year market swap-rate = ~a"
                    (format-rate (Quote-value 5-years-swap) 2)))
(dblrule)

(tab)
(display/cr (format #f "5-years swap paying ~a"
                    (format-rate fixed-rate 2)))
(display/cr header)
(rule)

(YieldTermStructureHandle-link-to! discount-term-structure
                                   depo-futures-swap-curve)
(YieldTermStructureHandle-link-to! forecast-term-structure
                                   depo-futures-swap-curve)
(report spot "depo-fut-swap")

(YieldTermStructureHandle-link-to! discount-term-structure
                                   depo-fra-swap-curve)
(YieldTermStructureHandle-link-to! forecast-term-structure
                                   depo-fra-swap-curve)
(report spot "depo-fra-swap")

(rule)

(tab)
(display/cr (format #f "5-years 1-year forward swap paying ~a"
                    (format-rate fixed-rate 2)))
(rule)

(YieldTermStructureHandle-link-to! discount-term-structure
                                   depo-futures-swap-curve)
(YieldTermStructureHandle-link-to! forecast-term-structure
                                   depo-futures-swap-curve)
(report forward "depo-fut-swap")

(YieldTermStructureHandle-link-to! discount-term-structure
                                   depo-fra-swap-curve)
(YieldTermStructureHandle-link-to! forecast-term-structure
                                   depo-fra-swap-curve)
(report forward "depo-fra-swap")


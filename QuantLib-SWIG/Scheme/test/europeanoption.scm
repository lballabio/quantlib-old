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

(define (European-option-Greek-test)
  (define (derivative f obj datum)
    (let* ((x (MarketElement-value datum))
           (dx (/ x 10000))
           (x+ #f) (x- #f))
      (SimpleMarketElement-value-set! datum (+ x dx))
      (set! x+ (f obj))
      (SimpleMarketElement-value-set! datum (- x dx))
      (set! x- (f obj))
      (SimpleMarketElement-value-set! datum x)
      (/ (- x+ x-) (* 2 dx))))
  (define (make-assoc-list)
    '(("dummy" . #f)))
  (define (add-result assoc-list tag datum)
    (set-cdr! assoc-list (cons (cons tag datum) (cdr assoc-list))))
  (deleting-let* ((calendar (new-Calendar "TARGET") delete-Calendar)
                  (underlying (new-SimpleMarketElement 0.0)
                              delete-MarketElement)
                  (volatility (new-SimpleMarketElement 0.0)
                              delete-MarketElement)
                  (q-rate (new-SimpleMarketElement 0.0)
                          delete-MarketElement)
                  (div-curve (eu-test-make-flat-curve q-rate)
                             delete-TermStructureHandle)
                  (r-rate (new-SimpleMarketElement 0.0)
                          delete-MarketElement)
                  (rf-curve (eu-test-make-flat-curve r-rate)
                            delete-TermStructureHandle))
    (let ((tolerance (list
                      (cons "delta" 1.0e-4)
                      (cons "gamma" 1.0e-4)
                      (cons "theta" 1.0e-2)
                      (cons "rho" 1.0e-4)
                      (cons "dividend-rho" 1.0e-4)
                      (cons "vega" 1.0e-4))))
      (for-each-combination ((type '("Call" "Put" "Straddle"))
                             (strike '(50 99.5 100 100.5 150))
                             (ex-days '(365)))
        (deleting-let* ((option (eu-test-make-option type underlying strike 
                                                     div-curve rf-curve 
                                                     ex-days volatility)
                                delete-Instrument)
                        ; time-shifted exercise dates
                        (option+ (eu-test-make-option type underlying strike 
                                                      div-curve rf-curve 
                                                      (+ ex-days 1)
                                                      volatility)
                                 delete-Instrument)
                        (option- (eu-test-make-option type underlying strike 
                                                      div-curve rf-curve 
                                                      (- ex-days 1)
                                                      volatility)
                                 delete-Instrument))
        (for-each-combination ((u '(100))
                               (q '(0.04 0.05 0.06))
                               (r '(0.01 0.05 0.15))
                               (v '(0.11 0.5 1.2)))
          (let ((calculated (make-assoc-list))
                (expected (make-assoc-list)))

            (SimpleMarketElement-value-set! underlying u)
            (SimpleMarketElement-value-set! volatility v)
            (SimpleMarketElement-value-set! q-rate q)
            (SimpleMarketElement-value-set! r-rate r)

            (if (> (Instrument-NPV option) 1.0e-5)
                (begin
                  (add-result expected "delta" (VanillaOption-delta option))
                  (add-result expected "gamma" (VanillaOption-gamma option))
                  (add-result expected "rho" (VanillaOption-rho option))
                  (add-result expected "dividend-rho"
                              (VanillaOption-dividend-rho option))
                  (add-result expected "vega" (VanillaOption-vega option))
                  (add-result expected "theta" (VanillaOption-theta option))

                  (add-result calculated "delta"
                              (derivative Instrument-NPV option underlying))
                  (add-result calculated "gamma"
                              (derivative VanillaOption-delta 
                                          option underlying))
                  (add-result calculated "rho" 
                              (derivative Instrument-NPV option r-rate))
                  (add-result calculated "dividend-rho"
                              (derivative Instrument-NPV option q-rate))
                  (add-result calculated "vega"
                              (derivative Instrument-NPV option volatility))
                  (add-result calculated "theta"
                              (/ (- (Instrument-NPV option-)
                                    (Instrument-NPV option+))
                                 (/ 2 365)))

                  (for-each (lambda (greek)
                              (check-expected
                               (cdr (assoc greek calculated))
                               (cdr (assoc greek expected))
                               (* (cdr (assoc greek tolerance)) u)
                               "Option parameters: "
                               type ", " u ", " strike ", " q ", " r ", " 
                               ex-date ", " v eol
                               greek ":"))
                            '("delta" "gamma" "theta" "rho"
                              "dividend-rho" "vega")))))))))))

(define (European-option-implied-vol-test)
  (deleting-let* ((underlying (new-SimpleMarketElement 0.0)
                              delete-MarketElement)
                  (volatility (new-SimpleMarketElement 0.0)
                              delete-MarketElement)
                  (q-rate (new-SimpleMarketElement 0.0)
                          delete-MarketElement)
                  (div-curve (eu-test-make-flat-curve q-rate)
                             delete-TermStructureHandle)
                  (r-rate (new-SimpleMarketElement 0.0)
                          delete-MarketElement)
                  (rf-curve (eu-test-make-flat-curve r-rate)
                            delete-TermStructureHandle))
    (let ((max-evaluations 100)
          (tolerance 1.0e-6))
      (for-each-combination ((type '("Call" "Put" "Straddle"))
                             (strike '(50 99.5 100 100.5 150))
                             (ex-days '(36 180 360 1080)))
        (deleting-let* ((option (eu-test-make-option type underlying strike 
                                                     div-curve rf-curve 
                                                     ex-days volatility)
                                delete-Instrument))
          (for-each-combination ((u '(80 95 99.9 100 100.1 105 120))
                                 (q '(0.01 0.05 0.10))
                                 (r '(0.01 0.05 0.10))
                                 (v '(0.01 0.2 0.3 0.7 0.9)))

            (SimpleMarketElement-value-set! underlying u)
            (SimpleMarketElement-value-set! volatility v)
            (SimpleMarketElement-value-set! q-rate q)
            (SimpleMarketElement-value-set! r-rate r)

            (let ((value (Instrument-NPV option)))
              (if (not (zero? value))
                  (begin
                    ; shift guess somehow
                    (SimpleMarketElement-value-set! volatility (* v 1.5))
                    (let ((implied-vol (VanillaOption-implied-volatility
                                        option
                                        value
                                        tolerance
                                        max-evaluations)))
                      (if (> (abs (- implied-vol v)) tolerance)
                          (begin
                            ; the difference might not matter
                            (SimpleMarketElement-value-set! volatility 
                                                            implied-vol)
                            (check-expected (Instrument-NPV option)
                                            value 1.0e-4
                                            "Option parameters: "
                                            type ", " u ", " strike ", " q ", "
                                            r ", " ex-days ", " v eol
                                            "recalculated NPV:")))))))))))))

(define (European-option-binomial-engine-test)
  (deleting-let* ((underlying (new-SimpleMarketElement 0.0)
                              delete-MarketElement)
                  (volatility (new-SimpleMarketElement 0.0)
                              delete-MarketElement)
                  (q-rate (new-SimpleMarketElement 0.0)
                          delete-MarketElement)
                  (div-curve (eu-test-make-flat-curve q-rate)
                             delete-TermStructureHandle)
                  (r-rate (new-SimpleMarketElement 0.0)
                          delete-MarketElement)
                  (rf-curve (eu-test-make-flat-curve r-rate)
                            delete-TermStructureHandle))
    (let ((tolerance 0.1))
      (for-each-combination ((type '("Call" "Put" "Straddle"))
                             (strike '(50 100 150))
                             (ex-days '(365)))
        (deleting-let* ((option-1 (eu-test-make-option type underlying strike 
                                                       div-curve rf-curve 
                                                       ex-days volatility)
                                  delete-Instrument)
                        (option-2 (eu-test-make-option type underlying strike 
                                                       div-curve rf-curve 
                                                       ex-days volatility 
                                                       "jr")
                                  delete-Instrument)
                        (option-3 (eu-test-make-option type underlying strike 
                                                       div-curve rf-curve 
                                                       ex-days volatility 
                                                       "crr")
                                  delete-Instrument))
          (for-each-combination ((u '(100))
                                 (q '(0.0 0.05))
                                 (r '(0.01 0.05 0.15))
                                 (v '(0.11 0.5 1.2)))

            (SimpleMarketElement-value-set! underlying u)
            (SimpleMarketElement-value-set! volatility v)
            (SimpleMarketElement-value-set! q-rate q)
            (SimpleMarketElement-value-set! r-rate r)

            (let ((value-1 (Instrument-NPV option-1))
                  (value-2 (Instrument-NPV option-2))
                  (value-3 (Instrument-NPV option-3)))
              (assert-equal value-1 value-2 (* u tolerance)
                            "Option parameters: "
                            type ", " u ", " strike ", " q ", "
                            r ", " ex-days ", " v eol
                            "    analytic value: " value-1 eol
                            "    binomial (JR):  " value-2 eol)
              (assert-equal value-1 value-3 (* u tolerance)
                            "Option parameters: "
                            type ", " u ", " strike ", " q ", "
                            r ", " ex-days ", " v eol
                            "    analytic value: " value-1 eol
                            "    binomial (CRR): " value-3 eol))))))))


(define (eu-test-make-option type underlying strike div-curve 
                             rf-curve ex-days volatility . engine-type)
  (deleting-let* ((today (Date-todays-date) delete-Date)
                  (ex-date (Date-plus-days today ex-days) delete-Date)
                  (uh (new-MarketElementHandle underlying)
                      delete-MarketElementHandle)
                  (vh (new-MarketElementHandle volatility)
                      delete-MarketElementHandle)
                  (engine (eu-test-make-engine engine-type) 
                          delete-PricingEngine))
    (new-VanillaOption type uh strike div-curve
                       rf-curve ex-date vh engine)))
(define (eu-test-make-engine engine-type)
  (if (null? engine-type) 
      (new-EuropeanAnalyticEngine)
      (let ((engine-type (car engine-type)))
        (cond ((string=? engine-type "analytic") 
               (new-EuropeanAnalyticEngine))
              ((string=? engine-type "jr") 
               (new-EuropeanBinomialEngine "jr" 800))
              ((string=? engine-type "crr") 
               (new-EuropeanBinomialEngine "crr" 800))))))

(define (eu-test-make-flat-curve forward)
  (deleting-let* ((today (Date-todays-date) delete-Date)
                  (calendar (new-Calendar "TARGET") delete-Calendar)
                  (settlement (Calendar-advance calendar today 2 "days")
                              delete-Date)
                  (day-counter (new-DayCounter "act/360")
                               delete-DayCounter)
                  (fh (new-MarketElementHandle forward)
                      delete-MarketElementHandle)
                  (curve (new-FlatForward settlement fh day-counter)
                         delete-TermStructure))
    (new-TermStructureHandle curve)))

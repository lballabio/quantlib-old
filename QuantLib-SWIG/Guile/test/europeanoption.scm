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

(define (European-option-test tag)
  ; helper functions
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
  (define (make-option type underlying strike div-curve 
                               rf-curve ex-days volatility . engine-type)
    (deleting-let* ((today (Date-todays-date) delete-Date)
                    (ex-date (Date-plus-days today ex-days) delete-Date)
                    (exercise (new-EuropeanExercise ex-date) delete-Exercise)
                    (uh (new-MarketElementHandle underlying)
                        delete-MarketElementHandle)
                    (engine (make-engine engine-type) 
                            delete-PricingEngine))
      (new-VanillaOption type uh strike div-curve
                         rf-curve exercise volatility engine)))
  (define (make-engine engine-type)
    (if (null? engine-type) 
        (new-AnalyticEuropeanEngine)
        (let ((engine-type (car engine-type)))
          (cond ((string=? engine-type "analytic") 
                 (new-AnalyticEuropeanEngine))
                ((member engine-type '("jr""crr" "eqp" "trigeorgis" "tian"))
                 (new-BinomialEuropeanEngine engine-type 800))))))
  (define (make-flat-curve forward)
    (deleting-let* ((today (Date-todays-date) delete-Date)
                    (calendar (new-Calendar "TARGET") delete-Calendar)
                    (settlement (Calendar-advance calendar today 2 "days")
                                delete-Date)
                    (day-counter (new-DayCounter "act/365")
                                 delete-DayCounter)
                    (fh (new-MarketElementHandle forward)
                        delete-MarketElementHandle)
                    (curve (new-FlatForward today settlement fh day-counter)
                           delete-TermStructure))
      (new-TermStructureHandle curve)))
  (define (make-flat-volatility volatility)
    (deleting-let* ((today (Date-todays-date) delete-Date)
                    (calendar (new-Calendar "TARGET") delete-Calendar)
                    (settlement (Calendar-advance calendar today 2 "days")
                                delete-Date)
                    (day-counter (new-DayCounter "act/365")
                                 delete-DayCounter)
                    (vh (new-MarketElementHandle volatility)
                        delete-MarketElementHandle)
                    (curve (new-BlackConstantVol settlement vh day-counter)
                           delete-BlackVolTermStructure))
      (new-BlackVolTermStructureHandle curve)))
  ; setup
  (deleting-let* ((underlying (new-SimpleMarketElement 0.0)
                              delete-MarketElement)
                  (volatility (new-SimpleMarketElement 0.0)
                              delete-MarketElement)
                  (vol-curve (make-flat-volatility volatility)
                             delete-BlackVolTermStructureHandle)
                  (q-rate (new-SimpleMarketElement 0.0)
                          delete-MarketElement)
                  (div-curve (make-flat-curve q-rate)
                             delete-TermStructureHandle)
                  (r-rate (new-SimpleMarketElement 0.0)
                          delete-MarketElement)
                  (rf-curve (make-flat-curve r-rate)
                            delete-TermStructureHandle))
    (cond ((equal? tag 'greek)
           ; test Greeks
           (deleting-let ((calendar (new-Calendar "TARGET") delete-Calendar))
             (let ((tolerance '(("delta" . 1.0e-4)
                                ("gamma" . 1.0e-4)
                                ("theta" . 1.0e-2)
                                ("rho" . 1.0e-4)
                                ("dividend-rho" . 1.0e-4)
                                ("vega" . 1.0e-4))))
               (for-each-combination ((type '("Call" "Put" "Straddle"))
                                      (strike '(50 99.5 100 100.5 150))
                                      (ex-days '(730)))
                 (deleting-let* ((option (make-option type underlying strike 
                                                      div-curve rf-curve 
                                                      ex-days vol-curve)
                                         delete-Instrument)
                                  ; time-shifted exercise dates
                                 (option+ (make-option type underlying strike 
                                                       div-curve rf-curve 
                                                       (+ ex-days 1)
                                                       vol-curve)
                                          delete-Instrument)
                                 (option- (make-option type underlying strike 
                                                       div-curve rf-curve 
                                                       (- ex-days 1)
                                                       vol-curve)
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
                             (add-result expected "delta" 
                                         (VanillaOption-delta option))
                             (add-result expected "gamma" 
                                         (VanillaOption-gamma option))
                             (add-result expected "rho" 
                                         (VanillaOption-rho option))
                             (add-result expected "dividend-rho"
                                         (VanillaOption-dividend-rho option))
                             (add-result expected "vega" 
                                         (VanillaOption-vega option))
                             (add-result expected "theta" 
                                         (VanillaOption-theta option))

                             (add-result calculated "delta"
                                         (derivative Instrument-NPV 
                                                     option underlying))
                             (add-result calculated "gamma"
                                         (derivative VanillaOption-delta 
                                                     option underlying))
                             (add-result calculated "rho" 
                                         (derivative Instrument-NPV 
                                                     option r-rate))
                             (add-result calculated "dividend-rho"
                                         (derivative Instrument-NPV 
                                                     option q-rate))
                             (add-result calculated "vega"
                                         (derivative Instrument-NPV 
                                                     option volatility))
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
                                          type ", " u ", " strike ", " 
                                          q ", " r ", " ex-date ", " v cr
                                          greek ":"))
                                       '("delta" "gamma" "theta" "rho"
                                         "dividend-rho" "vega")))))))))))
          ((equal? tag 'implied-vol)
           ; test implied volatility
           (let ((max-evaluations 100)
                 (tolerance 1.0e-6))
             (for-each-combination ((type '("Call" "Put" "Straddle"))
                                    (strike '(50 99.5 100 100.5 150))
                                    (ex-days '(36 180 360 1080)))
               (deleting-let* ((option (make-option type underlying strike 
                                                    div-curve rf-curve 
                                                    ex-days vol-curve)
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
                           (SimpleMarketElement-value-set! volatility 
                                                           (* v 1.5))
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
                                                   type ", " u ", " 
                                                   strike ", " q ", " r ", " 
                                                   ex-days ", " v cr
                                                   "recalculated NPV:"
                                                   ))))))))))))
          ((equal? tag 'binomial)
           ; test binomial option engines
           (let ((tolerance 0.1))
             (for-each-combination ((type '("Call" "Put" "Straddle"))
                                    (strike '(50 100 150))
                                    (ex-days '(365)))
               (let ((engines '("jr""crr" "eqp" "trigeorgis" "tian")))
                 (deleting-let* ((ref-option (make-option type 
                                                          underlying strike 
                                                          div-curve rf-curve 
                                                          ex-days vol-curve)
                                             delete-Instrument)
                                 (options '()
                                          (lambda (l)
                                            (for-each (lambda (x)
                                                        (delete-Instrument 
                                                         (cdr x)))
                                                      l))))
                   (for-each (lambda (e)
                               (set! options (cons
                                              (cons e
                                                    (make-option type 
                                                                 underlying 
                                                                 strike 
                                                                 div-curve 
                                                                 rf-curve 
                                                                 ex-days 
                                                                 vol-curve 
                                                                 e))
                                              options)))
                             engines)
                   (for-each-combination ((u '(100))
                                          (q '(0.0 0.05))
                                          (r '(0.01 0.05 0.15))
                                          (v '(0.11 0.5 1.2)))

                   (SimpleMarketElement-value-set! underlying u)
                   (SimpleMarketElement-value-set! volatility v)
                   (SimpleMarketElement-value-set! q-rate q)
                   (SimpleMarketElement-value-set! r-rate r)

                   (let ((ref-value (Instrument-NPV ref-option)))
                     (for-each
                      (lambda (entry)
                        (assert-equal ref-value (Instrument-NPV (cdr entry))
                                      (* u tolerance)
                                      "Option parameters: "
                                      type ", " u ", " strike ", " q ", "
                                      r ", " ex-days ", " v cr
                                      "    analytic value: " 
                                      ref-value cr
                                      "    binomial (" (car entry) "):  " 
                                      (Instrument-NPV (cdr entry)) cr))
                      options)))))))))))

(define (European-option-Greek-test)
  (European-option-test 'greek))
(define (European-option-implied-vol-test)
  (European-option-test 'implied-vol))
(define (European-option-binomial-engine-test)
  (European-option-test 'binomial))


; Copyright (C) 2000, 2001, 2002 RiskMap srl
;
; This file is part of QuantLib, a free-software/open-source library
; for financial quantitative analysts and developers - http://quantlib.org/
;
; QuantLib is free software: you can redistribute it and/or modify it under the
; terms of the QuantLib license.  You should have received a copy of the
; license along with this program; if not, please email ferdinando@ametrano.net
; The license is also available online at http://quantlib.org/html/license.html
;
; This program is distributed in the hope that it will be useful, but WITHOUT
; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
; FOR A PARTICULAR PURPOSE.  See the license for more details.
;
; $Id$

(load "common.scm")

(define (OldBarrierPricer-test)
  (let ((max-error-allowed 5e-5)
        (max-straddle-error-allowed 5e-4)
        (underlying-price 100)
        (rebate 3)
        (residual-time 0.5)
        (r-rate 0.08)
        (div-rate 0.04)
        ; this table is from:
        ; "Option pricing formulas", E.G. Haug, McGraw-Hill 1998
        ; pag 72
        (test-data '(; barrType vol strike barrier (   Call .     Put)
                     ("DownOut" 0.25    90      95 ( 9.0246 .  2.2798))
                     ("DownOut" 0.25   100      95 ( 6.7924 .  2.2947))
                     ("DownOut" 0.25   110      95 ( 4.8759 .  2.6252))
                     ("DownOut" 0.25    90     100 ( 3.0000 .  3.0000))
                     ("DownOut" 0.25   100     100 ( 3.0000 .  3.0000))
                     ("DownOut" 0.25   110     100 ( 3.0000 .  3.0000))
                     ("UpOut"   0.25    90     105 ( 2.6789 .  3.7760))
                     ("UpOut"   0.25   100     105 ( 2.3580 .  5.4932))
                     ("UpOut"   0.25   110     105 ( 2.3453 .  7.5187))
                     
                     ("DownIn"  0.25    90      95 ( 7.7627 .  2.9586))
                     ("DownIn"  0.25   100      95 ( 4.0109 .  6.5677))
                     ("DownIn"  0.25   110      95 ( 2.0576 . 11.9752))
                     ("DownIn"  0.25    90     100 (13.8333 .  2.2845))
                     ("DownIn"  0.25   100     100 ( 7.8494 .  5.9085))
                     ("DownIn"  0.25   110     100 ( 3.9795 . 11.6465))
                     ("UpIn"    0.25    90     105 (14.1112 .  1.4653))
                     ("UpIn"    0.25   100     105 ( 8.4482 .  3.3721))
                     ("UpIn"    0.25   110     105 ( 4.5910 .  7.0846))
                     
                     ("DownOut" 0.30    90      95 ( 8.8334 .  2.4170))
                     ("DownOut" 0.30   100      95 ( 7.0285 .  2.4258))
                     ("DownOut" 0.30   110      95 ( 5.4137 .  2.6246))
                     ("DownOut" 0.30    90     100 ( 3.0000 .  3.0000))
                     ("DownOut" 0.30   100     100 ( 3.0000 .  3.0000))
                     ("DownOut" 0.30   110     100 ( 3.0000 .  3.0000))
                     ("UpOut"   0.30    90     105 ( 2.6341 .  4.2293))
                     ("UpOut"   0.30   100     105 ( 2.4389 .  5.8032))
                     ("UpOut"   0.30   110     105 ( 2.4315 .  7.5649))
                     
                     ("DownIn"  0.30    90      95 ( 9.0093 .  3.8769))
                     ("DownIn"  0.30   100      95 ( 5.1370 .  7.7989))
                     ("DownIn"  0.30   110      95 ( 2.8517 . 13.3078))
                     ("DownIn"  0.30    90     100 (14.8816 .  3.3328))
                     ("DownIn"  0.30   100     100 ( 9.2045 .  7.2636))
                     ("DownIn"  0.30   110     100 ( 5.3043 . 12.9713))
                     ("UpIn"    0.30    90     105 (15.2098 .  2.0658))
                     ("UpIn"    0.30   100     105 ( 9.7278 .  4.4226))
                     ("UpIn"    0.30   110     105 ( 5.8350 .  8.3686)))))

    (for-each-case ((barrier-type vol strike barrier results) test-data)
      (deleting-let* ((option (new-BarrierOption barrier-type "Call"
                                                 underlying-price strike
                                                 div-rate r-rate residual-time
                                                 vol barrier rebate)
                              delete-BarrierOption))
        (let ((calculated (BarrierOption-value option))
              (expected (car results)))
          (check-expected calculated expected max-error-allowed
                          barrier-type " Call " strike " " barrier)))
      (deleting-let* ((option (new-BarrierOption barrier-type "Put"
                                                 underlying-price strike
                                                 div-rate r-rate residual-time
                                                 vol barrier rebate)
                              delete-BarrierOption))
        (let ((calculated (BarrierOption-value option))
              (expected (cdr results)))
          (check-expected calculated expected max-error-allowed
                          barrier-type " Put " strike " " barrier)))
      (deleting-let* ((option (new-BarrierOption barrier-type "Straddle"
                                                 underlying-price strike
                                                 div-rate r-rate residual-time
                                                 vol barrier rebate)
                              delete-BarrierOption))
        (let ((calculated (BarrierOption-value option))
              (expected (+ (car results) (cdr results))))
          (check-expected calculated expected max-straddle-error-allowed
                          barrier-type " Straddle " strike " " barrier))))))

(define (OldBinaryPricer-test)
  (define (derivative plus minus method dx)
    (/ (- (method plus) (method minus)) (* 2 dx)))
  (let ((tolerance '(("delta"  . 5e-5)
                     ("gamma"  . 5e-5)
                     ("theta"  . 5e-5)
                     ("rho"    . 5e-5)
                     ("divRho" . 5e-5)
                     ("vega"   . 5e-5))))
    (for-each-combination ((type '("Call" "Put" "Straddle"))
                           (u '(100))                   ; underlying
                           (r '(0.01 0.05 0.15))        ; r rate
                           (q '(0.04 0.05 0.06))        ; q rate
                           (T '(1.0))                   ; residual time
                           (k '(50 99.5 100 100.5 150)) ; strike
                           (v '(0.11 0.5 1.2)))         ; volatility
      (let ((du (/ u 10000))
            (dT (/ T 10000))
            (dv (/ v 10000))
            (dr (/ r 10000))
            (dq (/ q 10000)))
        (deleting-let* ((option (new-BinaryOption type u k q r T v)
                                delete-BinaryOption))
          (if (> (BinaryOption-value option) 1.0e-6)
              (deleting-let ((optPs (new-BinaryOption type (+ u du) k q r T v)
                                    delete-BinaryOption)
                             (optMs (new-BinaryOption type (- u du) k q r T v)
                                    delete-BinaryOption)
                             (optPt (new-BinaryOption type u k q r (+ T dT) v)
                                    delete-BinaryOption)
                             (optMt (new-BinaryOption type u k q r (- T dT) v)
                                    delete-BinaryOption)
                             (optPr (new-BinaryOption type u k q (+ r dr) T v)
                                    delete-BinaryOption)
                             (optMr (new-BinaryOption type u k q (- r dr) T v)
                                    delete-BinaryOption)
                             (optPq (new-BinaryOption type u k (+ q dq) r T v)
                                    delete-BinaryOption)
                             (optMq (new-BinaryOption type u k (- q dq) r T v)
                                    delete-BinaryOption)
                             (optPv (new-BinaryOption type u k q r T (+ v dv))
                                    delete-BinaryOption)
                             (optMv (new-BinaryOption type u k q r T (- v dv))
                                    delete-BinaryOption))
                (let ((calculated
                       (list
                        (cons "delta"
                              (derivative optPs optMs BinaryOption-value du))
                        (cons "gamma"
                              (derivative optPs optMs BinaryOption-delta du))
                        (cons "theta"
                              (derivative optMt optPt BinaryOption-value dT))
                        (cons "rho"
                              (derivative optPr optMr BinaryOption-value dr))
                        (cons "divRho"
                              (derivative optPq optMq BinaryOption-value dq))
                        (cons "vega"
                              (derivative optPv optMv BinaryOption-value dv))))
                      (expected
                       (list
                        (cons "delta" (BinaryOption-delta option))
                        (cons "gamma" (BinaryOption-gamma option))
                        (cons "theta" (BinaryOption-theta option))
                        (cons "rho" (BinaryOption-rho option))
                        (cons "divRho" (BinaryOption-dividend-rho option))
                        (cons "vega" (BinaryOption-vega option)))))
                  (for-each (lambda (greek)
                              (check-expected
                               (cdr (assoc greek calculated))
                               (cdr (assoc greek expected))
                               (* (cdr (assoc greek tolerance)) u)
                               "Option parameters: "
                               type ", " u ", " k ", " 
                               q ", " r ", " t ", " v cr
                               greek ":"))
                            '("delta" "gamma" "theta" "rho"
                              "divRho" "vega"))))))))))

(define (OldCliquetPricer-test)
  (let ((spot 60)
        (moneyness 1.1)
        (div-yield '(0.04 0.04))
        (r-rate '(0.08 0.08))
        (dates '(0.25 1.00))
        (vol '(0.30 0.30)))
    (deleting-let ((option (new-CliquetOption "Call" spot moneyness
                                              div-yield r-rate dates vol)
                           delete-CliquetOption))
      ; Haug, pag 37
      (let ((stored-value 4.4064))
        (check-expected (CliquetOption-value option)
                        stored-value
                        1.0e-4
                        "failed to reproduce cached value")))))
            
(define (OldDividendEuropeanPricer-test)
  (define (derivative plus minus method dx)
    (/ (- (method plus) (method minus)) (* 2 dx)))
  (let* ((n-steps 150)
         (n-grid (+ n-steps 1))
         (dividends '(3.92 4.21))
         (times '(0.333 0.667))
         (tolerance '(("delta"  . 1e-4)
                      ("gamma"  . 1e-4)
                      ("theta"  . 1e-4)
                      ("rho"    . 1e-4)
                      ("vega"   . 1e-4))))
    (for-each-combination ((type '("Call" "Put" "Straddle"))
                           (u '(100))                   ; underlying
                           (r '(0.01 0.1 0.3))          ; r rate
                           (q '(0.0 0.05 0.15))         ; q rate
                           (T '(1.0 2.0))               ; residual time
                           (k '(50 99.5 100 100.5 150)) ; strike
                           (v '(0.04 0.2 0.7)))         ; volatility
      (let ((du (/ u 10000))
            (dT (/ T n-steps))
            (dv (/ v 10000))
            (dr (/ r 10000))
            (make new-FdDividendEuropeanOption)
            (delete delete-FdDividendEuropeanOption))
        (deleting-let* ((option (make type u k q r T v dividends times)
                                delete))
          (if (> (FdDividendEuropeanOption-value option) (* u 1.0e-5))
              (deleting-let ((optPs (make type (+ u du) k q r T v
                                          dividends times)
                                    delete)
                             (optMs (make type (- u du) k q r T v
                                          dividends times)
                                    delete)
                             (optPt (make type u k q r (+ T dT) v
                                          dividends
                                          (map (lambda (t) (+ t dT)) times))
                                    delete)
                             (optMt (make type u k q r (- T dT) v
                                          dividends
                                          (map (lambda (t) (- t dT)) times))
                                    delete)
                             (optPr (make type u k q (+ r dr) T v
                                          dividends times)
                                    delete)
                             (optMr (make type u k q (- r dr) T v
                                          dividends times)
                                    delete)
                             (optPv (make type u k q r T (+ v dv)
                                          dividends times)
                                    delete)
                             (optMv (make type u k q r T (- v dv)
                                          dividends times)
                                    delete))
                (let ((calculated
                       (list
                        (cons "delta"
                              (derivative optPs optMs 
                                          FdDividendEuropeanOption-value du))
                        (cons "gamma"
                              (derivative optPs optMs 
                                          FdDividendEuropeanOption-delta du))
                        (cons "theta"
                              (derivative optMt optPt 
                                          FdDividendEuropeanOption-value dT))
                        (cons "rho"
                              (derivative optPr optMr 
                                          FdDividendEuropeanOption-value dr))
                        (cons "vega"
                              (derivative optPv optMv 
                                          FdDividendEuropeanOption-value dv))))
                      (expected
                       (list
                        (cons "delta" (FdDividendEuropeanOption-delta option))
                        (cons "gamma" (FdDividendEuropeanOption-gamma option))
                        (cons "theta" (FdDividendEuropeanOption-theta option))
                        (cons "rho" (FdDividendEuropeanOption-rho option))
                        (cons "vega" (FdDividendEuropeanOption-vega option)))))
                  (for-each (lambda (greek)
                              (check-expected
                               (cdr (assoc greek calculated))
                               (cdr (assoc greek expected))
                               (* (cdr (assoc greek tolerance)) u)
                               "Option parameters: "
                               type ", " u ", " k ", " 
                               q ", " r ", " t ", " v cr
                               greek ":"))
                            '("delta" "gamma" "theta" "rho" "vega"))))))))))
  
(define (OldFdEuropeanPricer-test)
  (define (next rng)
    (deleting-let ((sample (UniformRandomGenerator-next rng) 
                           delete-SampleNumber))
      (SampleNumber-value sample)))
  (let ((u 100)
        (strike-min 60)
        (strike-range 100)
        (r-rate-range 0.18)
        (q-rate-range 0.02)
        (vol-range 1.2)
        (time-min 0.5)
        (time-range 2.0)
        (tolerance 1.0e-2)
        (total-cases 200))
    (deleting-let ((rng (new-UniformRandomGenerator 56789012)
                        delete-UniformRandomGenerator))
      (repeat (total-cases)
        (let ((k (+ strike-min (* strike-range (next rng))))
              (q (* q-rate-range (next rng)))
              (r (* r-rate-range (next rng)))
              (v (* vol-range (next rng)))
              (t (+ time-min (* time-range (next rng)))))
          (for-each-combination ((type '("Call" "Put" "Straddle")))
            (deleting-let ((analytic (new-EuropeanOption type u k q r t v)
                                     delete-EuropeanOption)
                           (fin-diff (new-FdEuropean type u k q r t v 100 400)
                                     delete-FdEuropean))
              (check-expected (EuropeanOption-value analytic)
                              (FdEuropean-value fin-diff)
                              tolerance
                              "Option details: " type " " u " " k
                              " " q " " r " " t " " v))))))))

(define (OldAmericanPricers-test)
  (define (derivative plus minus method dx)
    (/ (- (method plus) (method minus)) (* 2 dx)))
  (let* ((n-steps 145)
         (n-grid (+ n-steps 1))
         (tolerance '(("delta"  . 2e-3)
                      ("gamma"  . 2e-3)
                      ("theta"  . 2e-3)
                      ("rho"    . 2e-3)
                      ("divRho" . 2e-3)
                      ("vega"   . 2e-3))))
    (for-each-combination ((funcs (list (list new-FdAmericanOption
                                              delete-FdAmericanOption
                                              FdAmericanOption-value
                                              FdAmericanOption-delta
                                              FdAmericanOption-gamma
                                              FdAmericanOption-theta
                                              FdAmericanOption-rho
                                              FdAmericanOption-dividend-rho
                                              FdAmericanOption-vega)
                                        (list new-FdShoutOption
                                              delete-FdShoutOption
                                              FdShoutOption-value
                                              FdShoutOption-delta
                                              FdShoutOption-gamma
                                              FdShoutOption-theta
                                              FdShoutOption-rho
                                              FdShoutOption-dividend-rho
                                              FdShoutOption-vega)))
                           (type '("Call" "Put" "Straddle"))
                           (u '(100))                   ; underlying
                           (r '(0.01 0.05 0.15))        ; r rate
                           (q '(0.04 0.05 0.06))        ; q rate
                           (T '(1.0))                   ; residual time
                           (k '(50 100 150))            ; strike
                           (v '(0.05 0.5 1.2)))         ; volatility
      (let ((du (/ u 10000))
            (dv (/ v 10000))
            (dr (/ r 10000))
            (dq (/ q 10000)))
        (let-at-once ((make delete value delta gamma 
                            theta rho dividend-rho vega) funcs)
          (deleting-let ((option (make type u k q r T v n-steps n-grid) 
                                 delete))
          (if (> (value option) (* u 1e-5))
              (deleting-let ((optPs (make type (+ u du) k q r T v 
                                          n-steps n-grid)
                                    delete)
                             (optMs (make type (- u du) k q r T v 
                                          n-steps n-grid)
                                    delete)
                             (optPr (make type u k q (+ r dr) T v 
                                          n-steps n-grid)
                                    delete)
                             (optMr (make type u k q (- r dr) T v 
                                          n-steps n-grid)
                                    delete)
                             (optPq (make type u k (+ q dq) r T v 
                                          n-steps n-grid)
                                    delete)
                             (optMq (make type u k (- q dq) r T v 
                                          n-steps n-grid)
                                    delete)
                             (optPv (make type u k q r T (+ v dv) 
                                          n-steps n-grid)
                                    delete)
                             (optMv (make type u k q r T (- v dv) 
                                          n-steps n-grid)
                                    delete))
                (let ((expected
                       (list
                        (cons "delta"  (derivative optPs optMs value du))
                        (cons "gamma"  (derivative optPs optMs delta du))
                        (cons "theta"  (- (* r (value option))
                                          (* (- r q) u (delta option))
                                          (* 0.5 v v u u (gamma option))))
                        (cons "rho"    (derivative optPr optMr value dr))
                        (cons "divRho" (derivative optPq optMq value dq))
                        (cons "vega"   (derivative optPv optMv value dv))))
                      (calculated
                       (list
                        (cons "delta"  (delta option))
                        (cons "gamma"  (gamma option))
                        (cons "theta"  (theta option))
                        (cons "rho"    (rho option))
                        (cons "divRho" (dividend-rho option))
                        (cons "vega"   (vega option)))))
                  (for-each (lambda (greek)
                              (check-expected
                               (cdr (assoc greek calculated))
                               (cdr (assoc greek expected))
                               (* (cdr (assoc greek tolerance)) u)
                               "Option parameters: "
                               type ", " u ", " k ", " 
                               q ", " r ", " t ", " v cr
                               greek ":"))
                            '("delta" "gamma" "theta" 
                              "rho" "divRho" "vega")))))))))))

(define (OldMcSingleFactorPricers-test)
  (let ((seed 3456789)
        (fixedSamples 100)
        (minimumTol 0.01))
    (let ((methods (list 
                    (list 'EuropeanOption
                          new-EuropeanOption
                          delete-EuropeanOption
                          EuropeanOption-value
                          #f #f)
                    (list 'DiscreteGeometricAPO
                          new-DiscreteGeometricAPO
                          delete-DiscreteGeometricAPO
                          DiscreteGeometricAPO-value
                          #f #f)
                    (list 'ContinuousGeometricAPO
                          new-ContinuousGeometricAPO
                          delete-ContinuousGeometricAPO
                          ContinuousGeometricAPO-value
                          #f #f)
                    (list 'McEuropean
                          new-McEuropean
                          delete-McEuropean
                          McEuropean-value
                          McEuropean-value-with-samples
                          McEuropean-error-estimate)
                    (list 'McDiscreteArithmeticAPO
                          new-McDiscreteArithmeticAPO
                          delete-McDiscreteArithmeticAPO
                          McDiscreteArithmeticAPO-value
                          McDiscreteArithmeticAPO-value-with-samples
                          McDiscreteArithmeticAPO-error-estimate)
                    (list 'McDiscreteArithmeticASO
                          new-McDiscreteArithmeticASO
                          delete-McDiscreteArithmeticASO
                          McDiscreteArithmeticASO-value
                          McDiscreteArithmeticASO-value-with-samples
                          McDiscreteArithmeticASO-error-estimate))))

      ; data from "Implementing Derivatives Model",
      ; Clewlow, Strickland, pag.118-123
      (let ((cases 
             '((DiscreteGeometricAPO "Call" 100.0 100.0 0.03 0.06
                                     (0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0)
                                     0.2 5.34255485619))))
        (for-each-case ((tag optionType underlying strike
                             dividendYield riskFreeRate timeIncrements
                             volatility storedValue) cases)
          (let-at-once ((_ make delete value __ ___) (assoc tag methods))
            (deleting-let ((p (make optionType underlying strike dividendYield 
                                    riskFreeRate timeIncrements volatility)
                              delete))
              (check-expected (value p) storedValue 1.0e-10
                              "In batch 1:")))))

      ; data from "Option Pricing Formulas", Haug, pag.96-97
      (let ((cases
             '((EuropeanOption "Put" 80.0 85.0 -0.03 
                               0.05 0.25 0.2 5.21858890396)
               (ContinuousGeometricAPO  "Put" 80.0 85.0 -0.03
                                        0.05 0.25 0.2 4.69221973405))))
        (for-each-case ((tag optionType underlying strike
                             dividendYield riskFreeRate residualTime
                             volatility storedValue) cases)
          (let-at-once ((_ make delete value __ ___) (assoc tag methods))
            (deleting-let ((p (make optionType underlying strike dividendYield
                                    riskFreeRate residualTime volatility)
                              delete))
              (check-expected (value p) storedValue 1.0e-10
                              "In batch 2:")))))

      ; trying to approximate the continous version with the discrete version
      (let ((cases
             '((DiscreteGeometricAPO "Put" 80.0 85.0 -0.03
                                     0.05 0.25 90000 0.2 4.6922231469))))
        (for-each-case ((tag optionType underlying strike dividendYield
                             riskFreeRate residualTime timesteps
                             volatility storedValue) cases)
          (let-at-once ((_ make delete value __ ___) (assoc tag methods))
            (let* ((dt (/ residualTime timesteps))
                   (timeIncrements (map (lambda (i) (* (+ i 1) dt))
                                        (range 0 timesteps))))
              (deleting-let ((p (make optionType underlying strike
                                      dividendYield riskFreeRate
                                      timeIncrements volatility)
                                delete))
                (check-expected (value p) storedValue 1.0e-10
                                "In batch 3:"))))))

      (let ((cases
             '((McEuropean "Put" 80.0 85.0 -0.03 0.05
                           0.25 0.2 5.9135872358 #f)
               (McEuropean "Put" 80.0 85.0 -0.03 0.05
                           0.25 0.2 5.42005964479 #t)
               (McEuropean "Call" 80.0 85.0 -0.03 0.05 
                           0.25 0.2 1.98816310759 #f)
               (McEuropean "Call" 80.0 85.0 -0.03 0.05 
                           0.25 0.2 2.12098432917 #t)
               (McEuropean "Straddle" 80.0 85.0 -0.03 0.05 
                           0.25 0.2 7.90175034339 #f)
               (McEuropean "Straddle" 80.0 85.0 -0.03 0.05
                           0.25 0.2 7.54104397396 #t))))
        (for-each-case ((tag optionType underlying strike
                             dividendYield riskFreeRate residualTime
                             volatility storedValue antithetic) cases)
          (let-at-once ((_ make delete value value/samples error-estimate) 
                        (assoc tag methods))
            (deleting-let ((p (make optionType underlying strike dividendYield
                                    riskFreeRate residualTime volatility
                                    antithetic seed)
                              delete))
              (let* ((pvalue (value/samples p fixedSamples))
                     (tol (min (/ (error-estimate p) pvalue 2) minimumTol))
                     (pvalue2 (value p tol))
                     (accuracy (/ (error-estimate p) pvalue2)))
                (check-expected pvalue storedValue 1.0e-10
                                "In batch 4: ")
                (assert (<= accuracy tol)
                        "In batch 4:" cr
                        "    accuracy reached    : " accuracy cr
                        "    tolerance requested : " tol cr))))))
    
      ; data from "Asian Option", Levy, 1997
      ; in "Exotic Options: The State of the Art",
      ; edited by Clewlow, Strickland
      (let ((cases
             '((McDiscreteArithmeticAPO "Put" 90.0 87.0 0.06 0.025
                                        0.0 11/12 2 0.13
                                        1.38418414762 #t #t)
               (McDiscreteArithmeticAPO "Put" 90.0 87.0 0.06 0.025
                                        0.0 11/12 4 0.13
                                        1.57691714387 #t #t)
               (McDiscreteArithmeticAPO "Put" 90.0 87.0 0.06 0.025
                                        0.0 11/12 8 0.13
                                        1.66062743445 #t #t)
               (McDiscreteArithmeticAPO "Put" 90.0 87.0 0.06 0.025
                                        0.0 11/12 12 0.13
                                        1.68847081883 #t #t)
               (McDiscreteArithmeticAPO "Put" 90.0 87.0 0.06 0.025
                                        0.0 11/12 26 0.13
                                        1.72955964448 #t #t)
               (McDiscreteArithmeticAPO "Put" 90.0 87.0 0.06 0.025
                                        0.0 11/12 52 0.13
                                        1.73372169316 #t #t)
               (McDiscreteArithmeticAPO "Put" 90.0 87.0 0.06 0.025
                                        0.0 11/12 100 0.13
                                        1.74918801089 #t #t)
               (McDiscreteArithmeticAPO "Put" 90.0 87.0 0.06 0.025
                                        0.0 11/12 250 0.13
                                        1.75421310915 #t #t)
               (McDiscreteArithmeticAPO "Put" 90.0 87.0 0.06 0.025
                                        0.0 11/12 500 0.13
                                        1.75158383443 #t #t)
               (McDiscreteArithmeticAPO "Put" 90.0 87.0 0.06 0.025
                                        0.0 11/12 1000 0.13
                                        1.75162110180 #t #t)
               (McDiscreteArithmeticAPO "Put" 90.0 87.0 0.06 0.025
                                        1/12 11/12 2 0.13
                                        1.83665087164 #t #t)
               (McDiscreteArithmeticAPO "Put" 90.0 87.0 0.06 0.025
                                        1/12 11/12 4 0.13
                                        2.00560271429 #t #t)
               (McDiscreteArithmeticAPO "Put" 90.0 87.0 0.06 0.025
                                        1/12 11/12 8 0.13
                                        2.07789721712 #t #t)
               (McDiscreteArithmeticAPO "Put" 90.0 87.0 0.06 0.025
                                        1/12 11/12 12 0.13
                                        2.09622556625 #t #t)
               (McDiscreteArithmeticAPO "Put" 90.0 87.0 0.06 0.025
                                        1/12 11/12 26 0.13
                                        2.14229795212 #t #t)
               (McDiscreteArithmeticAPO "Put" 90.0 87.0 0.06 0.025
                                        1/12 11/12 52 0.13
                                        2.14470270916 #t #t)
               (McDiscreteArithmeticAPO "Put" 90.0 87.0 0.06 0.025
                                        1/12 11/12 100 0.13
                                        2.1595414574 #t #t)
               (McDiscreteArithmeticAPO "Put" 90.0 87.0 0.06 0.025
                                        1/12 11/12 250 0.13
                                        2.1600769002 #t #t)
               (McDiscreteArithmeticAPO "Put" 90.0 87.0 0.06 0.025
                                        1/12 11/12 500 0.13
                                        2.1598670440 #t #t)
               (McDiscreteArithmeticAPO "Put" 90.0 87.0 0.06 0.025
                                        1/12 11/12 1000 0.13
                                        2.1595163439 #t #t)
               (McDiscreteArithmeticAPO "Put" 90.0 87.0 0.06 0.025
                                        3/12 11/12 2 0.13
                                        2.63315092584 #t #t)
               (McDiscreteArithmeticAPO "Put" 90.0 87.0 0.06 0.025
                                        3/12 11/12 4 0.13
                                        2.76723962361 #t #t)
               (McDiscreteArithmeticAPO "Put" 90.0 87.0 0.06 0.025
                                        3/12 11/12 8 0.13
                                        2.83124836881 #t #t)
               (McDiscreteArithmeticAPO "Put" 90.0 87.0 0.06 0.025
                                        3/12 11/12 12 0.13
                                        2.84290301412 #t #t)
               (McDiscreteArithmeticAPO "Put" 90.0 87.0 0.06 0.025
                                        3/12 11/12 26 0.13
                                        2.88179560417 #t #t)
               (McDiscreteArithmeticAPO "Put" 90.0 87.0 0.06 0.025
                                        3/12 11/12 52 0.13
                                        2.88447044543 #t #t)
               (McDiscreteArithmeticAPO "Put" 90.0 87.0 0.06 0.025
                                        3/12 11/12 100 0.13
                                        2.8998532960 #t #t)
               (McDiscreteArithmeticAPO "Put" 90.0 87.0 0.06 0.025
                                        3/12 11/12 250 0.13
                                        2.9004729606 #t #t)
               (McDiscreteArithmeticAPO "Put" 90.0 87.0 0.06 0.025
                                        3/12 11/12 500 0.13
                                        2.8981341216 #t #t)
               (McDiscreteArithmeticAPO "Put" 90.0 87.0 0.06 0.025
                                        3/12 11/12 1000 0.13
                                        2.8970336244 #t #t))))
        (for-each-case ((tag optionType underlying strike
                             dividendYield riskFreeRate first length
                             fixings volatility storedValue 
                             antithetic controlVariate) cases)
          (let* ((dt (/ length (- fixings 1)))
                 (timeIncrements (map (lambda (i) (+ (* i dt) first))
                                      (range 0 fixings))))
            (let-at-once ((_ make delete value value/samples error-estimate) 
                          (assoc tag methods))
              (deleting-let ((p (make optionType underlying strike 
                                      dividendYield riskFreeRate 
                                      timeIncrements volatility
                                      antithetic controlVariate seed)
                                delete))
                (let* ((pvalue (value/samples p fixedSamples))
                       (tol (min (/ (error-estimate p) pvalue 2) minimumTol))
                       (pvalue2 (value p tol))
                       (accuracy (/ (error-estimate p) pvalue2)))
                  (check-expected pvalue storedValue 1.0e-10
                                  "In batch 5: ")
                  (assert (<= accuracy tol)
                          "In batch 5:" cr
                          "    accuracy reached    : " accuracy cr
                          "    tolerance requested : " tol cr)))))))


      ; data from "Asian Option", Levy, 1997
      ; in "Exotic Options: The State of the Art",
      ; edited by Clewlow, Strickland
      (let ((cases
             '((McDiscreteArithmeticASO "Call" 90.0 87.0 0.06 0.025
                                        0.0 11/12    2 0.13
                                        1.51917595129 #t #t)
               (McDiscreteArithmeticASO "Call" 90.0 87.0 0.06 0.025
                                        0.0 11/12    4 0.13
                                        1.67940165674 #t #t)
               (McDiscreteArithmeticASO "Call" 90.0 87.0 0.06 0.025
                                        0.0 11/12    8 0.13
                                        1.75371215251 #t #t)
               (McDiscreteArithmeticASO "Call" 90.0 87.0 0.06 0.025
                                        0.0 11/12   12 0.13
                                        1.77595318693 #t #t)
               (McDiscreteArithmeticASO "Call" 90.0 87.0 0.06 0.025
                                        0.0 11/12   26 0.13
                                        1.81430536630 #t #t)
               (McDiscreteArithmeticASO "Call" 90.0 87.0 0.06 0.025
                                        0.0 11/12   52 0.13
                                        1.82269246898 #t #t)
               (McDiscreteArithmeticASO "Call" 90.0 87.0 0.06 0.025
                                        0.0 11/12  100 0.13
                                        1.83822402464 #t #t)
               (McDiscreteArithmeticASO "Call" 90.0 87.0 0.06 0.025
                                        0.0 11/12  250 0.13
                                        1.83875059026 #t #t)
               (McDiscreteArithmeticASO "Call" 90.0 87.0 0.06 0.025
                                        0.0 11/12  500 0.13
                                        1.83750703638 #t #t)
               (McDiscreteArithmeticASO "Call" 90.0 87.0 0.06 0.025
                                        0.0 11/12 1000 0.13
                                        1.83887181884 #t #t)
               (McDiscreteArithmeticASO "Call" 90.0 87.0 0.06 0.025
                                        1/12 11/12 2 0.13
                                        1.51154400089 #t #t)
               (McDiscreteArithmeticASO "Call" 90.0 87.0 0.06 0.025
                                        1/12 11/12 4 0.13
                                        1.67103508506 #t #t)
               (McDiscreteArithmeticASO "Call" 90.0 87.0 0.06 0.025
                                        1/12 11/12 8 0.13
                                        1.74529684070 #t #t)
               (McDiscreteArithmeticASO "Call" 90.0 87.0 0.06 0.025
                                        1/12 11/12 12 0.13
                                        1.76667074564 #t #t)
               (McDiscreteArithmeticASO "Call" 90.0 87.0 0.06 0.025
                                        1/12 11/12 26 0.13
                                        1.80528400613 #t #t)
               (McDiscreteArithmeticASO "Call" 90.0 87.0 0.06 0.025
                                        1/12 11/12 52 0.13
                                        1.81400883891 #t #t)
               (McDiscreteArithmeticASO "Call" 90.0 87.0 0.06 0.025
                                        1/12 11/12 100 0.13
                                        1.8292290145 #t #t)
               (McDiscreteArithmeticASO "Call" 90.0 87.0 0.06 0.025
                                        1/12 11/12 250 0.13
                                        1.8293711177 #t #t)
               (McDiscreteArithmeticASO "Call" 90.0 87.0 0.06 0.025
                                        1/12 11/12 500 0.13
                                        1.8282619319 #t #t)
               (McDiscreteArithmeticASO "Call" 90.0 87.0 0.06 0.025
                                        1/12 11/12 1000 0.13
                                        1.8296784665 #t #t)
               (McDiscreteArithmeticASO "Call" 90.0 87.0 0.06 0.025
                                        3/12 11/12 2 0.13
                                        1.49648170891 #t #t)
               (McDiscreteArithmeticASO "Call" 90.0 87.0 0.06 0.025
                                        3/12 11/12 4 0.13
                                        1.65443100462 #t #t)
               (McDiscreteArithmeticASO "Call" 90.0 87.0 0.06 0.025
                                        3/12 11/12 8 0.13
                                        1.72817806731 #t #t)
               (McDiscreteArithmeticASO "Call" 90.0 87.0 0.06 0.025
                                        3/12 11/12 12 0.13
                                        1.74877367895 #t #t)
               (McDiscreteArithmeticASO "Call" 90.0 87.0 0.06 0.025
                                        3/12 11/12 26 0.13
                                        1.78733801988 #t #t)
               (McDiscreteArithmeticASO "Call" 90.0 87.0 0.06 0.025
                                        3/12 11/12 52 0.13
                                        1.79624826757 #t #t)
               (McDiscreteArithmeticASO "Call" 90.0 87.0 0.06 0.025
                                        3/12 11/12 100 0.13
                                        1.8111418688 #t #t)
               (McDiscreteArithmeticASO "Call" 90.0 87.0 0.06 0.025
                                        3/12 11/12 250 0.13
                                        1.8110115259 #t #t)
               (McDiscreteArithmeticASO "Call" 90.0 87.0 0.06 0.025
                                        3/12 11/12 500 0.13
                                        1.8100231194 #t #t)
               (McDiscreteArithmeticASO "Call" 90.0 87.0 0.06 0.025
                                        3/12 11/12 1000 0.13
                                        1.8114576031 #t #t))))
        (for-each-case ((tag optionType underlying strike dividendYield
                         riskFreeRate first length fixings volatility
                         storedValue antithetic controlVariate) cases)
          (let* ((dt (/ length (- fixings 1)))
                 (timeIncrements (map (lambda (i) (+ (* i dt) first))
                                      (range 0 fixings))))
            (let-at-once ((_ make delete value value/samples error-estimate) 
                          (assoc tag methods))
              (deleting-let ((p (make optionType underlying dividendYield 
                                      riskFreeRate timeIncrements volatility
                                      antithetic controlVariate seed)
                                delete))
                (let* ((pvalue (value/samples p fixedSamples))
                       (tol (min (/ (error-estimate p) pvalue 2) minimumTol))
                       (pvalue2 (value p tol))
                       (accuracy (/ (error-estimate p) pvalue2)))
                  (check-expected pvalue storedValue 1.0e-10
                                  "In batch 6: ")
                  (assert (<= accuracy tol)
                          "In batch 6:" cr
                          "    accuracy reached    : " accuracy cr
                          "    tolerance requested : " tol cr))))))))))

(define (OldMcMultiFactorPricers-test)
  (let* ((cor #(#(1.00 0.50 0.30 0.10)
                #(0.50 1.00 0.20 0.40)
                #(0.30 0.20 1.00 0.60)
                #(0.10 0.40 0.60 1.00)))
         (volatilities #(0.30 0.35 0.25 0.20))
         (cov (covariance volatilities cor))
         (dividendYields #(0.01 0.05 0.04 0.03))
         (riskFreeRate 0.05)
         (resTime 1.0)
         ; degenerate portfolio
         (perfectCorrelation #(#(1.00 1.00 1.00 1.00)
                               #(1.00 1.00 1.00 1.00)
                               #(1.00 1.00 1.00 1.00)
                               #(1.00 1.00 1.00 1.00)))
         (sameAssetVols #(0.30 0.30 0.30 0.30))
         (sameAssetCovariance (covariance sameAssetVols
                                          perfectCorrelation))
         (sameAssetDividend #(0.03 0.03 0.03 0.03))
         (seed 86421)
         (fixedSamples 100)
         (minimumTol 0.01))
    
    ; McEverest
    (for-each-case ((antithetic? storedValue) '((#f 0.743448)
                                                (#t 0.756979)))
      (deleting-let ((p (new-McEverest dividendYields cov riskFreeRate
                                       resTime antithetic? seed)
                        delete-McEverest))
        (let* ((pvalue (McEverest-value-with-samples p fixedSamples))
               (tol (min (/ (McEverest-error-estimate p) pvalue 2) minimumTol))
               (pvalue2 (McEverest-value p tol))
               (accuracy (/ (McEverest-error-estimate p) pvalue2)))
          (check-expected pvalue storedValue 1e-5
                          "McEverest:")
          (assert (<= accuracy tol)
                  "McEverest:" cr
                  "    accuracy reached    : " accuracy cr
                  "    tolerance requested : " tol cr))))

    ; McBasket
    (let ((sameAssetValues #(25 25 25 25))
          (type "Call")
          (strike 100))
      (for-each-case ((antithetic? storedValue) '((#f 10.448445)
                                                  (#t 12.294677)))
        (deleting-let ((p (new-McBasket type sameAssetValues strike
                                        sameAssetDividend sameAssetCovariance
                                        riskFreeRate resTime antithetic? seed)
                          delete-McBasket))
          (let* ((pvalue (McBasket-value-with-samples p fixedSamples))
                 (tol (min (/ (McBasket-error-estimate p) pvalue 2) 
                           minimumTol))
                 (pvalue2 (McBasket-value p tol))
                 (accuracy (/ (McBasket-error-estimate p) pvalue2)))
          (check-expected pvalue storedValue 1e-5
                          "McBasket:")
          (assert (<= accuracy tol)
                  "McBasket:" cr
                  "    accuracy reached    : " accuracy cr
                  "    tolerance requested : " tol cr)))))

    ; McMaxBasket
    (let ((assetValues #(100 110 90 105)))
      (for-each-case ((antithetic? storedValue) '((#f 120.733780)
                                                  (#t 123.520909)))
        (deleting-let ((p (new-McMaxBasket assetValues dividendYields cov
                                           riskFreeRate resTime 
                                           antithetic? seed)
                          delete-McMaxBasket))
          (let* ((pvalue (McMaxBasket-value-with-samples p fixedSamples))
                 (tol (min (/ (McMaxBasket-error-estimate p) pvalue 2) 
                           minimumTol))
                 (pvalue2 (McMaxBasket-value p tol))
                 (accuracy (/ (McMaxBasket-error-estimate p) pvalue2)))
          (check-expected pvalue storedValue 1e-5
                          "McMaxBasket:")
          (assert (<= accuracy tol)
                  "McMaxBasket:" cr
                  "    accuracy reached    : " accuracy cr
                  "    tolerance requested : " tol cr)))))

    ; McPagoda
    (let ((portfolio #(0.15 0.20 0.35 0.30))
          (fraction 0.62)
          (roof 0.20)
          (timeIncrements '(0.25 0.5 0.75 1)))
      (for-each-case ((antithetic? storedValue) '((#f 0.0343898)
                                                  (#t 0.0386095)))
        (deleting-let ((p (new-McPagoda portfolio fraction roof
                                        dividendYields cov riskFreeRate
                                        timeIncrements antithetic? seed)
                          delete-McPagoda))
          (let* ((pvalue (McPagoda-value-with-samples p fixedSamples))
                 (tol (min (/ (McPagoda-error-estimate p) pvalue 2)
                           minimumTol))
                 (pvalue2 (McPagoda-value p tol))
                 (accuracy (/ (McPagoda-error-estimate p) pvalue2)))
          (check-expected pvalue storedValue 1e-5
                          "McPagoda:")
          (assert (<= accuracy tol)
                  "McPagoda:" cr
                  "    accuracy reached    : " accuracy cr
                  "    tolerance requested : " tol cr)))))

    ; McHimalaya
    (let ((assetValues #(100 110 90 105))
          (timeIncrements '(0.25 0.5 0.75 1))
          (strike 101))
      (for-each-case ((antithetic? storedValue) '((#f 5.0768499)
                                                  (#t 6.2478050)))
        (deleting-let ((p (new-McHimalaya assetValues dividendYields cov
                                          riskFreeRate strike timeIncrements
                                          antithetic? seed)
                          delete-McHimalaya))
          (let* ((pvalue (McHimalaya-value-with-samples p fixedSamples))
                 (tol (min (/ (McHimalaya-error-estimate p) pvalue 2)
                           minimumTol))
                 (pvalue2 (McHimalaya-value p tol))
                 (accuracy (/ (McHimalaya-error-estimate p) pvalue2)))
          (check-expected pvalue storedValue 1e-5
                          "McHimalaya:")
          (assert (<= accuracy tol)
                  "McHimalaya:" cr
                  "    accuracy reached    : " accuracy cr
                  "    tolerance requested : " tol cr)))))))


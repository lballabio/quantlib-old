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


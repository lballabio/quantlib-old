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

(load "unittest.scm")
(load "common.scm")

(define (Risk-statistics-test)
  (define (test-case stats average sigma)
    (define pi (acos -1.0))
    (define (gaussian x)
      (let ((dx (- x average)))
        (/ (exp (/ (- (* dx dx)) (* 2.0 sigma sigma)))
           (* sigma (sqrt (* 2.0 pi))))))
    (let* ((N 25000)
           (sigmas 15)
           (target average)
           (normal (new-NormalDistribution average sigma))
           (data-min (- average (* sigmas sigma)))
           (data-max (+ average (* sigmas sigma)))
           (h (grid-step data-min data-max N))
           (data (grid data-min data-max N))
           (weights (map gaussian data)))
      (Statistics-add stats data weights)
      (check-expected (Statistics-samples stats) N 0
                      "number of samples")
      (check-expected (Statistics-weight-sum stats) 
                      (apply + weights) 
                      (* (Statistics-weight-sum stats) 1.0e-13)
                      "sum of weights")
      (check-expected (Statistics-min stats) (apply min data) 0.0
                      "minimum value")
      (check-expected (Statistics-max stats) (apply max data) 1.0e-13
                      "maximum value")
      (check-expected (Statistics-mean stats) average
                      (if (= average 0.0)
                          1.0e-13
                          (* (abs average) 1.0e-13))
                      "mean value")
      (check-expected (Statistics-variance stats) (* sigma sigma)
                      (* sigma sigma 1.0e-4)
                      "variance")
      (check-expected (Statistics-standard-deviation stats) sigma
                      (* sigma 1.0e-4)
                      "standard deviation")
      (check-expected (Statistics-skewness stats) 0.0 1.0e-4
                      "skewness")
      (check-expected (Statistics-kurtosis stats) 0.0 1.0e-1
                      "kurtosis")

      (deleting-let ((cum (new-CumulativeNormalDistribution average sigma)
                          delete-CumulativeNormalDistribution))
        (let ((two-std-dev (CumulativeNormalDistribution-call
                            cum
                            (+ average (* 2 sigma)))))
          (let ((right-potential-upside (max 0.0 (+ average (* 2 sigma)))))
            (check-expected (RiskStatistics-potential-upside stats two-std-dev)
                            right-potential-upside
                            (if (= 0.0 right-potential-upside)
                                1.0e-3
                                (* right-potential-upside 1.0e-3))
                            "potential upside"))
          (let ((right-VAR (- (min 0.0 (- average (* 2 sigma))))))
            (check-expected (RiskStatistics-value-at-risk stats two-std-dev)
                            right-VAR
                            (if (= 0.0 right-VAR)
                                1.0e-3
                                (* 1.0e-3 right-VAR))
                            "value at risk"))
          (if (not (and (> average 0) (< sigma average)))
              (begin
                (let ((right-ex-shortfall
                       (- (min 
                           (- average
                              (/ (* sigma sigma 
                                    (gaussian (- average (* 2 sigma))))
                                 (- 1 two-std-dev)))
                           0.0))))
                  (check-expected (RiskStatistics-expected-shortfall 
                                   stats two-std-dev)
                                  right-ex-shortfall
                                  (if (= 0.0 right-ex-shortfall)
                                      2.0e-4
                                      (* 2.0e-4 right-ex-shortfall))
                                  "expected shortfall"))
                (check-expected (RiskStatistics-shortfall stats target) 
                                0.5 0.5e-8
                                "shortfall")
                (let ((right-avg-shortfall (* 2 (/ sigma (sqrt (* 2 pi))))))
                  (check-expected (RiskStatistics-average-shortfall 
                                   stats target)
                                  right-avg-shortfall
                                  (* 1.0e-4 right-avg-shortfall)
                                  "average shortfall"))))
          (Statistics-reset! stats)))))
  (deleting-let ((s (new-RiskStatistics) delete-RiskStatistics))
    (for-each-combination ((average '(-100.0 0.0 100.0))
                           (sigma '(0.1 1.0 10.0)))
      (test-case s average sigma))))

(define RiskStatistics-suite
  (make-test-suite 
   "Risk-statistics tests"
   (make-test-case/msg "Testing risk statistics" (Risk-statistics-test))))


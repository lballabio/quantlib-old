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

(use-modules (QuantLib))
(load "common.scm")

(define (Risk-statistics-test)
  (let ((s (new-RiskStatistics)))
    (for-each (lambda (average)
                (for-each (lambda (sigma)
                            (risk-statistics-test-case s average sigma))
                          '(0.1 1.0 10.0)))
              '(-100.0 0.0 100.0))
    (delete-RiskStatistics s)))

(define (risk-statistics-test-case stats average sigma)
  (define pi (acos -1.0))
  (define (gaussian x)
    (let ((dx (- x average)))
      (/ (exp (/ (- (* dx dx)) (* 2.0 sigma sigma)))
         (* sigma (sqrt (* 2.0 pi))))))
  (let ((N 25000)
        (sigmas 15)
        (target average)
        (normal (new-NormalDistribution average sigma)))
    (let ((data-min (- average (* sigmas sigma)))
          (data-max (+ average (* sigmas sigma))))
      (let ((h (risk-stat-grid-step data-min data-max N))
            (data (risk-stat-grid data-min data-max N)))
        (let ((weights (map gaussian data)))
          (RiskStatistics-add-weighted-sequence stats data weights)
          (check "number of samples"
                 (RiskStatistics-samples stats)
                 N
                 0)
          (check "sum of weights"
                 (RiskStatistics-weight-sum stats)
                 (apply + weights)
                 0.0)
          (check "minimum value"
                 (RiskStatistics-min stats)
                 (apply min data)
                 0.0)
          (check "maximum value"
                 (RiskStatistics-max stats)
                 (apply max data)
                 1.0e-13)
          (check "mean value"
                 (RiskStatistics-mean stats)
                 average
                 (if (= average 0.0)
                     1.0e-13
                     (* (abs average) 1.0e-13)))
          (check "variance"
                 (RiskStatistics-variance stats)
                 (* sigma sigma)
                 (* sigma sigma 1.0e-4))
          (check "standard deviation"
                 (RiskStatistics-standard-deviation stats)
                 sigma
                 (* sigma 1.0e-4))
          (check "skewness"
                 (RiskStatistics-skewness stats)
                 0.0
                 1.0e-4)
          (check "kurtosis"
                 (RiskStatistics-kurtosis stats)
                 0.0
                 1.0e-1)

          (let ((cum (new-CumulativeNormalDistribution average sigma)))
            (let ((two-std-dev (CumulativeNormalDistribution-call
                                cum
                                (+ average (* 2 sigma)))))
              (let ((right-potential-upside (max 0.0 (+ average (* 2 sigma)))))
                (check "potential upside"
                       (RiskStatistics-potential-upside stats
                                                        two-std-dev)
                       right-potential-upside
                       (if (= 0.0 right-potential-upside)
                           1.0e-3
                           (* right-potential-upside 1.0e-3))))
              (let ((right-VAR (- (min 0.0 (- average (* 2 sigma))))))
                (check "value at risk"
                       (RiskStatistics-value-at-risk stats 
                                                     two-std-dev)
                       right-VAR
                       (if (= 0.0 right-VAR)
                           1.0e-3
                           (* 1.0e-3 right-VAR))))
              (let ((right-ex-shortfall
                     (- (min 
                         (- average
                            (/ (* sigma sigma 
                                  (gaussian (- average (* 2 sigma))))
                               (- 1 two-std-dev)))
                         0.0))))
                (check "expected shortfall"
                       (RiskStatistics-expected-shortfall 
                        stats two-std-dev)
                       right-ex-shortfall
                       (if (= 0.0 right-ex-shortfall)
                           1.0e-4
                           (* 1.0e-4 right-ex-shortfall))))
              (check "shortfall"
                     (RiskStatistics-shortfall stats target)
                     0.5
                     0.5e-8)
              (let ((right-avg-shortfall (/ sigma (sqrt (* 2 pi)))))
                (check "average shortfall"
                       (RiskStatistics-average-shortfall stats 
                                                         target)
                       right-avg-shortfall
                       (* 1.0e-4 right-avg-shortfall))))
              (delete-CumulativeNormalDistribution cum))))))
  (RiskStatistics-reset! stats))

(define (risk-stat-grid-step xmin xmax N)
  (/ (- xmax xmin) (- N 1)))

(define (risk-stat-grid xmin xmax N)
  (let ((h (grid-step xmin xmax N)))
    (define (loop i accum)
      (if (= i N)
          accum
          (loop (+ i 1) (cons (+ xmin (* h i)) accum))))
    (loop 0 '())))


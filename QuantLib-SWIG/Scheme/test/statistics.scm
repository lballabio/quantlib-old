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

(define (Statistics-test)
  (deleting-let ((stats (new-Statistics) delete-Statistics))
    (let ((tolerance 1.0e-9)
          (data    '(  3   4   5   2   3   4   5   6   4   7))
          (weights '(1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0)))
      (Statistics-add-weighted-sequence stats data weights)
      (check-expected (Statistics-samples stats) (length data) 0
                      "number of samples")
      (check-expected (Statistics-weight-sum stats) (apply + weights) 0.0
                      "sum of weights")
      (check-expected (Statistics-min stats) (apply min data) 0.0
                      "minimum value")
      (check-expected (Statistics-max stats) (apply max data) 0.0
                      "maximum value")
      (check-expected (Statistics-mean stats)
                      (/ (apply + (map * data weights)) (length data))
                      tolerance
                      "mean value")
      (check-expected (Statistics-variance stats) 2.23333333333 tolerance
                      "variance")
      (check-expected (Statistics-standard-deviation stats) 1.4944341181 
                      tolerance
                      "standard deviation")
      (check-expected (Statistics-skewness stats) 0.359543071407 tolerance
                      "skewness")
      (check-expected (Statistics-kurtosis stats) -0.151799637209 tolerance
                      "kurtosis")
      (Statistics-reset! stats)

      (Statistics-add-weighted-sequence stats
                                        (map (lambda (x) (- x 3)) data)
                                        weights)
      (check-expected (Statistics-downside-deviation stats) 0.333333333 
                      tolerance
                      "downside deviation"))))

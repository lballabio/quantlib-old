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

(define (Operator-test)
  (define (check-difference f1 f2 h tolerance f1-name f2-name)
    (let ((e (norm (difference f1 f2) h)))
      (check-zero e tolerance
                  "norm of " f1-name " minus " f2-name ": " e cr
                  "tolerance exceeded" cr)))
  (define (difference f1 f2)
    (define (diff-iter l a i acc)
      (if (null? l)
          acc
          (diff-iter (cdr l) a (+ i 1) 
                     (cons (- (car l) (Array-ref a i)) acc))))
    (reverse (diff-iter f1 f2 0 '())))

  (let* ((average 0.0)
         (sigma 1.0)
         (normal-dist    (new-NormalDistribution average sigma))
         (cumul-dist     (new-CumulativeNormalDistribution average sigma)))
      (let* ((N 10001)
             (xmin (- average (* 4.0 sigma)))
             (xmax (+ average (* 4.0 sigma)))
             (h (grid-step xmin xmax N))
             (x (grid xmin xmax N)))

        (define (normal x)
          (NormalDistribution-call normal-dist x))
        (define (normal-derivative x)
          (NormalDistribution-derivative normal-dist x))
        (define (cumulative x)
          (CumulativeNormalDistribution-call cumul-dist x))
        
        (let ((y (map normal x))
              (y-int (map cumulative x))
              (y-der (map normal-derivative x)))
          
          (deleting-let ((D0 (new-D0 N h) delete-TridiagonalOperator)
                         (D+D- (new-D+D- N h) delete-TridiagonalOperator))
            (deleting-let ((y-temp (TridiagonalOperator-apply-to
                                    D0 (list->vector y-int))
                                   delete-Array)
                           (yd-temp (TridiagonalOperator-apply-to 
                                     D+D- (list->vector y-int))
                                    delete-Array))

              (check-difference y y-temp h 1.0e-6
                                "FD 1st derivative of cum(x)"
                                "analytic Gaussian")
              (check-difference y-der yd-temp h 1.0e-4
                                "FD 2nd derivative of cum(x)"
                                "identity")))))))

(define Operator-suite
  (make-test-suite 
   "Operator tests"
   (make-test-case/msg "Testing differential operators" (Operator-test))))


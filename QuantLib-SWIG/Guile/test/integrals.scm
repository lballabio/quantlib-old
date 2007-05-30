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
; program; if not, please email quantlib-dev@lists.sf.net
;
; The QuantLib License is also available at <http://quantlib.org/license.shtml>
; The members of the QuantLib Group are listed in the QuantLib License

(load "common.scm")
(load "utilities.scm")
(load "unittest.scm")
(use-modules (QuantLib))

(greg-assert/display
 "Testing segment integral"
 (let* ((pi (acos -1.0))
        (gauss (lambda (x)
                 (/ (exp (- (* x x 0.5))) (sqrt (* 2.0 pi))))))
   (let ((I (new-SegmentIntegral 10000)))
     (let ((tolerance 1.0e-4)
           (cases (list
                   (list "f(x) = 1"        (lambda (x) 1.0)      0  1    1)
                   (list "f(x) = x"        (lambda (x) x)        0  1  0.5)
                   (list "f(x) = x^2"      (lambda (x) (* x x))  0  1  1/3)
                   (list "f(x) = sin(x)"   sin                   0 pi    2)
                   (list "f(x) = sin(x)"   cos                   0 pi    0)
                   (list "f(x) = Gauss(x)" gauss               -10 10    1))))
       (check-all
        (map (lambda (test-case)
               (let-at-once ((tag f a b expected) test-case)
                 (let ((calculated (SegmentIntegral-calculate I f a b)))
                   (check-expected calculated expected tolerance tag))))
             cases))))))

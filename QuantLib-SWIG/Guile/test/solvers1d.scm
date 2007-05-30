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

(load "unittest.scm")
(use-modules (QuantLib))

(define (test-solver make solve)
  (let ((solver (make)))
    (for-each (lambda (accuracy)
                (let ((root (solve solver 
                                   (lambda (x) (- (* x x) 1))
                                   accuracy
                                   1.5
                                   0.1)))
                  (check-equal root 1.0 accuracy
                               "solve():" cr
                               "  expected:        1.0" cr
                               "  calculated root: " root cr
                               "  accuracy:        " accuracy cr))
                (let ((root (solve solver 
                                   (lambda (x) 
                                     (- (* x x) 1))
                                   accuracy
                                   1.5
                                   0.0
                                   2.0)))
                  (check-equal root 1.0 accuracy
                               "bracketed solve():" cr
                               "  expected:        1.0" cr
                               "  calculated root: " root cr
                               "  accuracy:        " accuracy cr)))
              '(1.0e-4 1.0e-6 1.0e-8))))

(greg-assert/display
 "Testing 1D solvers"
 (check-all
  (map (lambda (l) (apply test-solver l))
       (list (list new-Brent Brent-solve)
             (list new-Bisection Bisection-solve)
             (list new-FalsePosition FalsePosition-solve)
             (list new-Ridder Ridder-solve)
             (list new-Secant Secant-solve)))))



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

(define (Solver-1D-test)
  (for-each (lambda (l) (apply test-solver l))
            (list (list new-Brent         delete-Brent)
                  (list new-Bisection     delete-Bisection)
                  (list new-FalsePosition delete-FalsePosition)
                  (list new-Ridder        delete-Ridder)
                  (list new-Secant        delete-Secant))))

(define (test-solver make delete)
  (let ((solver (make)))
    (for-each (lambda (accuracy)
                (let ((root (Solver1D-solve solver 
                                            (lambda (x) (- (* x x) 1))
                                            accuracy
                                            1.5
                                            0.1)))
                  (if (< accuracy (abs (- root 1.0)))
                      (let ((error-msg
                             (string-append
                              (format #f "solve():~n")
                              (format #f "  expected:        1.0~n")
                              (format #f "  calculated root: ~a~n" root)
                              (format #f "  accuracy:        ~a~n" accuracy))))
                        (error error-msg))))
                (let ((root (Solver1D-bracketed-solve solver 
                                                      (lambda (x) 
                                                        (- (* x x) 1))
                                                      accuracy
                                                      1.5
                                                      0.0
                                                      2.0)))
                  (if (< accuracy (abs (- root 1.0)))
                      (let ((error-msg
                             (string-append
                              (format #f "bracketed-solve():~n")
                              (format #f "  expected:        1.0~n")
                              (format #f "  calculated root: ~a~n" root)
                              (format #f "  accuracy:        ~a~n" accuracy))))
                        (error error-msg)))))
                '(1.0e-4 1.0e-6 1.0e-8))
    (delete solver)))

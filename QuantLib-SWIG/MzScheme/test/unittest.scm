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

(require (lib "test.ss" "schemeunit"))
(require (lib "text-ui.ss" "schemeunit"))

; output progress while testing
(define-syntax make-test-case/msg
  (syntax-rules ()
    ((make-test-case/msg name action)
     (make-test-case name (begin (display name) (display "...") 
                                 (flush-output) 
                                 action 
                                 (newline))))
    ((make-test-case/msg name action teardown)
     (make-test-case name (begin (display name) (display "...") 
                                 (flush-output) 
                                 action
                                 (newline))
                     teardown))
    ((make-test-case/msg name action setup teardown)
     (make-test-case name (begin (display name) (display "...") 
                                 (flush-output) 
                                 action
                                 (newline))
                     setup
                     teardown))))

; fool Schemeunit into writing "assertion failed"
(define-assertion (assertion) #f)
; assertion concatenating args into error message 
(define-syntax check
  (syntax-rules ()
    ((check cond msg1 msg2 ...)
     (if (not cond)
         (assertion (string-append 
                     (format "~a" msg1)
                     (format "~a" msg2) ...))))))

; carriage return (useful for error messages)
(define cr (format "~n"))

; check for equality within a tolerance
(define-syntax check-equal
  (syntax-rules ()
    ((check-equal x y tolerance msg1 msg2 ...)
     (check (<= (abs (- x y)) tolerance) msg1 msg2 ... cr))))

; check for null value within a tolerance
(define-syntax check-zero
  (syntax-rules ()
    ((check-zero x tolerance msg1 msg2 ...)
     (check-equal x 0.0 tolerance msg1 msg2 ... cr))))

; check against expected value within a tolerance
(define-syntax check-expected
  (syntax-rules ()
    ((check-expected calculated expected tolerance msg1 msg2 ...)
     (check-equal calculated expected tolerance
            msg1 msg2 ... cr
            "    calculated: " calculated cr
            "    expected:   " expected cr))))


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

; utility functions for testing

(use-modules (ice-9 greg))

(define-macro greg-assert/display
  (lambda (name body)
    `(greg-expect-pass ,name 
                       (begin
                         (display ,name) (display "...")
                         (flush-all-ports)
                         (let ((x ,body)) (newline) x)))))

(define-macro check
  (lambda (condition . msg)
    `(let ((check-result ,condition))
       (if (not check-result)
           (greg-dlog
            (apply string-append (map (lambda (token) (format #f "~A" token)) 
                                      (list ,@msg)))))
       check-result)))

(define (check-all checks)
  ; (apply and checks) doesn't work
  (cond ((null? checks) #t)
        ((car checks) (check-all (cdr checks)))
        (else #f)))

; shorter
(define cr "\n")

; a few specialized assertions
(define-macro check-zero
  (lambda (x tolerance . msg)
    `(check (<= (abs ,x) ,tolerance) ,@msg)))
(define-macro check-equal
  (lambda (lhs rhs tolerance . msg)
    `(check-zero (- ,lhs ,rhs) ,tolerance ,@msg)))
(define-macro check-expected
  (lambda (calculated expected tolerance . msg)
    `(check-equal ,calculated ,expected ,tolerance
                  ,@msg cr
                  "    calculated: " ,calculated cr
                  "    expected:   " ,expected cr)))


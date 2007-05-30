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


; bind a set of names to the elements of a list
(define-syntax let-at-once
  (syntax-rules ()
    ((let-at-once ((var1 var2 ...) vals) body1 body2 ...)
     (call-with-values
       (lambda () (apply values vals))
       (lambda (var1 var2 ...) 
         body1 body2 ...)))))

; bind a set of parameters to each test case

(define-syntax for
  (syntax-rules (in)
    ((for (var1 var2 ...) in cases 
          body1 body2 ...)
     (for-each
      (lambda (test-case)
        (let-at-once ((var1 var2 ...) test-case)
          body1 body2 ...))
      cases))
    ((for ((var1 in set1) (var2 in set2) ...)
          body1 body2 ...)
     (for-each
      (lambda (test-case)
        (let-at-once ((var1 var2 ...) test-case)
          body1 body2 ...))
      (combinations set1 set2 ...)))))

; get a number of test cases by building all possible
; combinations of sets of parameters
(define (combinations l . ls)
  (define (add-one x ls)
    (map (lambda (l) (cons x l)) ls))
  (define (add-all l ls)
    (flatmap (lambda (x) (add-one x ls)) l))
  (if (null? ls)
      (map list l)
      (add-all l (apply combinations ls))))

(define (flatmap proc seq)
  (foldr append '() (map proc seq)))

(define (foldr op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (foldr op initial (cdr sequence)))))

; repeat n times a series of instructions

(define (do-repeat n thunk)
  (if (> n 0)
      (begin
        (thunk)
        (do-repeat (- n 1) thunk))))

(define-syntax repeat
  (syntax-rules ()
    ((repeat (n) body1 body2 ...)
     (do-repeat n (lambda () body1 body2 ...)))))


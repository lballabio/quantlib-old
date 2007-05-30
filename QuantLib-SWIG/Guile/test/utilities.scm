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
(define-macro let-at-once
  (lambda (binding . body)
    (let ((vars (car binding))
          (vals (cadr binding)))
      `(call-with-values
         (lambda () (apply values ,vals))
         (lambda ,vars ,@body)))))

; bind a set of parameters to each test case
(define-macro for-each-case
  (lambda (bindings . body)
    (let ((vars (car bindings))
          (cases (cadr bindings))
          (test-case (gensym)))
      `(for-each
        (lambda (,test-case)
          (let-at-once (,vars ,test-case)
            ,@body))
        ,cases))))

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

; bind a set of parameters to all possible combinations
(define-macro for-each-combination
  (lambda (bind-sets . body)
    (let ((vars (map car bind-sets))
          (sets (map cadr bind-sets))
          (combo (gensym)))
      `(for-each 
        (lambda (,combo)
          (let-at-once (,vars ,combo)
            ,@body))
        (combinations ,@sets)))))

; repeat n times a series of instructions

(define (do-repeat n thunk)
  (if (> n 0)
      (begin
        (thunk)
        (do-repeat (- n 1) thunk))))

(define-macro repeat
  (lambda (times . body)
    `(do-repeat ,(car times)
                (lambda () ,@body))))


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

; common utility functions
(define (range i j)
  (define (range-iter i j acc)
    (if (= i j)
        acc
        (range-iter (+ i 1) j (cons i acc))))
  (reverse (range-iter i j '())))
(define (sorted? l less-than?)
  (if (null? l)
      #t
      (let ((first (car l))
            (rest (cdr l)))
        (if (null? rest)
            #t
            (let ((second (car rest)))
              (if (less-than? first second)
                  (sorted? rest less-than?)
                  #f))))))


; make a grid
(define (grid-step xmin xmax N)
  (/ (- xmax xmin) (- N 1)))
(define (grid xmin xmax N)
  (let ((h (grid-step xmin xmax N)))
    (define (loop i accum)
      (if (= i N)
          (reverse accum)
          (loop (+ i 1) (cons (+ xmin (* h i)) accum))))
    (loop 0 '())))

; norm of a discretized function
(define (norm f h)
  (define (sum f)
    (apply + f))
  (define (integral f h)
    (let ((first (car f))
          (last (list-ref f (- (length f) 1))))
      (sqrt (* h (- (sum f) (* 0.5 first) (* 0.5 last))))))
  (let ((f^2 (map (lambda (x) (* x x)) f)))
    (integral f^2 h)))



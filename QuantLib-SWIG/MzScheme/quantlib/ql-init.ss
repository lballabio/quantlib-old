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
; The QuantLib License is also available at http://quantlib.org/license.html
; The members of the QuantLib Group are listed in the QuantLib License

(require (lib "file.ss" "dynext"))
(load-relative-extension (append-extension-suffix "QuantLibc"))

; added functionality
(define History-old-init new-History)
(define (new-History dates values)
  (let ((null (null-double)))
    (History-old-init dates
                      (map (lambda (x) (or x null)) values))))
(define (History-map h f)
  (let ((results '()))
    (History-for-each h (lambda (e)
                          (if e
                              (set! results (cons (f e) results))
                              (set! results (cons #f results)))))
    (reverse results)))
(define (History-map-valid h f)
  (let ((results '()))
    (History-for-each-valid h (lambda (e)
                                (set! results (cons (f e) results))))
    (reverse results)))
(define (Schedule-map s f)
  (let ((results '()))
    (Schedule-for-each s (lambda (d) (set! results (cons (f d) results))))
    (reverse results)))


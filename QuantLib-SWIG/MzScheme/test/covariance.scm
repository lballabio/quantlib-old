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

(define (Covariance-test)
  (let* ((vol #(0.1 0.5 1.0))
         (corr #( #(1.0 0.2 0.5)
                  #(0.2 1.0 0.8)
                  #(0.5 0.8 1.0)))
         (n (vector-length vol)))
    (deleting-let ((expected-cov (new-Matrix n n) delete-Matrix)
                   (calculated-cov (get-covariance vol corr) delete-Matrix))
      ; calculate expected covariance
      (for-each-combination ((i (range 0 n)))
        (let ((vol_i (vector-ref vol i)))
          (Matrix-set! expected-cov i i (* vol_i vol_i))
          (for-each-combination ((j (range 0 i)))
            (let ((vol_j (vector-ref vol j))
                  (corr_ij (vector-ref (vector-ref corr i) j)))
              (Matrix-set! expected-cov i j (* corr_ij vol_i vol_j))
              (Matrix-set! expected-cov j i (Matrix-ref expected-cov i j))))))
      ; check
      (for-each-combination ((i (range 0 n))
                             (j (range 0 n)))
        (check-expected (Matrix-ref calculated-cov i j)
                        (Matrix-ref expected-cov i j) 1.0e-10
                        "covariance[" i "][" j "]")))))

(define Covariance-suite
  (make-test-suite 
   "Covariance tests"
   (make-test-case/msg "Testing covariance calculation" (Covariance-test))))


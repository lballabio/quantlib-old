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

(require-library "quantlib.ss" "quantlib")

(define (Market-element-test-1)
  (let ((flag #f))
    (let ((me (new-SimpleMarketElement 0.0))
          (obs (new-Observer (lambda () (set! flag #t)))))
      (Observer-register-with obs me)
      (SimpleMarketElement-value-set! me 3.14)
      (if (not flag)
          (error "Observer was not notified of market element change"))
      (delete-Observer obs)
      (delete-SimpleMarketElement me))))

(define (Market-element-test-2)
  (for-each (lambda (f)
              (letrec ((me (new-SimpleMarketElement 17.0))
                       (h (new-MarketElementHandle me))
                       (derived-me (new-DerivedMarketElement h f)))
                (let ((derived-result (MarketElement-value derived-me))
                      (function-result (f (MarketElement-value me))))
                  (if (> (abs (- derived-result function-result)) 1.0e-10)
                      (let ((error-msg
                             (string-append
                              (format "derived market element yields ~a~n"
                                      derived-result)
                              (format "function result is ~a~n"
                                      function-result))))
                        (error error-msg))))
                (delete-DerivedMarketElement derived-me)
                (delete-MarketElementHandle h)
                (delete-SimpleMarketElement me)))
            (list (lambda (x) (+ x 10))
                  (lambda (x) (* x 10))
                  (lambda (x) (- x 10)))))

(define (Market-element-test-3)
  (letrec ((me1 (new-SimpleMarketElement 12.0))
           (me2 (new-SimpleMarketElement 13.0))
           (h1 (new-MarketElementHandle me1))
           (h2 (new-MarketElementHandle me2)))
    (for-each (lambda (f)
                (let ((composite-me (new-CompositeMarketElement h1 h2 f)))
                  (let ((composite-result (MarketElement-value composite-me))
                        (function-result (f (MarketElement-value me1)
                                             (MarketElement-value me2))))
                    (if (> (abs (- composite-result function-result)) 1.0e-10)
                        (let ((error-msg
                             (string-append
                              (format "composite market element yields ~a~n"
                                      composite-result)
                              (format "function result is ~a~n"
                                      function-result))))
                          (error error-msg))))
                  (delete-CompositeMarketElement composite-me)))
              (list (lambda (x y) (+ x y))
                    (lambda (x y) (* x y))
                    (lambda (x y) (- x y))))
    (delete-MarketElementHandle h1)
    (delete-MarketElementHandle h2)
    (delete-SimpleMarketElement me1)
    (delete-SimpleMarketElement me2)))


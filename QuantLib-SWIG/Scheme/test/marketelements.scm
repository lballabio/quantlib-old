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

(define (Market-element-observability-test)
  (let ((flag #f))
    (deleting-let ((me (new-SimpleMarketElement 0.0)
                       delete-MarketElement)
                   (obs (new-Observer (lambda () (set! flag #t)))
                        delete-Observer))
      (deleting-let ((temp (MarketElement->Observable me)
                           delete-Observable))
        (Observer-register-with obs temp))
      (SimpleMarketElement-value-set! me 3.14)
      (if (not flag)
          (error "Observer was not notified of market element change")))))

(define (Derived-market-element-test)
  (for-each 
   (lambda (f)
     (deleting-let* ((me (new-SimpleMarketElement 17.0)
                         delete-MarketElement)
                     (h (new-MarketElementHandle me)
                        delete-MarketElementHandle)
                     (derived-me (new-DerivedMarketElement h f)
                                 delete-MarketElement))
       (let ((derived-result (MarketElement-value derived-me))
             (function-result (f (MarketElement-value me))))
         (assert-equal derived-result function-result 1.0e-10
                       "derived market element yields " derived-result cr
                       "function result is " function-result cr))))
   (list (lambda (x) (+ x 10))
         (lambda (x) (* x 10))
         (lambda (x) (- x 10)))))

(define (Composite-market-element-test)
  (deleting-let* ((me1 (new-SimpleMarketElement 12.0) delete-MarketElement)
                  (me2 (new-SimpleMarketElement 13.0) delete-MarketElement)
                  (h1 (new-MarketElementHandle me1)
                      delete-MarketElementHandle)
                  (h2 (new-MarketElementHandle me2)
                      delete-MarketElementHandle))
    (for-each 
     (lambda (f)
       (deleting-let ((composite-me (new-CompositeMarketElement h1 h2 f)
                                    delete-MarketElement))
         (let ((composite-result (MarketElement-value composite-me))
               (function-result (f (MarketElement-value me1)
                                   (MarketElement-value me2))))
           (assert-equal composite-result function-result 1.0e-10
                         "composite market element yields " 
                         composite-result cr
                         "function result is " function-result cr))))
     (list (lambda (x y) (+ x y))
           (lambda (x y) (* x y))
           (lambda (x y) (- x y))))))

(define (Market-element-handle-observability-test)
  (let ((flag #f))
    (deleting-let* ((me1 (new-SimpleMarketElement 0.0)
                         delete-MarketElement)
                    (me2 (new-SimpleMarketElement 0.0)
                         delete-MarketElement)
                    (h (new-MarketElementHandle me1)
                       delete-MarketElementHandle)
                    (obs (new-Observer (lambda () (set! flag #t)))
                         delete-Observer))
      (deleting-let ((temp (MarketElementHandle->Observable h)
                           delete-Observable))
        (Observer-register-with obs temp))
      (SimpleMarketElement-value-set! me1 3.14)
      (if (not flag)
          (error "Observer was not notified of market element change"))
      (set! flag #f)
      (MarketElementHandle-link-to! h me2)
      (if (not flag)
          (error "Observer was not notified of market element change")))))


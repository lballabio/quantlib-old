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

(define (Instrument-test)
  (let ((flag #f))
    (let* ((me (new-SimpleMarketElement 0.0))
           (h (new-MarketElementHandle me))
           (s (new-Stock h))
           (obs (new-Observer (lambda () (set! flag #t)))))
      (Observer-register-with obs s)
      (SimpleMarketElement-value-set! me 3.14)
      (if (not flag)
          (error "Observer was not notified of stock value change"))
      (delete-Observer obs)
      (delete-Stock s)
      (delete-MarketElementHandle h)
      (delete-SimpleMarketElement me))))


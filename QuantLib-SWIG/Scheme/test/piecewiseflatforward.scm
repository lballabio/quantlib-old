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
;
; $Id$

(load "common.scm")

(define (PiecewiseFlatForward-test)
  (let ((settlement-days 2)
        (fixing-days 2))
    (deleting-let* ((euribor-handle (new-TermStructureHandle)
                                    delete-TermStructureHandle)
                    (calendar (new-Calendar "TARGET") delete-Calendar)
                    (real-today (Date-todays-date) delete-Date)
                    (today (Calendar-roll calendar real-today) delete-Date)
                    (settlement (Calendar-advance calendar today 
                                                  settlement-days "days"
                                                  "following")
                                delete-Date))
      (let ((rolling-convention "ModifiedFollowing")
            (deposit-data '((1   "week" 4.559)
                            (1  "month" 4.581)
                            (2 "months" 4.573)
                            (3 "months" 4.557)
                            (6 "months" 4.496)
                            (9 "months" 4.490)))
            (swap-rolling-convention "modifiedFollowing")
            (fixed-frequency 1)
            (fixed-is-adjusted #f)
            (floating-frequency 2)
            (swap-data '((1 4.54)
                         (2 4.63)
                         (3 4.75)
                         (4 4.86)
                         (5 4.99)
                         (6 5.11)
                         (7 5.23)
                         (8 5.33)
                         (9 5.41)
                         (10 5.47)
                         (12 5.60)
                         (15 5.75)
                         (20 5.89)
                         (25 5.95)
                         (30 5.96))))
        (deleting-let* ((day-counter (new-DayCounter "act/360")
                                     delete-DayCounter)
                        (fixed-day-count (new-DayCounter "30/360")
                                         delete-DayCounter)
                        (curve-day-counter (new-DayCounter "act/360")
                                           delete-DayCounter)
                        ; instruments to bootstrap on
                        (instruments 
                         (append
                          ; add deposits
                          (map (lambda (datum)
                                 (let-at-once ((n units rate) datum)
                                   (new-DepositRateHelper
                                    (new-MarketElementHandle
                                     (new-SimpleMarketElement (/ rate 100)))
                                    n units settlement-days
                                    calendar rolling-convention 
                                    day-counter)))
                               deposit-data)
                          ; add swaps
                          (map (lambda (datum)
                                 (let-at-once ((years rate) datum)
                                   (new-SwapRateHelper
                                    (new-MarketElementHandle
                                     (new-SimpleMarketElement (/ rate 100)))
                                    years "years" settlement-days
                                    calendar swap-rolling-convention 
                                    fixed-frequency fixed-is-adjusted 
                                    fixed-day-count floating-frequency)))
                               swap-data))
                         ; clean-up function
                         (lambda (l) (for-each delete-RateHelper l)))
                        ; the curve itself
                        (term-structure (new-PiecewiseFlatForward
                                         today settlement instruments 
                                         curve-day-counter)
                                        delete-TermStructure))
          (TermStructureHandle-link-to! euribor-handle term-structure)
          ; check deposits
          (for-each-case ((n units rate) deposit-data)
            (deleting-let ((index (new-Xibor "Euribor" n units 
                                             euribor-handle)
                                  delete-Index))
              (let ((expected-rate (/ rate 100))
                    (estimated-rate (Index-fixing index today)))
                (check-expected estimated-rate expected-rate 1.0e-9
                                n "-" units " deposit"))))
          ; check swaps
          (for-each-case ((years rate) swap-data)
            (deleting-let* ((index (new-Xibor 
                                    "Euribor" (/ 12 floating-frequency)
                                    "months" euribor-handle)
                                   delete-Index)
                            (swap (new-SimpleSwap 
                                   #t settlement years "years" calendar 
                                   swap-rolling-convention 100 
                                   fixed-frequency 0.0 fixed-is-adjusted 
                                   fixed-day-count floating-frequency index 
                                   fixing-days 0.0 euribor-handle)
                                  delete-Instrument))
              (let ((expected-rate (/ rate 100))
                    (estimated-rate (SimpleSwap-fair-rate swap)))
                (check-expected estimated-rate expected-rate 1.0e-9
                                years " years swap")))))))))

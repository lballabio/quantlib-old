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

(define (Date-test)
  (define (test-single-date serial-number end dyold dold mold yold wdold)
    (if (< serial-number end)
        (let ((date (new-Date serial-number)))
          (let ((serial (Date-serial-number date))
                (dy (Date-day-of-year date))
                (d  (Date-day-of-month date))
                (m  (Date-month date))
                (y  (Date-year date))
                (wd (Date-weekday-number date)))
            ; check day, month, year, serial number consistency
            (test-serial-number date serial-number serial)
            (test-new-date date serial-number d m y)
            ; check whether we are skipping any date
            (test-day-of-year date dy dyold yold)
            (test-day-month-year date d m y dold mold yold)
            ; check month number
            (test-month date m)
            ; check day of month number
            (test-day-of-month date d m y)
            ; check weekday
            (test-weekday date wd wdold)
            ; manual deletion here to preserve tail-recursion
            (delete-Date date)
            ; on to next date with the latest results as the old ones
            (test-single-date (+ serial-number 1) end dy d m y wd)))))
  (define (test-serial-number date original new)
    (assert (= original new)
            "inconsistent serial number:" cr
            "    original:  " original cr
            "    date:      " (Date->string date) cr
            "    serial no: " new cr))
  (define (test-new-date date serial-number d m y)
    (deleting-let ((clone (new-Date d m y) delete-Date))
      (let ((serial (Date-serial-number clone)))
        (assert (= serial-number serial)
                "inconsistent serial number:" cr
                "    date:      " (Date->string date) cr
                "    serial no: " serial-number cr
                "    clone:     " (Date->string clone) cr
                "    serial no: " serial cr))))
  (define (test-day-of-year date dy dyold yold)
    (assert (or (= dy (+ 1 dyold))
                (and (not (Date-is-leap? yold))
                     (= dyold 365)
                     (= dy 1))
                (and (Date-is-leap? yold)
                     (= dyold 366)
                     (= dy 1)))
            "wrong day of year:" cr 
            "    date:        " (Date->string date) cr
            "    day of year: " dy cr
            "    previous:    " dyold cr))
  (define (test-day-month-year date d m y dold mold yold)
    (assert (or (and (= d (+ dold 1))
                     (= m mold)
                     (= y yold))
                (and (= d 1)
                     (= m (+ mold 1))
                     (= y yold))
                (and (= d 1)
                     (= m 1)
                     (= y (+ yold 1))))
            "wrong day, month, year increment:" cr
            "    date: " (Date->string date) cr
            "    day, month, year: " d "/" m "/" y cr
            "    previous:         " dold "/" mold "/" yold cr))
  (define (test-month date m)
    (assert (and (>= m 1) (<= m 12))
            "invalid month:" cr
            "    date:  " (Date->string date) cr
            "    month: " m cr))
  (define (test-day-of-month date d m y)
    (assert (and (>= d 1)
                 (or (and (<= d 31) (= m 1))
                     (and (<= d 28) (= m 2))
                     (and (= d 29)  (= m 2) (Date-is-leap? y))
                     (and (<= d 31) (= m 3))
                     (and (<= d 30) (= m 4))
                     (and (<= d 31) (= m 5))
                     (and (<= d 30) (= m 6))
                     (and (<= d 31) (= m 7))
                     (and (<= d 31) (= m 8))
                     (and (<= d 30) (= m 9))
                     (and (<= d 31) (= m 10))
                     (and (<= d 30) (= m 11))
                     (and (<= d 31) (= m 12))))
            "invalid day of month:" cr
            "    date:  " (Date->string date) cr
            "    day  : " d cr
            "    month: " m cr))
  (define (test-weekday date wd wdold)
    (assert (or (= wd (+ wdold 1))
                (and (= wd 1) (= wdold 7)))
            "invalid weekday:" cr
            "    date:     " (Date->string date) cr
            "    weekday : " wd cr
            "    previous: " wdold cr))
  ; setup
  (deleting-let ((min-date (Date-min-date) delete-Date)
                 (max-date (Date-max-date) delete-Date))
	(let ((begin (+ (Date-serial-number min-date) 1))
		  (end   (+ (Date-serial-number max-date) 1))
          (dyold (Date-day-of-year min-date))
          (dold  (Date-day-of-month min-date))
          (mold  (Date-month min-date))
          (yold  (Date-year min-date))
          (wdold (Date-weekday-number min-date)))
      ; recursively test all dates
      (test-single-date begin end dyold dold mold yold wdold))))

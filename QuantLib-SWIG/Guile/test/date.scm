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

(define version "$Revision$")

(use-modules (QuantLib))

(define (Date-test)
  (let ((min-date (Date-min-date))
		(max-date (Date-max-date)))
	(let ((begin (+ (Date-serial-number min-date) 1))
		  (end   (+ (Date-serial-number max-date) 1)))
	  (let ((dyold (Date-day-of-year min-date))
			(dold  (Date-day-of-month min-date))
			(mold  (Date-month min-date))
			(yold  (Date-year min-date))
			(wdold (Date-weekday-number min-date)))
		(Date-test-single-date begin end dyold dold mold yold wdold)
		(delete-Date min-date)
		(delete-Date max-date)))))

(define (Date-test-single-date serial-number end dyold dold mold yold wdold)
  (if (< serial-number end)
	  (let ((date (Date-from-serial-number serial-number)))
		(let ((serial (Date-serial-number date))
			  (dy (Date-day-of-year date))
			  (d  (Date-day-of-month date))
			  (m  (Date-month date))
			  (y  (Date-year date))
			  (wd (Date-weekday-number date)))

		  ; check day, month, year, serial number consistency
		  (Date-test-serial-numbers date serial-number serial)
		  (Date-test-new-date date serial-number d m y)

		  ; check whether we are skipping any date
		  (Date-test-day-of-years date dy dyold yold)
		  (Date-test-day-month-year date d m y dold mold yold)

		  ; check month number
		  (Date-test-month date m)

		  ; check day of month number
		  (Date-test-day-of-month date d m y)

		  ; check weekday
		  (Date-test-weekday date wd wdold)

		  ; this date tested ok - it is no longer needed
		  (delete-Date date)

		  ; on to next date with the latest results as the old ones
		  (Date-test-single-date (+ serial-number 1) end dy d m y wd)))))


(define (Date-test-serial-numbers date original new)
  (if (not (= original new))
	  (let ((error-msg
			 (string-append
			  (format #f "inconsistent serial number:\n")
			  (format #f "    original:  ~A\n" original)
			  (format #f "    date:      ~A\n" (Date->string date))
			  (format #f "    serial no: ~A\n" new))))
		(error error-msg))))

(define (Date-test-new-date date serial-number d m y)
  (let ((clone (new-Date d m y)))
	(let ((serial (Date-serial-number clone)))
	  (delete-Date clone)
	  (if (not (= serial-number serial))
		  (let ((error-msg
				 (string-append
				  (format #f "inconsistent serial number:\n")
				  (format #f "    date:      ~A\n" (Date->string date))
				  (format #f "    serial no: ~A\n" serial-number)
				  (format #f "    clone:     ~A\n" (Date->string clone))
				  (format #f "    serial no: ~A\n" serial))))
			(error error-msg))))))

(define (Date-test-day-of-years date dy dyold yold)
  (if (not (or (= dy (+ 1 dyold))
			   (and (not (Date-is-leap? yold))
					(= dyold 365)
					(= dy 1))
			   (and (Date-is-leap? yold)
					(= dyold 366)
					(= dy 1))))
	  (let ((error-msg
			 (string-append
			  (format #f "wrong day of year:\n")
			  (format #f "    date:        ~A\n" (Date->string date))
			  (format #f "    day of year: ~A\n" dy)
			  (format #f "    previous:    ~A\n" dyold))))
		(error error-msg))))

(define (Date-test-day-month-year date d m y dold mold yold)
  (if (not (or (and (= d (+ dold 1))
					(= m mold)
					(= y yold))
			   (and (= d 1)
					(= m (+ mold 1))
					(= y yold))
			   (and (= d 1)
					(= m 1)
					(= y (+ yold 1)))))
	  (let ((error-msg
			 (string-append
			  (format #f "wrong day, month, year increment:\n")
			  (format #f "    date: ~A\n" (Date->string date))
			  (format #f "    day, month, year: ~A/~A/~A\n" d m y)
			  (format #f "    previous:         ~A/~A/~A\n" dold mold yold))))
		(error error-msg))))

(define (Date-test-month date m)
  (if (not (and (>= m 1) (<= m 12)))
	  (let ((error-msg
			 (string-append
			  (format #f "invalid month:\n")
			  (format #f "    date:  ~A\n" (Date->string date))
			  (format #f "    month: ~A\n" m))))
		(error error-msg))))

(define (Date-test-day-of-month date d m y)
  (if (not (and (>= d 1)
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
					(and (<= d 31) (= m 12)))))
	  (let ((error-msg
			 (string-append
			  (format #f "invalid day of month:\n")
			  (format #f "    date:  ~A\n" (Date->string date))
			  (format #f "    day  : ~A\n" d)
			  (format #f "    month: ~A\n" m))))
		(error error-msg))))

(define (Date-test-weekday date wd wdold)
  (if (not (or (= wd (+ wdold 1))
			   (and (= wd 1) (= wdold 7))))
	  (let ((error-msg
			 (string-append
			  (format #f "invalid weekday:\n")
			  (format #f "    date:     ~A\n" (Date->string date))
			  (format #f "    weekday : ~A\n" wd)
			  (format #f "    previous: ~A\n" wdold))))
		(error error-msg))))


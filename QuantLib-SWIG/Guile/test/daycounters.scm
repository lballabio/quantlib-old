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

(define (DayCounter-test)
  (let ((cases
   ; each case lists startDate, endDate, refStartDate, refEndDate, and
					; the results for ISDA, ISMA and AFB, respectively
		 '(
		   ; first example
		   (( 1 11 2003) ( 1 5 2004) ( 1 11 2003) ( 1 5 2004) 
            0.497724380567 0.5 0.497267759563)
		   ; short first calculation period (first period)
		   (( 1  2 1999) ( 1 7 1999) ( 1  7 1998) ( 1 7 1999) 
            0.410958904110 0.410958904110 0.410958904110)
		   ; short first calculation period (second period)
		   (( 1  7 1999) ( 1 7 2000) ( 1  7 1999) ( 1 7 2000) 
            1.001377348600 1.0 1.0)
		   ; long first calculation period (first period)
		   ((15  8 2002) (15 7 2003) (15  1 2003) (15 7 2003) 
            0.915068493151 0.915760869565 0.915068493151)
		   ; long first calculation period (second period)
		   ; note: the ISDA case is in disagreement with mktc1198.pdf
		   ((15  7 2003) (15 1 2004) (15  7 2003) (15 1 2004) 
            0.504004790778 0.5 0.504109589041)
		   ; short final calculation period (penultimate period)
		   ((30  7 1999) (30 1 2000) (30  7 1999) (30 1 2000) 
            0.503892506924 0.5 0.504109589041)
		   ; short final calculation period (final period)
		   ((30  1 2000) (30 6 2000) (30  1 2000) (30 7 2000) 
            0.415300546448 0.417582417582 0.415300546448)))
		(isda (new-DayCounter "act/act(h)"))
		(isma (new-DayCounter "act/act"))
		(afb (new-DayCounter "act/act(e)")))
	(Day-counter-test-all-cases cases isda isma afb)
	(delete-DayCounter isda)
	(delete-DayCounter isma)
	(delete-DayCounter afb)))

(define (Day-counter-test-all-cases cases isda isma afb)
  (if (not (null? cases))
	  (let ((first-case (car cases))
			(other-cases (cdr cases)))
		(let ((d1 (apply new-Date (car first-case)))
			  (d2 (apply new-Date (cadr first-case)))
			  (start-ref (apply new-Date (caddr first-case)))
			  (end-ref (apply new-Date (cadddr first-case)))
			  (results (cddddr first-case)))
		  (let ((isda-expected (car results))
				(isma-expected (cadr results))
				(afb-expected (caddr results)))
			(Day-counter-test-isda isda d1 d2 isda-expected)
			(Day-counter-test-isma isma d1 d2 start-ref end-ref isma-expected)
			(Day-counter-test-afb afb d1 d2 afb-expected)
			(delete-Date d1)
			(delete-Date d2)
			(delete-Date start-ref)
			(delete-Date end-ref)))
		(Day-counter-test-all-cases other-cases isda isma afb))))

(define (Day-counter-test-isda isda d1 d2 expected)
  (let ((calculated (DayCounter-year-fraction isda d1 d2)))
	(if (not (<= (abs (- calculated expected)) 1.0e-10))
		(let ((error-msg
			   (string-append
				(format #f "ISDA day counter:\n")
				(format #f "    first date:       ~A\n" (Date->string d1))
				(format #f "    second date:      ~A\n" (Date->string d2))
				(format #f "    expected value:   ~A\n" expected)
				(format #f "    calculated value: ~A\n" calculated))))
		  (error error-msg)))))

(define (Day-counter-test-isma isma d1 d2 start-ref end-ref expected)
  (let ((calculated (DayCounter-year-fraction isma d1 d2 start-ref end-ref)))
	(if (not (<= (abs (- calculated expected)) 1.0e-10))
		(let ((error-msg
			   (string-append
				(format #f "ISMA day counter:\n")
				(format #f "    first date:       ~A\n" (Date->string d1))
				(format #f "    second date:      ~A\n" (Date->string d2))
				(format #f "    first ref. date:  ~A\n" (Date->string start-ref))
				(format #f "    second ref. date: ~A\n" (Date->string end-ref))
				(format #f "    expected value:   ~A\n" expected)
				(format #f "    calculated value: ~A\n" calculated))))
		  (error error-msg)))))

(define (Day-counter-test-afb afb d1 d2 expected)
  (let ((calculated (DayCounter-year-fraction afb d1 d2)))
	(if (not (<= (abs (- calculated expected)) 1.0e-10))
		(let ((error-msg
			   (string-append
				(format #f "AFB day counter:\n")
				(format #f "    first date:       ~A\n" (Date->string d1))
				(format #f "    second date:      ~A\n" (Date->string d2))
				(format #f "    expected value:   ~A\n" expected)
				(format #f "    calculated value: ~A\n" calculated))))
		  (error error-msg)))))


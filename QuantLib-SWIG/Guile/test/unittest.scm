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

(define (make-suite)
  (let ((test-list '()))
	(lambda (command . args)
	  (cond
	   ; add a test to the suite
	   ((eqv? command 'add)
		(set! test-list (append test-list
								(list (cons (car args) (cadr args))))))
	   ; run the test suite
	   ((eqv? command 'run)
		(letrec ((error-list '())
				 (add-error
				  (lambda (test-msg error-msg)
					(set! error-list
						  (append error-list
								  (list (cons test-msg error-msg))))))
				 (run-single-test
				  (lambda (test)
					(letrec ((test-proc (car test))
							 (test-msg (cdr test))
							 (handler
							  (lambda (key . args)
								(display "failed. ") (newline) 
                                (flush-all-ports)
								(add-error test-msg
                                           (apply format 
                                                  (cons #f (cdr args))))))
                             (run-test
                              (lambda ()
                                (display test-msg) (display "... ") 
                                (flush-all-ports)
                                (test-proc)
                                (display "ok. ") (newline) 
                                (flush-all-ports))))
                      (catch #t run-test handler))))
				 (run-all-tests
				  (lambda () (for-each run-single-test test-list))))
          (run-all-tests) ; possible to time this?

		  (display (length test-list)) (display " test(s) run.") (newline)
		  (cond ((null? error-list) (display "All tests passed.") (newline))
				(else
				 (display (length error-list)) (display " failure(s).") 
                 (newline)
				 (for-each
				  (lambda (err)
					(let ((test-msg (car err))
						  (error-msg (cdr err)))
					  (newline)
					  (display test-msg) (display ":") (newline)
					  (display error-msg)))
				  error-list)
				 (newline)))))))))

(define (suite-add-test suite test-proc test-msg)
  (suite 'add test-proc test-msg))
(define (suite-run suite)
  (suite 'run))

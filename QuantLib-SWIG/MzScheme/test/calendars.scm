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

(load "unittest.scm")
(load "common.scm")

(define (Calendar-test)
  (deleting-let ((c1 (new-Calendar "TARGET") delete-Calendar)
                 (c2 (new-Calendar "London") delete-Calendar)
                 (c3 (new-Calendar "NewYork") delete-Calendar)
                 (c4 (new-Calendar "Tokyo") delete-Calendar))
    (deleting-let ((c12h (new-JointCalendar c1 c2 "JoinHolidays")
                         delete-JointCalendar)
                   (c12b (new-JointCalendar c1 c2 "JoinBusinessDays")
                         delete-JointCalendar)
                   (c123h (new-JointCalendar c1 c2 c3 "JoinHolidays")
                          delete-JointCalendar)
                   (c123b (new-JointCalendar c1 c2 c3 "JoinBusinessDays")
                          delete-JointCalendar)
                   (c1234h (new-JointCalendar c1 c2 c3 c4 "JoinHolidays")
                           delete-JointCalendar)
                   (c1234b (new-JointCalendar c1 c2 c3 c4 "JoinBusinessDays")
                           delete-JointCalendar))
      (letrec ((test-single-date
                (lambda (d end)
                  (if (Date<? d end)
                      (let ((b1 (Calendar-is-business-day? c1 d))
                            (b2 (Calendar-is-business-day? c2 d))
                            (b3 (Calendar-is-business-day? c3 d))
                            (b4 (Calendar-is-business-day? c4 d)))
                        (check
                         (equal? (and b1 b2)
                                 (Calendar-is-business-day? c12h d))
                         "At date " (Date->string d) ":" cr
                         "    inconsistency between joint calendar"
                         (Calendar-name c12h) cr
                         "    and its components")
                        (check
                         (equal? (or b1 b2)
                                 (Calendar-is-business-day? c12b d))
                         "At date " (Date->string d) ":" cr
                         "    inconsistency between joint calendar"
                         (Calendar-name c12b) cr
                         "    and its components")
                        (check
                         (equal? (and b1 b2 b3)
                                 (Calendar-is-business-day? c123h d))
                         "At date " (Date->string d) ":" cr
                         "    inconsistency between joint calendar"
                         (Calendar-name c123h) cr
                         "    and its components")
                        (check
                         (equal? (or b1 b2 b3)
                                 (Calendar-is-business-day? c123b d))
                         "At date " (Date->string d) ":" cr
                         "    inconsistency between joint calendar"
                         (Calendar-name c123b) cr
                         "    and its components")
                        (check
                         (equal? (and b1 b2 b3 b4)
                                 (Calendar-is-business-day? c1234h d))
                         "At date " (Date->string d) ":" cr
                         "    inconsistency between joint calendar"
                         (Calendar-name c1234h) cr
                         "    and its components")
                        (check
                         (equal? (or b1 b2 b3 b4)
                                 (Calendar-is-business-day? c1234b d))
                         "At date " (Date->string d) ":" cr
                         "    inconsistency between joint calendar"
                         (Calendar-name c1234b) cr
                         "    and its components")
                        (let ((next (Date-plus-days d 1)))
                          (delete-Date d)
                          (test-single-date next end)))
                      (delete-Date d)))))
        (let* ((first-date (Date-todays-date))
               (end-date (Date-plus-years first-date 1)))
          (test-single-date first-date end-date)
          (delete-Date end-date))))))

(define Calendar-suite
  (make-test-suite 
   "Calendar tests"
   (make-test-case/msg "Testing joint calendars" (Calendar-test))))


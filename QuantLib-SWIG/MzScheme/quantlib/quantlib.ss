
(require-library "file.ss" "dynext")
(load-extension (build-path (collection-path "quantlib") 
                            (append-extension-suffix "QuantLibc")))

; more scheme-like names which could'n be set from SWIG

(define calendar=? calendar-equal)

(define date=?  date-equal)
(define date<?  date-less)
(define date>?  date-greater)
(define date<=? date-less-equal)
(define date>=? date-greater-equal)

(define daycounter=? daycounter-equal)

(define samplenumber-value  samplenumber-value-get)
(define samplenumber-weight samplenumber-weight-get)

; added functionality
(define Calendar-advance-units Calendar-advance)
(define (Calendar-advance . args)
  (if (integer? (caddr args))
      (apply Calendar-advance-units args)
      (apply Calendar-advance-period args)))

(define (History-map h f)
  (let ((results '()))
    (History-for-each h (lambda (e)
                          (if e
                              (set! results (cons (f e) results))
                              (set! results (cons #f results)))))
    (reverse results)))
(define (History-map-valid h f)
  (let ((results '()))
    (History-for-each-valid h (lambda (e)
                                (set! results (cons (f e) results))))
    (reverse results)))


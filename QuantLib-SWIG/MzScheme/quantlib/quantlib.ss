
(require-library "file.ss" "dynext")
(load-extension (build-path (collection-path "quantlib") 
                            (append-extension-suffix "QuantLibc")))

; macros for making it easier to free memory
; careful: they could prevent tail-recursion!
(define-macro deleting-let
  (lambda (bindings . body)
    (let ((thunk (gensym))
          (result (gensym)))
      `(let ,(map (lambda (b) (list (car b) (cadr b))) bindings)
         (define ,thunk (lambda () ,@body))
         (let ((,result (,thunk)))
           ,@(map (lambda (b) (list (caddr b) (car b))) bindings)
           ,result)))))

(define-macro deleting-let*
  (lambda (bindings . body)
    (let ((thunk (gensym))
          (result (gensym)))
      `(let* ,(map (lambda (b) (list (car b) (cadr b))) bindings)
         (define ,thunk (lambda () ,@body))
         (let ((,result (,thunk)))
           ,@(map (lambda (b) (list (caddr b) (car b))) bindings)
           ,result)))))

(define (do-not-delete x) #f)

; more scheme-like names which couldn't be set from SWIG

(define calendar=? calendar-equal)

(define date=?  date-equal)
(define date<?  date-less)
(define date>?  date-greater)
(define date<=? date-less-equal)
(define date>=? date-greater-equal)

(define daycounter=? daycounter-equal)

(define samplenumber-value  samplenumber-value-get)
(define samplenumber-weight samplenumber-weight-get)

(define tridiagonaloperator+ tridiagonaloperator-add)
(define tridiagonaloperator- tridiagonaloperator-sub)
(define tridiagonaloperator* tridiagonaloperator-mul)
(define tridiagonaloperator/ tridiagonaloperator-div)

; added functionality
(define (Calendar-advance . args)
  (if (integer? (caddr args))
      (apply Calendar-advance-units args)
      (apply Calendar-advance-period args)))


(define History-old-init new-History)
(define (new-History dates values)
  (let ((null (null-double)))
    (History-old-init dates
                      (map (lambda (x) (or x null)) values))))
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

(define MarketElementHandle-old-init new-MarketElementHandle)
(define (new-MarketElementHandle . args)
  (let ((h (MarketElementHandle-old-init)))
    (if (not (null? args))
        (MarketElementHandle-link-to! h (car args)))
    h))

(define (TermStructure-discount self x . extrapolate)
  (let ((method #f))
    (if (number? x)
        (set! method TermStructure-discount-vs-time)
        (set! method TermStructure-discount-vs-date))
    (apply method (cons self (cons x extrapolate)))))
(define (TermStructure-zero-yield self x . extrapolate)
  (let ((method #f))
    (if (number? x)
        (set! method TermStructure-zeroYield-vs-time)
        (set! method TermStructure-zeroYield-vs-date))
    (apply method (cons self (cons x extrapolate)))))
(define (TermStructure-forward self x . extrapolate)
  (let ((method #f))
    (if (number? x)
        (set! method TermStructure-forward-vs-time)
        (set! method TermStructure-forward-vs-date))
    (apply method (cons self (cons x extrapolate)))))

(define TermStructureHandle-old-init new-TermStructureHandle)
(define (new-TermStructureHandle . args)
  (let ((h (TermStructureHandle-old-init)))
    (if (not (null? args))
        (TermStructureHandle-link-to! h (car args)))
    h))

(define FlatForward-old-init new-FlatForward)
(define (new-FlatForward currency dayCounter today settlement forward)
  (if (number? forward)
      (letrec ((m (new-SimpleMarketElement forward))
               (h (new-MarketElementHandle m))
               (ff (FlatForward-old-init currency dayCounter 
                                         today settlement h)))
        (delete-MarketElementHandle h)
        (delete-MarketElement m)
        ff)
      (FlatForward-old-init currency dayCounter today settlement forward)))

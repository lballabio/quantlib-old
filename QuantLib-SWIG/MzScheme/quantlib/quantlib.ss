
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

; utilities

(define (list->array l)
  (define (loop l a i)
    (if (= i (array-length a))
        a
        (begin
          (array-set! a i (car l))
          (loop (cdr l) a (+ i 1)))))
  (loop l (new-array (length l)) 0))

(define (vector->array v)
  (define (loop v a i)
    (if (= i (array-length a))
        a
        (begin
          (array-set! a i (vector-ref v i))
          (loop v a (+ i 1)))))
  (loop v (new-array (vector-length v)) 0))

(define (array->list a)
  (define (loop a l i)
    (if (= i (array-length a))
        (reverse l)
        (loop a (cons (array-ref a i) l) (+ i 1))))
  (loop a '() 0))

(define (array->vector a)
  (define (loop a v i)
    (if (= i (array-length a))
        v
        (begin
          (vector-set! v i (array-ref a i))
          (loop a v (+ i 1)))))
  (loop a (make-vector (array-length a)) 0))



(define (round-with-digits x d)
  (if (not (number? x))
      x
      (let ((b (expt 10.0 d)))
        (/ (round (* b x)) b))))

(define (right-fill s width)
  (let ((l (string-length s)))
    (if (>= l width)
        s
        (string-append s (make-string (- width l) #\space)))))

(define (left-fill s width)
  (let ((l (string-length s)))
    (if (>= l width)
        s
        (string-append (make-string (- width l) #\space) s))))

(define (tabulate fmt sep . args)
  (define (tabulate-iter fmt sep args accum)
    (if (null? fmt)
        accum
        (let* ((f1 (car fmt))
               (a1 (car args))
               (rounded (cond ((null? (cddr f1)) a1)
                              (else (round-with-digits a1 (caddr f1)))))
               (str (format "~a" rounded))
               (filled (case (cadr f1)
                         ((r) (left-fill str (car f1)))
                         ((l) (right-fill str (car f1))))))
          (tabulate-iter (cdr fmt) sep (cdr args)
                         (if (= (string-length accum) 0)
                             filled
                             (string-append accum sep filled))))))
  (tabulate-iter fmt sep args ""))


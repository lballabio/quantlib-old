
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


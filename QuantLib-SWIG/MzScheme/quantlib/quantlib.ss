
(require-library "file.ss" "dynext")
(load-extension (build-path (collection-path "quantlib") 
                            (append-extension-suffix "QuantLibc")))

; more scheme-like names

(define date->string date-str)
(define date=?       date-equal)
(define date<?       date-less)
(define date>?       date-greater)
(define date<=?      date-less-equal)
(define date>=?      date-greater-equal)

(define calendar->string          calendar-str)
(define calendar=?                calendar-equal)
(define calendar-is-business-day? calendar-is-business-day)
(define calendar-is-holiday?      calendar-is-holiday)

(define daycounter->string        daycounter-str)
(define daycounter=?              daycounter-equal)


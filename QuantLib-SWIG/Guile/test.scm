
(set! %load-path (cons (getcwd) %load-path))
(chdir "./test")
(load "quantlib-test-suite.scm")
(chdir "..")
(set! %load-path (cdr %load-path))


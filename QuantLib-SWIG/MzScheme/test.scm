
(current-library-collection-paths
  (cons (current-directory) (current-library-collection-paths)))
(current-directory "./test")
(load "quantlib-test-suite.scm")
(current-directory "..")
(current-library-collection-paths
  (cdr (current-library-collection-paths)))


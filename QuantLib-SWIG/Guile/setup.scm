
; Copyright (C) 2002, 2003 RiskMap srl
;
; This file is part of QuantLib, a free-software/open-source library
; for financial quantitative analysts and developers - http://quantlib.org/
;
; QuantLib is free software: you can redistribute it and/or modify it under the
; terms of the QuantLib license.  You should have received a copy of the
; license along with this program; if not, please email quantlib-dev@lists.sf.net
; The license is also available online at http://quantlib.org/html/license.html
;
; This program is distributed in the hope that it will be useful, but WITHOUT
; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
; FOR A PARTICULAR PURPOSE.  See the license for more details.

; usage
(define (usage)
  (format #t "Usage: guile -s setup scm command\n")
  (format #t "  Commands:\n")
  (format #t "    wrap             generate wrappers from SWIG interfaces\n")
  (format #t "    build            build QuantLib-Guile\n")
  (format #t "    test             test QuantLib-Guile\n")
  (format #t "    install          install QuantLib-Guile\n")
  (format #t "    sdist            create source distribution\n")
  (format #t "    clean            clean up\n")
  (exit))


; current QuantLib version
(define version "0.3.7")

; files
(define info-files
  (list "Authors.txt" "ChangeLog.txt" "Contributors.txt"
        "LICENSE.TXT" "History.txt" "News.txt" "README.txt"))
(define source-files
  (list "QuantLib.scm" "quantlib_wrap.cpp"))
(define binary-files
  (list "QuantLibc.so"))
(define scripts
  (list "setup.scm"))
(define SWIG-interfaces
  (list "quantlib.i"
        "ql.i"
        "common.i"
        "blackmodel.i"
        "calendars.i"
        "capfloor.i"
        "cashflows.i"
        "compoundforward.i"
        "currencies.i"
        "date.i"
        "daycounters.i"
        "diffusionprocess.i"
        "discountcurve.i"
        "distributions.i"
        "exercise.i"
        "functions.i"
        "history.i"
        "indexes.i"
        "instruments.i"
        "integrals.i"
        "interpolation.i"
        "linearalgebra.i"
        "marketelements.i"
        "montecarlo.i"
        "null.i"
        "observer.i"
        "operators.i"
        "optimizers.i"
        "options.i"
        "payoffs.i"
        "piecewiseflatforward.i"
        "randomnumbers.i"
        "scheduler.i"
        "shortratemodels.i"
        "statistics.i"
        "stochasticprocess.i"
        "swap.i"
        "swaption.i"
        "termstructures.i"
        "timebasket.i"
        "types.i"
        "vectors.i"
        "volatilities.i"
        ; to be removed
        "old_pricers.i"
        "old_volatility.i"))
(define test-files
  (list "quantlib-test-suite.scm"
        "instruments.scm"
        "integrals.scm"
        "marketelements.scm"
        "solvers1d.scm"
        "termstructures.scm"
        ; support files
        "common.scm"
        "unittest.scm"
        "utilities.scm"))

(define (ls path)
  (define (read-one s acc)
    (let ((x (readdir s)))
      (cond ((eof-object? x)
             acc)
            ((or (string=? x ".") (string=? x ".."))
             (read-one s acc))
            (else
             (read-one s (cons x acc))))))
  (let ((s (opendir path)))
    (let ((l (read-one s '())))
      (closedir s)
      l)))

(define (rec-delete-directory dir)
  (define (delete-item item)
    (if (file-is-directory? item)
        (rec-delete-directory item)
        (delete-file item)))
  (chdir dir)
  (for-each delete-item (ls "."))
  (chdir "..")
  (rmdir dir))

; commands

(define (wrap)
  (display "Generating Guile bindings for QuantLib...") (newline)
  (let ((swig-dir "./SWIG"))
    (if (not (file-exists? swig-dir))
        (set! swig-dir "../SWIG"))
    (system (string-append "swig -guile -c++ -Linkage simple "
                           "-scmstub "
                           (format #f "-I~A " swig-dir)
                           "-o quantlib_wrap.cpp "
                           "quantlib.i"))))

(define (build)
  (define (string-split s)
    (let ((n (string-length s))
          (spcs '()))
      (do ((i 0 (+ i 1)))
          ((= i n))
        (if (char=? (string-ref s i) #\space)
            (set! spcs (cons i spcs))))
      (let ((begins (cons 0 (map (lambda (i) (+ i 1))
                                 (reverse spcs))))
            (ends (reverse (cons n spcs))))
        (map (lambda (b e) (substring s b e)) begins ends))))
  (display "Building QuantLib-Guile...") (newline)
  (let ((c++-compiler (getenv "CXX"))
        (c-flags (getenv "CFLAGS"))
        (c++-flags (getenv "CXXFLAGS")))
    (if (not c++-compiler) (set! c++-compiler "g++"))
    (let ((flags '("-DHAVE_CONFIG_H" "-c" "-fpic"
                   "-I/usr/include" "-I/usr/local/include")))
      (if c-flags
          (set! flags (append flags (string-split c-flags))))
      (if c++-flags
          (set! flags (append flags (string-split c++-flags))))
      (let ((command (apply string-append
                            (map
                             (lambda (s) (string-append s " "))
                             (append (list c++-compiler)
                                     flags
                                     (list "quantlib_wrap.cpp"))))))
        (display command) (newline)
        (system command)))
    (let ((command (string-append c++-compiler " -shared "
                         "quantlib_wrap.o "
                         "-L/usr/local/lib "
                         "-lQuantLib "
                         "-o QuantLibc.so")))
      (display command) (newline)
      (system command))))

(define (test)
  (load "test/quantlib-test-suite.scm")
  (format #t "Testing QuantLib-Guile ~A\n" version)
  (let ((this-dir (getcwd)))
    (set! %load-path (cons this-dir %load-path))
    (set! greg-debug #t)
    (set! greg-tools '("test"))
    (greg-test-run)
    (set! %load-path (cdr %load-path))))

(define (install)
  (define (find-install-path path)
    (if (null? path)
        #f
        (let ((dir (car path))
              (cwd (getcwd)))
          (if (and (file-exists? dir)
                   (file-is-directory? dir)
                   (not (string=? dir "."))
                   (not (string=? dir cwd)))
              dir
              (find-install-path (cdr path))))))
  (display "Installing QuantLib-Guile...") (newline)
  (let ((install-path (find-install-path %load-path)))
    (for-each (lambda (file)
                (let ((destination-file (string-append install-path
                                                       "/"
                                                       file)))
                  (display (string-append
                            "./" file
                            " -> "
                            destination-file))
                  (newline)
                  (system (format #f "install -m 0555 ~A ~A"
                                  file
                                  destination-file))))
              '("QuantLib.scm" "QuantLibc.so"))))

(define (sdist)
  (display "Packing source distribution...") (newline)
  (let ((distribution-dir (string-append "QuantLib-Guile-" version)))
    (if (file-exists? distribution-dir)
        (if (file-is-directory? distribution-dir)
            (rec-delete-directory distribution-dir)
            (delete-file distribution-dir)))
    (let ((swig-dir (string-append distribution-dir "/SWIG"))
          (test-dir (string-append distribution-dir "/test")))
      (define (install-files files source-dir target-dir)
        (for-each
         (lambda (f)
           (let ((source-file (string-append source-dir "/" f))
                 (destination-file (string-append distribution-dir "/"
                                                  target-dir "/" f)))
             (copy-file source-file destination-file)))
         files))
      (mkdir distribution-dir)
      (for-each mkdir (list swig-dir test-dir))
      (install-files info-files "." ".")
      (install-files source-files "." ".")
      (install-files scripts "." ".")
      (let ((i-dir "SWIG"))
        (if (not (file-exists? i-dir))
            (set! i-dir "../SWIG"))
        (install-files SWIG-interfaces i-dir "SWIG"))
      (install-files test-files "test" "test")
      (system (string-append "tar cfz "
                             distribution-dir ".tar.gz "
                             distribution-dir))
      (rec-delete-directory distribution-dir))))

(define (clean)
  (define (clean-file file)
    (if (file-exists? file)
        (delete-file file)))
  (for-each clean-file
            '("QuantLib.scm" "QuantLibc.so" "quantlib_wrap.cpp"
              "quantlib_wrap.o")))


(define available-commands
  (list (cons "wrap"    wrap)
        (cons "build"   build)
        (cons "test"    test)
        (cons "install" install)
        (cons "sdist"   sdist)
        (cons "clean"   clean)))

; parse command line
(let ((argv (command-line)))
  (if (not (= (length argv) 2))
      (usage))
  (let ((command (assoc (cadr argv) available-commands)))
    (if command
        ((cdr command))
        (usage))))


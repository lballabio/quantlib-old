
; Copyright (C) 2002, 2003 RiskMap srl
; Copyright (C) 2003, 2004, 2005, 2006, 2007 StatPro Italia srl
;
; This file is part of QuantLib, a free-software/open-source library
; for financial quantitative analysts and developers - http://quantlib.org/
;
; QuantLib is free software: you can redistribute it and/or modify it under the
; terms of the QuantLib license.  You should have received a copy of the
; license along with this program; if not, please email
; <quantlib-dev@lists.sf.net>. The license is also available online at
; <http://quantlib.org/license.shtml>.
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
  (exit))

; current QuantLib version
(define version "1.7")

; commands

(define (wrap)
  (let ((swig-dir "../SWIG"))
    (let ((command (string-append "swig -guile -c++ -Linkage passive "
                                  "-scmstub -gh "
                                  (format #f "-I~A " swig-dir)
                                  "-o quantlib_wrap.cpp "
                                  "quantlib.i")))
      (display command) (newline)
      (system command))))

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
  (let ((c++-compiler (getenv "CXX"))
        (c-flags (getenv "CFLAGS"))
        (c++-flags (getenv "CXXFLAGS")))
    (if (not c++-compiler) (set! c++-compiler "g++"))
    (if (not c++-flags) (set! c++-flags "-g -O2"))
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


(define available-commands
  (list (cons "wrap"    wrap)
        (cons "build"   build)
        (cons "test"    test)
        (cons "install" install)))

; parse command line
(let ((argv (command-line)))
  (if (not (= (length argv) 2))
      (usage))
  (let ((command (assoc (cadr argv) available-commands)))
    (if command
        ((cdr command))
        (usage))))


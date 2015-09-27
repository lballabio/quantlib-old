
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

(require (lib "compile.ss" "dynext"))
(require (lib "link.ss" "dynext"))
(require (lib "file.ss" "dynext"))

; usage
(define (usage)
  (display
   (string-append
    (format "Usage: mzscheme -r setup scm command~n")
    (format "  Commands:~n")
    (format "    wrap             generate wrappers from SWIG interfaces~n")
    (format "    build            build QuantLib-MzScheme~n")
    (format "    test             test QuantLib-MzScheme~n")
    (format "    install          install QuantLib-MzScheme~n")))
  (exit))


; current QuantLib version
(define version "1.7")

; utilities
(define (string-split s c)
  (let ((n (string-length s))
        (spcs '()))
    (do ((i 0 (+ i 1)))
        ((= i n))
      (if (char=? (string-ref s i) c)
          (set! spcs (cons i spcs))
          '()))
    (let ((begins (cons 0 (map (lambda (i) (+ i 1))
                               (reverse spcs))))
          (ends (reverse (cons n spcs))))
      (map (lambda (b e) (substring s b e)) begins ends))))

(define (execute prog . args)
  (let ((full-path (find-executable-path
                    (if (eqv? (system-type) 'windows)
                        (string-append prog ".exe")
                        prog)
                    #f))
        (stdin (current-input-port))
        (stdout (current-output-port))
        (stderr (current-output-port)))
    (call-with-values (lambda () (apply subprocess
                                        stdout stdin stderr full-path args))
      (lambda (proc _1 _2 _3) (subprocess-wait proc)))))

; commands

(define (wrap)
  (let ((swig-dir "../SWIG"))
    (execute "swig" "-mzscheme" "-c++"
             (string-append "-I" swig-dir)
             "-o" "quantlib_wrap.cpp"
             "quantlib.i")))

(define (build)
  (let ((platform (system-type))
        (include-dirs '()))
    (cond ((eqv? platform 'unix)
           (let ((c++-compiler (getenv "CXX"))
                 (c-flags (getenv "CFLAGS"))
                 (c++-flags (getenv "CXXFLAGS")))
             (if (not c++-compiler)
                 (set! c++-compiler (find-executable-path "g++" #f))
                 '())
             (current-extension-compiler c++-compiler)
             (current-extension-linker c++-compiler)
             (if c-flags
                 (current-extension-compiler-flags
                  (append
                   (current-extension-compiler-flags)
                   (string-split c-flags #\space)))
                 '())
             (if c++-flags
                 (current-extension-compiler-flags
                  (append
                   (current-extension-compiler-flags)
                   (string-split c++-flags #\space)))
                 '())
             (current-extension-compiler-flags
                  (append
                   (current-extension-compiler-flags)
                   (list "-Wno-return-type" "-fpermissive")))
             (current-extension-linker-flags
              (append
               (current-extension-linker-flags)
               (list "-L/usr/local/lib" "-lstdc++" "-lgcc"
                     (string-append "-lQuantLib"))))))
          ((eqv? platform 'windows)
           (let ((ql-dir (getenv "QL_DIR")))
             (if ql-dir
                 (begin
                   (set! include-dirs (cons ql-dir include-dirs))
                   (putenv "LIB"
                           (string-append
                            (build-path ql-dir "lib")
                            ";"
                            (getenv "LIB"))))
                 (display
                  (string-append
                   (format "warning: unable to detect QuantLib installation~n")
                   (format "I will assume that it was added ")
                   (format "to the default compiler paths~n")))))
           (current-extension-compiler-flags
            (append
             (current-extension-compiler-flags)
             (list "-DNOMINMAX" "/MD" "/GR" "/GX" "/Zm250"))))
          (else
           (error "Unsupported platform")))
    (let ((object (append-object-suffix "quantlib_wrap"))
          (extension (build-path "quantlib"
                                 (append-extension-suffix "QuantLibc"))))
      (compile-extension #f "quantlib_wrap.cpp" object include-dirs)
      (link-extension #f (list object) extension))))

(define (test)
  (display (format "Testing QuantLib-MzScheme ~a~n" version))
  (current-library-collection-paths
   (cons (current-directory) (current-library-collection-paths)))
  (current-directory "./test")
  (load "quantlib-test-suite.scm")
  (current-directory "..")
  (current-library-collection-paths
   (cdr (current-library-collection-paths))))

(define (install)
  (let-values (((collect-path _1 _2) (split-path (collection-path "mzlib"))))
              (let ((installation-path (build-path collect-path "quantlib")))
                (if (not (directory-exists? installation-path))
                    (make-directory installation-path)
                    '())
                (for-each
                 (lambda (f)
                   (let* ((destination-file
                           (build-path installation-path f)))
                     (if (file-exists? destination-file)
                         (delete-file destination-file)
                         '())
                     (display (build-path "." "quantlib" f))
                     (display " -> ")
                     (display destination-file)
                     (newline)
                     (copy-file (build-path "quantlib" f)
                                destination-file)))
                 (list "quantlib.ss"
                       "ql-init.ss"
                       (append-extension-suffix "QuantLibc"))))))

(define available-commands
  (list (cons "wrap"    wrap)
        (cons "build"   build)
        (cons "test"    test)
        (cons "install" install)))

; parse command line
(let ((argv (current-command-line-arguments)))
  (if (not (= (vector-length argv) 1))
      (usage) '())
  (let ((command (assoc (vector-ref argv 0) available-commands)))
    (if command
        ((cdr command))
        (usage))))


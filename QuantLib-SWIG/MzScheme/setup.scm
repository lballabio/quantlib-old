
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
    (format "    install          install QuantLib-MzScheme~n")
    (format "    sdist            create source distribution~n")
    ;(format "    bdist            create binary distribution~n")
    ))
  (exit))


; current QuantLib version
(define version "0.3.10")

; files
(define info-files
  (list "Authors.txt" "ChangeLog.txt" "Contributors.txt"
        "LICENSE.TXT" "News.txt" "README.txt"))
(define source-files
  (list (build-path "quantlib" "quantlib.ss")
        (build-path "quantlib" "ql-init.ss")
        "quantlib_wrap.cpp"))
(define binary-files
  (list (build-path "quantlib" (append-extension-suffix "QuantLibc"))))
(define scripts
  (list "setup.scm"))
(define SWIG-interfaces
  (list "quantlib.i"
        "ql.i"
        "common.i"
        "blackmodel.i"
        "bonds.i"
        "calendars.i"
        "capfloor.i"
        "cashflows.i"
        "compoundforward.i"
        "currencies.i"
        "date.i"
        "daycounters.i"
        "stochasticprocess.i"
        "discountcurve.i"
        "distributions.i"
        "exchangerates.i"
        "exercise.i"
        "functions.i"
        "grid.i"
        "history.i"
        "indexes.i"
        "instruments.i"
        "integrals.i"
        "interestrate.i"
        "interpolation.i"
        "linearalgebra.i"
        "marketelements.i"
        "money.i"
        "montecarlo.i"
        "null.i"
        "observer.i"
        "operators.i"
        "optimizers.i"
        "options.i"
        "payoffs.i"
        "piecewiseflatforward.i"
        "randomnumbers.i"
        "rounding.i"
        "scheduler.i"
        "settings.i"
        "shortratemodels.i"
        "statistics.i"
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
  (list "common.scm"
        "instruments.scm"
        "integrals.scm"
        "marketelements.scm"
        "solvers1d.scm"
        "termstructures.scm"
        ; test support
        "unittest.scm"
        "utilities.scm"
        "quantlib-test-suite.scm"))
(define example-files
  (list "american-option.scm"
        "bermudan-swaption.scm"
        "european-option.scm"
        "swap.scm"
        ; support
        "tabulate.scm"))

; utilities
(define (string-split s c)
  (let ((n (string-length s))
        (spcs '()))
    (do ((i 0 (+ i 1)))
        ((= i n))
      (if (char=? (string-ref s i) c)
          (set! spcs (cons i spcs))))
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

(define (rec-delete-directory dir)
  (define (delete-item item)
    (if (file-exists? item)
        (delete-file item)
        (rec-delete-directory item)))
  (current-directory dir)
  (for-each delete-item (directory-list "."))
  (current-directory "..")
  (delete-directory dir))

; commands

(define (wrap)
  (display "Generating MzScheme bindings for QuantLib...") (newline)
  (let ((swig-dir "./SWIG"))
    (if (not (directory-exists? swig-dir))
        (set! swig-dir "../SWIG"))
    (execute "swig" "-mzscheme" "-c++"
             (string-append "-I" swig-dir)
             "-o" "quantlib_wrap.cpp"
             "quantlib.i")))

(define (build)
  (display "Building QuantLib-MzScheme...") (newline)
  (let ((platform (system-type))
        (include-dirs '()))
    (cond ((eqv? platform 'unix)
           (let ((c++-compiler (getenv "CXX"))
                 (c-flags (getenv "CFLAGS"))
                 (c++-flags (getenv "CXXFLAGS")))
             (if (not c++-compiler)
                 (set! c++-compiler (find-executable-path "g++" #f)))
             (current-extension-compiler c++-compiler)
             (current-extension-linker c++-compiler)
             (if c-flags
                 (current-extension-compiler-flags
                  (append
                   (current-extension-compiler-flags)
                   (string-split c-flags #\space))))
             (if c++-flags
                 (current-extension-compiler-flags
                  (append
                   (current-extension-compiler-flags)
                   (string-split c++-flags #\space))))
           (current-extension-linker-flags
            (append
             (current-extension-linker-flags)
             (list "-L/usr/local/lib" "-lstdc++" "-lgcc"
                   (string-append "-lQuantLib-" version))))))
          ((eqv? platform 'windows)
           (set! include-dirs (cons (getenv "QL_DIR") include-dirs))
           (current-extension-compiler-flags
            (append
             (current-extension-compiler-flags)
             (list "-DNOMINMAX" "/MT" "/GR" "/GX" "/Zm150")))
           (putenv "LIB"
                   (string-append
                    (build-path (getenv "QL_DIR") "lib")
                    ";"
                    (getenv "LIB"))))
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
  (display "Installing QuantLib-MzScheme...") (newline)
  (let-values (((collect-path _1 _2) (split-path (collection-path "mzlib"))))
              (let ((installation-path (build-path collect-path "quantlib")))
                (if (not (directory-exists? installation-path))
                    (make-directory installation-path))
                (for-each
                 (lambda (f)
                   (let* ((destination-file
                           (build-path installation-path f)))
                     (if (file-exists? destination-file)
                         (delete-file destination-file))
                     (display (string-append
                               (build-path "." "quantlib" f)
                               " -> "
                               destination-file))
                     (newline)
                     (copy-file (build-path "quantlib" f)
                                destination-file)))
                 (list "quantlib.ss"
                       "ql-init.ss"
                       (append-extension-suffix "QuantLibc"))))))

(define (sdist)
  (display "Packing source distribution...") (newline)
  (let ((distribution-dir (string-append "QuantLib-MzScheme-" version)))
    (if (directory-exists? distribution-dir)
        (rec-delete-directory distribution-dir))
    (let ((swig-dir (build-path distribution-dir "SWIG"))
          (test-dir (build-path distribution-dir "test"))
          (example-dir (build-path distribution-dir "examples"))
          (module-dir (build-path distribution-dir "quantlib")))
      (define (install-files files source-dir target-dir)
        (for-each
         (lambda (f)
           (let ((source-file (build-path source-dir f))
                 (destination-file (build-path distribution-dir target-dir f)))
             (copy-file source-file destination-file)))
         files))
      (for-each make-directory
                (list distribution-dir swig-dir test-dir
                      example-dir module-dir))
      (install-files info-files "." ".")
      (install-files source-files "." ".")
      (install-files scripts "." ".")
      (let ((i-dir "./SWIG"))
        (if (not (directory-exists? i-dir))
            (set! i-dir "../SWIG"))
        (install-files SWIG-interfaces i-dir "SWIG"))
      (install-files test-files "./test" "test")
      (install-files example-files "./examples" "examples")
      (let ((os (system-type)))
        (cond ((equal? os 'unix)
               (execute "tar" "cfz"
                        (string-append distribution-dir ".tar.gz")
                        distribution-dir))
              ((equal? os 'windows)
               (execute "zip" "-q" "-r"
                        (string-append distribution-dir ".zip")
                        distribution-dir))
              (else
               (error (string-append
                       "unsupported host: "
                       (symbol->string os))))))
      (rec-delete-directory distribution-dir))))


; BDist = Command.new {
;	Wrap.execute
;	Build.execute
;	puts "Packing binary distribution..."
;	distDir = "QuantLib-Ruby-#{Version}"
;	raise "Directory #{distDir} already exist" if File.exists? distDir
;	swigDir = distDir+"/SWIG"
;	testDir = distDir+"/test"
;	[distDir,swigDir,testDir].each { |path| File.makedirs path }
;	Info.each       { |file| File.syscopy file, distDir }
;	Sources.each    { |file| File.syscopy file, distDir }
;	Binaries.each   { |file| File.syscopy file, distDir }
;	Scripts.each    { |file| File.syscopy file, distDir }
;	Interfaces.each { |file| File.syscopy '../SWIG/'+file, swigDir }
;	Tests.each      { |file| File.syscopy 'test/'+file, testDir }
;	system "tar cfz #{distDir}.#{Config::CONFIG['arch']}.tar.gz #{distDir}/"
; }



(define available-commands
  (list (cons "wrap"    wrap)
        (cons "build"   build)
        (cons "test"    test)
        (cons "install" install)
        (cons "sdist"   sdist)))

; parse command line
(if (not (= (vector-length argv) 1))
    (usage))

(let ((command (assoc (vector-ref argv 0) available-commands)))
  (if command
      ((cdr command))
      (usage)))



; Copyright (C) 2000, 2001, 2002 RiskMap srl
;
; This file is part of QuantLib, a free-software/open-source library
; for financial quantitative analysts and developers - http://quantlib.org/
;
; QuantLib is free software: you can redistribute it and/or modify it under the
; terms of the QuantLib license.  You should have received a copy of the
; license along with this program; if not, please email ferdinando@ametrano.net
; The license is also available online at http://quantlib.org/html/license.html
;
; This program is distributed in the hope that it will be useful, but WITHOUT
; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
; FOR A PARTICULAR PURPOSE.  See the license for more details.

; $Id$

(require-library "compile.ss" "dynext")
(require-library "link.ss" "dynext")
(require-library "file.ss" "dynext")

; usage
(define (usage)
  (display
   (string-append
    (format "Usage: mzscheme -r setup scm command~n")
    (format "  Commands:~n")
    (format "    wrap             generate wrappers from SWIG interfaces~n")
    (format "    build            build QuantLib-MzScheme~n")
    (format "    install          install QuantLib-MzScheme~n")
    (format "    sdist            create source distribution~n")
    (format "    bdist            create binary distribution~n")))
  (exit))


; current QuantLib version
(define version "0.3.1a0-cvs")

; files
(define info-files
  (list "Authors.txt" "ChangeLog.txt" "Contributors.txt"
        "LICENSE.TXT" "History.txt" "README.txt"))
(define source-files
  (list (build-path "quantlib" "quantlib.ss")
        "quantlib_wrap.cpp"))
(define binary-files
  (list (build-path "quantlib" (append-extension-suffix "QuantLibc"))))
(define scripts
  (list "setup.scm"))
(define SWIG-interfaces
  (list "common.i"
        "calendars.i"
        "currencies.i"
        "date.i"
        "daycounters.i"
        "distributions.i"
        "functions.i"
        "instruments.i"
        "history.i"
        "marketelements.i"
        "null.i"
        "observer.i"
        "quantlib.i"
        "ql.i"
        "qlarray.i"
        "randomnumbers.i"
        "riskstatistics.i"
        "solvers1d.i"
        "types.i"
        "vectors.i"))
(define test-files
  (list "dates.scm"
        "daycounters.scm"
        "distributions.scm"
        "instruments.scm"
        "marketelements.scm"
        "riskstatistics.scm"
        "solvers1d.scm"
        "quantlib-test-suite.scm"))

; commands

(define (wrap)
  (display "Generating MzScheme bindings for QuantLib...") (newline)
  (let ((swig-dir "./SWIG"))
    (if (not (directory-exists? swig-dir))
        (set! swig-dir "../SWIG"))
    (system (string-append 
             "swig -mzscheme -c++ "
             (format "-I~a " swig-dir)
             "-o quantlib_wrap.cpp quantlib.i"))))

(define (build)
  (display "Building QuantLib-MzScheme...") (newline)
  (let ((platform (system-type))
        (include-dirs '()))
    (cond ((eqv? platform 'unix)
           (current-extension-compiler "/usr/bin/g++")
           (current-extension-linker "/usr/bin/g++")
           (current-extension-linker-flags
            (append
             (current-extension-linker-flags)
             (list "-L/usr/local/lib" "-lstdc++" "-lgcc" "-lQuantLib"))))
          ((eqv? platform 'windows)
           (set! include-dirs (cons (getenv "QL_DIR") include-dirs))
           (current-extension-compiler-flags
            (append
             (current-extension-compiler-flags)
             (list "-DNOMINMAX" "/MD" "/GR" "/GX")))
           (putenv "LIB"
                   (string-append
                    (build-path (getenv "QL_DIR") "lib" "Win32" "VisualStudio")
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
                 (lambda (source-file)
                   (let ((destination-file 
                          (build-path installation-path source-file)))
                     (if (file-exists? destination-file)
                         (delete-file destination-file))
                     (copy-file (build-path "quantlib" source-file) 
                                destination-file)))
                 (list "quantlib.ss" 
                       (append-extension-suffix "QuantLibc"))))))
; 	Info.each { |file| File.install "./#{file}",docDir+"/#{file}",nil,true }
; 	Interfaces.each { |file| File.install "../SWIG/"+file,swigDir+"/#{file}",nil,true }
; 	Tests.each { |file| File.install "./test/"+file,testDir+"/#{file}",nil,true }
; }


; SDist = Command.new {
; 	Wrap.execute
; 	puts "Packing source distribution..."
; 	distDir = "QuantLib-Ruby-#{Version}"
; 	raise "Directory #{distDir} already exist" if File.exists? distDir
; 	swigDir = distDir+"/SWIG"
; 	testDir = distDir+"/test"
; 	[distDir,swigDir,testDir].each { |path| File.makedirs path }
; 	Info.each       { |file| File.syscopy file, distDir }
; 	Sources.each    { |file| File.syscopy file, distDir }
; 	Scripts.each    { |file| File.syscopy file, distDir }
; 	Interfaces.each { |file| File.syscopy '../SWIG/'+file, swigDir }
; 	Tests.each      { |file| File.syscopy 'test/'+file, testDir }
; 	cfg = Config::MAKEFILE_CONFIG
; 	case cfg['host_os']
; 	  when 'mswin32'
;     	system "zip -q -r #{distDir}.zip #{distDir}/"
; 	  when 'linux'
;     	system "tar cfz #{distDir}.tar.gz #{distDir}/"
;       else
;         puts "Unknown host: " + cfg['host_os']
;     end
; }

; BDist = Command.new {
; 	Wrap.execute
; 	Build.execute
; 	puts "Packing binary distribution..."
; 	distDir = "QuantLib-Ruby-#{Version}"
; 	raise "Directory #{distDir} already exist" if File.exists? distDir
; 	swigDir = distDir+"/SWIG"
; 	testDir = distDir+"/test"
; 	[distDir,swigDir,testDir].each { |path| File.makedirs path }
; 	Info.each       { |file| File.syscopy file, distDir }
; 	Sources.each    { |file| File.syscopy file, distDir }
; 	Binaries.each   { |file| File.syscopy file, distDir }
; 	Scripts.each    { |file| File.syscopy file, distDir }
; 	Interfaces.each { |file| File.syscopy '../SWIG/'+file, swigDir }
; 	Tests.each      { |file| File.syscopy 'test/'+file, testDir }
; 	system "tar cfz #{distDir}.#{Config::CONFIG['arch']}.tar.gz #{distDir}/"
; }



(define available-commands
  (list (cons "wrap"    wrap)
        (cons "build"   build)
        (cons "test"    test)
        (cons "install" install)))

; parse command line
(if (not (= (vector-length argv) 1))
    (usage))

(let ((command (assoc (vector-ref argv 0) available-commands)))
  (if command
      ((cdr command))
      (usage)))


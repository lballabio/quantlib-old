
(require-library "compile.ss" "dynext")
(require-library "link.ss" "dynext")
(require-library "file.ss" "dynext")

(system "swig -mzscheme -c++ -I../SWIG -o quantlib_wrap.cpp quantlib.i")

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
	(link-extension #f (list object) extension)))

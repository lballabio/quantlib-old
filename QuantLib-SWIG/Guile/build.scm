
(display "Generating wrappers for QuantLib...")
(newline)
(system (string-append "swig -guile -c++ -Linkage simple "
                       "-scmstub QuantLib.scm "
                       "-I../SWIG "
                       "-o quantlib_wrap.cpp "
                       "quantlib.i"))

(display "Compiling...")
(newline)
(system (string-append "g++ -DHAVE_CONFIG_H -c -fpic "
                       "-I/usr/include -I/usr/local/include "
                       "quantlib_wrap.cpp"))

(display "Linking...")
(newline)
(system (string-append "g++ -shared "
                       "quantlib_wrap.o "
                       "-L/usr/local/lib "
                       "-lQuantLib "
                       "-o QuantLibc.so"))

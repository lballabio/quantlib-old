
The C++ wrappers for the QuantLib-Guile extension module are created 
by means of SWIG (Simple Wrapper Interface Generator) available from 
<http://swig.sourceforge.net/>. Features used in the QuantLib-Guile
interface files require version 1.3.15 or later of SWIG.

The wrappers are generated on all supported platforms by issuing the command
    guile -s setup.scm wrap
The above assumes that the SWIG executable is named "swig" and can be found 
in the system path.

The building and installation process consists of the following commands:
    guile -s setup.scm build
    guile -s setup.scm test
    guile -s setup.scm install

The install step above might require superuser privileges.


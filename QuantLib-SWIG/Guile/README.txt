
The C++ wrappers for the QuantLib-Guile extension module are created
by means of SWIG (Simplified Wrapper and Interface Generator) available
from <http://www.swig.org/>. Version 1.3.27 of SWIG is recommended.

The wrappers are generated on all supported platforms by issuing the command
    guile -s setup.scm wrap
The above assumes that the SWIG executable is named "swig" and can be found
in the system path. However, this step is only necessary if you are compiling
from sources checked out from the CVS repository. It is not required if you
are using a distributed tarball.

The building and installation process consists of the following commands:
    guile -s setup.scm build
    guile -s setup.scm test
    guile -s setup.scm install

The build step requires that the QuantLib headers and library can be
found by the compiler. This should already hold if you ran "make
install" from your QuantLib directory.

The install step above might require superuser privileges.

The test suite is implemented on top of the Greg framework, available
from <http://www.gnu.org/software/greg>. Version 1.4 of above of Greg
is required for running the suite.


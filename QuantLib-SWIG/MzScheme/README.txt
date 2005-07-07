
The C++ wrappers for the QuantLib-MzScheme extension module are created
by means of SWIG (Simplified Wrapper and Interface Generator) available
from <http://www.swig.org/>.  Features used in the QuantLib-MzScheme
interface files require version 2.0 or later of MzScheme; version
1.3.25 of SWIG is recommended.

The wrappers are generated on all supported platforms by issuing the command
    mzscheme -r setup.scm wrap
The above assumes that the SWIG executable is named "swig" and can be found
in the system path. However, this step is only necessary if you are compiling
from sources checked out from the CVS repository. It is not required if you
are using a distributed tarball.

The building and installation process consists of the following commands:
    mzscheme -r setup.scm build
    mzscheme -r setup.scm test
    mzscheme -r setup.scm install

The install step above might require superuser privileges.

The test suite is implemented on top of the Schemeunit framework, available
from <http://schematics.sourceforge.net>. Version 1.4 of above of Schemeunit
is required for running the suite.



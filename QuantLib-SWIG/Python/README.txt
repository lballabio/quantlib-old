
The C++ wrappers for the QuantLib-Python extension module are created
by means of SWIG (Simplified Wrapper and Interface Generator) available 
from <http://www.swig.org/>. Features used in the QuantLib-Python
interface files require version 2.2 or later of Python and version 1.3.21
or later of SWIG.

The wrappers are generated on all supported platforms by issuing the command
    python setup.py wrap
The above assumes that the SWIG executable is named "swig" and can be found
in the system path. However, this step is only necessary if you are compiling 
from sources checked out from the CVS repository. It is not required if you
are using a distributed tarball.

The build, test and installation processes are done by means of the Distutils
package, included in the Python standard library. The Python documentation
also includes instructions for using the Borland C++ compiler with Distutils.
The commands to be issued for the above processes are
    python setup.py build
    python setup.py test
    python setup.py install
respectively.

The install step above might require superuser privileges.
An alternate install location can be specified with the command:
    python setup.py install --prefix=/home/johndoe


The test suite is implemented on top of the PyUnit framework, which is also
included in the Python standard library.

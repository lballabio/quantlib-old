
The C++ wrappers for the QuantLib-Python extension module are created
by means of SWIG (Simplified Wrapper and Interface Generator) available 
from <http://www.swig.org/>. Features used in the QuantLib-Python
interface files require version 2.2 or later of Python; version 1.3.27
of SWIG is recommended.

The wrappers are generated on all supported platforms by issuing the command
    python setup.py wrap
The above assumes that the SWIG executable is named "swig" and can be found
in the system path. However, this step is only necessary if you are compiling 
from sources checked out from the CVS repository. It is not required if you
are using a distributed tarball, unless you are using Visual C++ 6; in the
latter case, you must regenerate the wrappers with swig 1.3.21.

The build, test and installation processes are done by means of the Distutils
package, included in the Python standard library. The Python documentation
also includes instructions for using the Borland C++ compiler with Distutils.
The commands to be issued for the above processes are
    python setup.py build
    python setup.py test
    python setup.py install
respectively.

The build step requires that the QuantLib headers and library can be
found by the compiler. On Unix-like platforms, this should already
hold if you ran "make install" from your QuantLib directory. On the
Windows platform, this can be achieved by defining a QL_DIR environment
variable pointing to your QuantLib directory (e.g., "C:\Lib\QuantLib".)

The install step might require superuser privileges.
An alternate install location can be specified with the command:
    python setup.py install --prefix=/home/johndoe

The test suite is implemented on top of the PyUnit framework, which is
included in the Python standard library.


The C++ wrappers for the QuantLib-Python extension module are created
by means of SWIG (Simple Wrapper Interface Generator) available from
<http://www.swig.org/>. Features used in the QuantLib-Python
interface files require version 2.1 or later of Python and version 1.3.17
or later of SWIG.

The wrappers are generated on all supported platforms by issuing the command
    python setup.py wrap
The above assumes that the SWIG executable is named "swig" and can be found
in the system path.

The build, test and installation processes are done by means of the Distutils
package, included in the Python standard library since the Python 1.6 release.
The commands to be issued for the above processes are
    python setup.py build
    python setup.py test
    python setup.py install
respectively.

Borland C++ users will need to replace the first of the above commands with
python setup.py build --compiler=bcpp
For the Borland setup to work you will need to have the compiler and QuantLib
installed in directories with no space in their names
(i.e. NOT "C:\Program Files").

Your Borland installation must have been completed with the addition of:
1) a bcc32.cfg file which will set the compiler options for the Include and
Lib paths (-I and -L switches to compiler) by adding these lines:
    -I"c:\Borland\Bcc55\include"
    -L"c:\Borland\Bcc55\lib"
2) an ilink32.cfg file which will set the linker option for the Lib path by
adding this line:
    -L"c:\Borland\Bcc55\lib"

Finally you will also need python22_bcpp.lib (or python21_bcpp.lib) that can
be created in your Python\libs folder by the command:
coff2omf python22.lib python22_bcpp.lib
coff2omf python22_d.lib python22_d_bcpp.lib

The test suite is implemented on top of the PyUnit framework, included in
the Python standard library.

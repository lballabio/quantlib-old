
The C++ wrappers for the QuantLib-MzScheme extension module are created 
by means of SWIG (Simple Wrapper Interface Generator) available from 
<http://swig.sourceforge.net/>. Features used in the QuantLib-MzScheme
interface files require version 2.0 as later of MzScheme and version 
1.3.14 or later of SWIG.

The wrappers are generated on all supported platforms by issuing the command
    mzscheme -r setup.scm wrap
The above assumes that the SWIG executable is named "swig" and can be found 
in the system path.

The building and installation process consists of the following commands:
    mzscheme -r setup.scm build
    mzscheme -r setup.scm test
    mzscheme -r setup.scm install

The install step above might require superuser privileges.


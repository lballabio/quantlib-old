
The C++ wrappers for the QuantLib-Ruby extension module are created 
by means of SWIG (Simple Wrapper Interface Generator) available from 
<http://swig.sourceforge.net/>. Features used in the QuantLib-Ruby
interface files require version 1.3.15 or later of SWIG.

The wrappers are generated on all supported platforms by issuing the command
    ruby setup.py wrap
The above assumes that the SWIG executable is named "swig" and can be found 
in the system path.

The building and installation process consists of the following commands:
    ruby setup.rb build
    ruby setup.rb test
    ruby setup.rb install

The install step above might require superuser privileges.
An alternate install location can be specified with the command:
    ruby setup.rb install --prefix=/home/johndoe

The test suite is implemented on top of the Test::Unit framework, available 
from <http://testunit.talbott.ws>. Version 0.1.6 of above of Test::Unit is
required for running the suite.


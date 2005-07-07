
The C++ wrappers for the QuantLib-Ruby extension module are created
by means of SWIG (Simplified Wrapper and Interface Generator) available
from <http://swig.sourceforge.net/>. Version 1.3.25 of SWIG is recommended.

The wrappers are generated on all supported platforms by issuing the command
    ruby setup.rb wrap
The above assumes that the SWIG executable is named "swig" and can be found
in the system path. However, this step is only necessary if you are compiling
from sources checked out from the CVS repository. It is not required if you
are using a distributed tarball.

The building and installation process consists of the following commands:
    ruby setup.rb build
    ruby setup.rb test
    ruby setup.rb install

The install step above might require superuser privileges.
An alternate install location can be specified with the command:
    ruby setup.rb install --prefix=/home/johndoe

The test suite is implemented on top of the Test::Unit framework, included
in the standard Ruby distribution since version 1.8.0. A version of Test::Unit 
for Ruby 1.6.x is available from <http://testunit.talbott.ws>. 


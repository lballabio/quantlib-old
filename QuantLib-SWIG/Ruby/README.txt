The Ruby module is currently supported on Linux only.

The C++ wrappers for the QuantLib-Ruby extension module are created 
by means of SWIG (Simple Wrapper Interface Generator) available from 
<http://swig.sourceforge.net/>. Features used in the QuantLib-Ruby
interface files require version 1.3.11 or later of SWIG.

The build script assumes that the SWIG executable is named "swig" and can 
be found into the system path. SWIG should not be necessary for building
from a released source tarball.

The building and installation process consists of the following commands:
    ruby setup.rb build
    ruby setup.rb test
    ruby setup.rb install

The install step above might require superuser privileges.
An alternate install location can be specified with the command:
    ruby setup.rb install --prefix=/home/ballabl

The test suite is implemented on top of the RubyUnit framework, available from 
<http://homepage1.nifty.com/markey/ruby/rubyunit/index_e.html>.


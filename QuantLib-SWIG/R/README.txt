This is the R interface to QuantLib. The needed C++ bindings are created
by means of SWIG (Simplified Wrapper and Interface Generator) available
from <http://swig.sourceforge.net/>. SWIG version 1.3.32 is needed.

SWIG 1.3.31 generates a .R called QuantLib_wrap.R instead of
QuantLib.R.  The Makefile.am and makeRData.R has commented sections
which can be used with SWIG 1.3.31

The command

   make

generates the R library QuantLib.so and the wrapper script 
QuantLib_wrap.R and a compiled wrapper QuantLib.RData.

Once you've compiled this you can load with

  dyn.load('QuantLib.so')
  load('QuantLib.RData')
  cacheMetaData(1)

The last line is to work around a bug that causes S4 methods not to
get loaded correctly.

You can also load the uncompiled wrapper file

  dyn.load('QuantLib_wrap.so')
  source('QuantLib.R')

Please contact 

Joseph Wang - joe@gnacademy.org 

if you have any comments or additions.


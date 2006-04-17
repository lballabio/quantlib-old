This is an experimental interface to R.  Generating the wrappers
requires patches to SWIG which can be gotten from
<http://wiki.quantlib.org/twiki/bin/view/Quantlib/RSwig>; the patched
executable should be renamed rswig.

The command

   make

generates the R library QuantLib.so and the wrapper script 
QuantLib_wrap.R and a compiled wrapper QuantLib.RData.

Once you've compiled this you can load with

  dyn.load('QuantLib_wrap.so')
  load('QuantLib.RData')
  cacheMetaData(1)

The last line is to work around a bug that causes S4 methods not to
get loaded correctly.

You can also load the uncompiled wrapper file

  dyn.load('QuantLib_wrap.so')
  source('QuantLib_wrap.R')

Please contact 

Joseph Wang - joe@gnacademy.org 

if you have any comments or additions.


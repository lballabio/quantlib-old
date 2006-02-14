This is an experimental interface to R.
Generating the wrappers requires patches to SWIG which can be gotten from
<http://wiki.quantlib.org/twiki/bin/view/Quantlib/RSwig>; the patched
executable should be renamed rswig.

Once you've compiled the module, you can load it with

dyn.load('QuantLib.so')
source('QuantLib_wrap.S')


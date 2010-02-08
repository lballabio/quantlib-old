Installation
------------

The following was tested on Mac OS X Intel (10.5.7) with gcc 4.0.1.

Install
1) OpenOffice.org 3.1.1 from http://download.openoffice.org
2) OpenOffice.org 3.1.1. SDK from http://download.openoffice.org/sdk 
(available for Linux, Solaris, Mac OS X Intel and Windows)

Environment variables
1) Ensure that SDK environment variables are set, e.g. by sourcing the script
   setsdkenv_unix that is compiled during SDK installation. This involves
   in particular setting OO_SDK_HOME and OO_SDK_OUT.
2) Set environment variables 
   QL_PATH, QA_PATH, OH_PATH, LG_PATH, respectively, to the full path to the  
   QuantLib, QuantLibAddin, ObjectHandler, log4cxx 
   directories.

"make" should then compile and deploy the Calc addin so that the functions are 
available after next start of OpenOffice.org.

Todo
----
- fix linker options (see Makefile comments) 
- turn Makefile into Makefile.am
- set environment variables via configure
- reanimate gensrc for Calc

QuantLib addin for OpenOffice.org Calc - linux and windows

Prerequisites: OpenOffice, OpenOffice SDK

- Build the quantlib Calc project, this outputs the registry database and library, e.g. on Windows
QuantLibAddin-vc6-mt-sgd.rdb
QuantLibAddin-vc6-mt-sgd.dll

- go to your Openoffice program directory, e.g.

(linux)   /usr/lib/openoffice/program
(windows) C:\Program Files\OpenOffice.org1.1.3\program

- edit file unorc, and add instructions for Openoffice to find the new libraries:

UNO_SHARED_PACKAGES=...
UNO_SHARED_PACKAGES_CACHE=...
UNO_USER_PACKAGES=...
UNO_USER_PACKAGES_CACHE=...
UNO_TYPES=$SYSBINDIR/types.rdb ... $SYSBINDIR/QuantLibAddin-vc6-mt-sgd.rdb
UNO_SERVICES=?$UNO_USER_PACKAGES_CACHE/services.rdb ... $SYSBINDIR/QuantLibAddin-vc6-mt-sgd.rdb

- copy the registry database and shared library to the program directory.  on linux you may prefer to just create symbolic links, e.g:

ln -s /path_to_SDK/OpenOffice.org1.1_SDK/examples/cpp/demo/QuantLibAddin.rdb
ln -s /path_to_SDK/OpenOffice.org1.1_SDK/examples/cpp/demo/QuantLibAddin.so

- start Calc, and do Control-F2 to bring up the "Insert Function" menu, if all has gone well the new QUantLibAddin functions should appear in the list of available functions
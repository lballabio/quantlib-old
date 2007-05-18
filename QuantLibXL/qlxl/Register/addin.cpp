
/* 
 Copyright (C) 2005, 2006, 2007 Eric Ehlers

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it under the
 terms of the QuantLib license.  You should have received a copy of the
 license along with this program; if not, please email quantlib-dev@lists.sf.net
 The license is also available online at http://quantlib.org/html/license.html

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
*/

#include <ohxl/objecthandlerxl.hpp>
#include <qlo/qladdindefines.hpp>
#include <qlo/Enumerations/Register/register_all.hpp>
#include <qlxl/Register/register_all.hpp>
/* Use BOOST_MSVC instead of _MSC_VER since some other vendors
   (Metrowerks, for example) also #define _MSC_VER
*/
#if defined BOOST_MSVC       // Microsoft Visual C++
#  define BOOST_LIB_DIAGNOSTIC
#  include <qlo/auto_link.hpp>
#  include <oh/auto_link.hpp>
#  if defined(XLL_STATIC)
     #include <ohxl/Register/register_all.hpp>
     #include <ohxl/Functions/export.hpp>
     #pragma message("XLL_STATIC is defined")
#  elif defined(XLL_IMPORTS)
     #include <xlsdk/auto_link.hpp>
     #pragma message("XLL_IMPORTS is defined")
#  endif
#  undef BOOST_LIB_DIAGNOSTIC
#endif

#if defined COMPILING_XLL_DYNAMIC
#   pragma message("COMPILING_XLL_DYNAMIC is defined")
#else
#   pragma message("COMPILING_XLL_DYNAMIC is NOT defined")
#endif

#ifdef XLL_STATIC
    // Instantiate the ObjectHandler Repository
    ObjectHandler::RepositoryXL oh;
#endif

DLLEXPORT void xlAutoFree(XLOPER *px) {
    freeOper(px);
}

DLLEXPORT XLOPER *xlAddInManagerInfo(XLOPER *xlAction) {
    XLOPER xlReturn;
    static XLOPER xlLongName;

    // Coerce the argument XLOPER to an integer.
    Excel(xlCoerce, &xlReturn, 2, xlAction, TempInt(xltypeInt));

    // The only valid argument value is 1. In this case we return the
    // long name for the XLL. Any other value should result in the
    // return of a #VALUE! error.
    if (1 == xlReturn.val.w) {
        ObjectHandler::scalarToOper(std::string("QuantLibAddin " QLADDIN_VERSION), xlLongName);
    } else {
        xlLongName.xltype = xltypeErr;
        xlLongName.val.err = xlerrValue;
    }

    return &xlLongName;
}

DLLEXPORT int xlAutoOpen() {
    XLOPER xDll;
    try {
        Excel(xlGetName, &xDll, 0);

#ifdef XLL_STATIC
        // Initialize configuration info
        ObjectHandler::Configuration::instance().init();

        // Initialize ObjectHandler functions
        registerOhFunctions(xDll);
#endif
        // Initialize QuantLib functions
        registerQlFunctions(xDll);

        // Initialize the Enumeration Registry
        QuantLibAddin::registerEnumerations();

        Excel(xlFree, 0, 1, &xDll);

        return 1;
    } catch (const std::exception &e) {
        std::ostringstream err;
        err << "Error loading QuantLibXL: " << e.what();
        Excel(xlcAlert, 0, 1, TempStrStl(err.str()));
        Excel(xlFree, 0, 1, &xDll);
        return 0;
    } catch (...) {
        Excel(xlFree, 0, 1, &xDll);
        return 0;
    }
}

DLLEXPORT int xlAutoClose() {
    static XLOPER xDll;
    try {
        Excel(xlGetName, &xDll, 0);

#ifdef XLL_STATIC
        // Unregister ObjectHandler functions
        unregisterOhFunctions(xDll);
#endif

        // Unregister QuantLib functions
        unregisterQlFunctions(xDll);

        // Deallocate the Enumeration Registry
        QuantLibAddin::unregisterEnumerations();

        Excel(xlFree, 0, 1, &xDll);

        return 1;
    } catch (...) {
        Excel(xlFree, 0, 1, &xDll);
        return 0;
    }
}


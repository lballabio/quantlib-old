
/* 
 Copyright (C) 2004, 2005, 2006 Eric Ehlers

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

#include <xlsdk/xlsdkdefines.hpp>
#include <ohxl/objecthandlerxl.hpp>
#include <ohxl/Register/register_all.hpp>
#include <ohxl/Functions/export.hpp>
#include <ohxl/Conversions/all.hpp>
#include <sstream>

// instantiate the object handler singleton
ObjHandler::ObjectHandlerXL objectHandler;

DLLEXPORT int xlAutoOpen() {
    static XLOPER xDll;
    try {
        Excel(xlGetName, &xDll, 0);

        registerOhFunctions(xDll);

        Excel(xlFree, 0, 1, &xDll);
        return 1;
    } catch (const std::exception &e) {
        std::ostringstream err;
        err << "Error loading ObjectHandler: " << e.what();
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
        // empty the ObjectHandler repository

        Excel(xlUDF, 0, 1, TempStrNoSize("\x12""ohDeleteAllObjects"));

        // unregister the addin functions

        Excel(xlGetName, &xDll, 0);

        unregisterOhFunctions(xDll);

        Excel(xlFree, 0, 1, &xDll);

        return 1;
    } catch (...) {
        Excel(xlFree, 0, 1, &xDll);
        return 0;
    }
}

DLLEXPORT void xlAutoFree(XLOPER *px) {
    if (px->xltype & xltypeStr && px->val.str)
        delete [] px->val.str;
    else if (px->xltype & xltypeMulti && px->val.array.lparray) {
        int size = px->val.array.rows * px->val.array.columns;
        for (int i=0; i<size; i++)
            if (px->val.array.lparray[i].xltype & xltypeStr
            &&  px->val.array.lparray[i].val.str)
                delete [] px->val.array.lparray[i].val.str;
        delete [] px->val.array.lparray;
    }
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
        ObjHandler::scalarToOper(std::string("ObjectHandler 0.9.0"), xlLongName);
    } else {
        xlLongName.xltype = xltypeErr;
        xlLongName.val.err = xlerrValue;
    }

    return &xlLongName;
}


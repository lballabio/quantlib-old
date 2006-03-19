
/*
 Copyright (C) 2005, 2006 Eric Ehlers

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

#include <oh/objhandler.hpp>
#include <car.hpp>
#include <xlsdk/xlsdk.hpp>
#include <ohxl/conversions.hpp>
#include <sstream>

DLLEXPORT int xlAutoOpen() {
    static XLOPER xDll;
    try {
        Excel(xlGetName, &xDll, 0);

        Excel(xlfRegister, 0, 7, &xDll,
            TempStrNoSize("\x0E""addin2GetSpeed"),  // function code name
            TempStrNoSize("\x02""NC"),              // parameter codes
            TempStrNoSize("\x0E""addin2GetSpeed"),  // function display name
            TempStrNoSize("\x06""handle"),          // comma-delimited list of parameters
            TempStrNoSize("\x01""1"),               // function type (0 = hidden function, 1 = worksheet function, 2 = command macro)
            TempStrNoSize("\x07""Example"));        // function category

        Excel(xlFree, 0, 1, &xDll);
        return 1;
    } catch (const std::exception &e) {
        std::ostringstream err;
        err << "Error loading ExampleXllDynamic: " << e.what();
        Excel(xlcAlert, 0, 1, TempStrStl(err.str()));
        Excel(xlFree, 0, 1, &xDll);
        return 0;
    } catch (...) {
        Excel(xlFree, 0, 1, &xDll);
        return 0;
    }
}

DLLEXPORT long *addin2GetSpeed(char *handle) {
    try {
        CarObjectPtr carObject =
            OH_GET_OBJECT(CarObject, handle);
        if (!carObject) {
            std::ostringstream msg;
            msg << "unable to retrieve object " << handle;
            throw ObjHandler::Exception(msg.str().c_str());
        }
        static long ret;
        ret = carObject->getSpeed();
        return &ret;
    } catch (const std::exception &e) {
        ObjHandler::logMessage(std::string("Error: getSpeed: ") + e.what(), 2);
        return 0;
    }
}

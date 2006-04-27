
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
#include <account.hpp>
#include <xlsdk/xlsdk.hpp>
#include <ohxl/conversions.hpp>
#include <ohxl/register.hpp>
#include <ohxl/export.hpp>
#include <ohxl/instancenamexl.hpp>
#include <ohxl/functioncall.hpp>
#include <sstream>

DLLEXPORT int xlAutoOpen() {
    static XLOPER xDll;
    try {
        Excel(xlGetName, &xDll, 0);

        ohRegisterFunctions(xDll);

        Excel(xlfRegister, 0, 7, &xDll,
            TempStrNoSize("\x0D""createAccount"),   // function code name
            TempStrNoSize("\x05""CCNC#"),           // parameter codes
            TempStrNoSize("\x0D""createAccount"),   // function display name
            TempStrNoSize("\x26""instanceName,accountNumber,accountType"), // comma-delimited list of parameters
            TempStrNoSize("\x01""1"),               // function type (0 = hidden function, 1 = worksheet function, 2 = command macro)
            TempStrNoSize("\x07""Example"));        // function category

        Excel(xlfRegister, 0, 7, &xDll,
            TempStrNoSize("\x0A""setBalance"),      // function code name
            TempStrNoSize("\x03""LCN"),             // parameter codes
            TempStrNoSize("\x0A""setBalance"),      // function display name
            TempStrNoSize("\x14""instanceName,balance"), // comma-delimited list of parameters
            TempStrNoSize("\x01""1"),               // function type (0 = hidden function, 1 = worksheet function, 2 = command macro)
            TempStrNoSize("\x07""Example"));        // function category

        Excel(xlfRegister, 0, 7, &xDll,
            TempStrNoSize("\x0A""getBalance"),      // function code name
            TempStrNoSize("\x03""NCP"),             // parameter codes
            TempStrNoSize("\x0A""getBalance"),      // function display name
            TempStrNoSize("\x14""instanceName,trigger"),  // comma-delimited list of parameters
            TempStrNoSize("\x01""1"),               // function type (0 = hidden function, 1 = worksheet function, 2 = command macro)
            TempStrNoSize("\x07""Example"));        // function category

        Excel(xlFree, 0, 1, &xDll);
        return 1;
    } catch (const std::exception &e) {
        std::ostringstream err;
        err << "Error loading ExampleXllStatic: " << e.what();
        Excel(xlcAlert, 0, 1, TempStrStl(err.str()));
        Excel(xlFree, 0, 1, &xDll);
        return 0;
    } catch (...) {
        Excel(xlFree, 0, 1, &xDll);
        return 0;
    }
}

DLLEXPORT char *createAccount(
        char *instanceName, 
        long *accountNumber,
        char *accountType) {
    try {
        ObjHandler::FunctionCall functionCall;
        ObjHandler::FunctionCall::instance().clearCell();

        ObjHandler::obj_ptr objectPointer(new AccountObject(
            *accountNumber, 
            accountType));
        objectPointer->setProperties(
            boost::shared_ptr<ObjHandler::ValueObject>(
            new AccountValueObject(instanceName, *accountNumber, accountType)));
        const std::string returnValue = ObjHandler::storeObject(instanceName, objectPointer);
        static char ret[XL_MAX_STR_LEN];
        ObjHandler::stringToChar(ret, returnValue);
        return ret;
    } catch (const std::exception &e) {
        ObjHandler::logMessage(std::string("Error: createAccount: ") + e.what(), 2);
        return 0;
    }
}

DLLEXPORT short int *setBalance(char *instanceName, long *balance) {
    try {
        OH_GET_OBJECT(accountObject, instanceName, AccountObject)
        accountObject->setBalance(*balance);
        static short int ret = TRUE;
        return &ret;
    } catch (const std::exception &e) {
        ObjHandler::logMessage(std::string("Error: setBalance: ") + e.what(), 2);
        return 0;
    }
}

DLLEXPORT long *getBalance(char *instanceName, OPER *trigger) {
    try {
        OH_GET_OBJECT(accountObject, instanceName, AccountObject)
        static long ret;
        ret = accountObject->getBalance();
        return &ret;
    } catch (const std::exception &e) {
        ObjHandler::logMessage(std::string("Error: getBalance: ") + e.what(), 2);
        return 0;
    }
}



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
#include <ohxl/instancenamexl.hpp>
#include <ohxl/functioncall.hpp>
#include <sstream>

DLLEXPORT int xlAutoOpen() {
    static XLOPER xDll;
    try {
        Excel(xlGetName, &xDll, 0);

        Excel(xlfRegister, 0, 7, &xDll,
            TempStrNoSize("\x13""addin1CreateAccount"), // function code name
            TempStrNoSize("\x05""CCNC#"),           // parameter codes
            TempStrNoSize("\x13""addin1CreateAccount"), // function display name
            TempStrNoSize("\x26""instanceName,accountNumber,accountType"), // comma-delimited list of parameters
            TempStrNoSize("\x01""1"),               // function type (0 = hidden function, 1 = worksheet function, 2 = command macro)
            TempStrNoSize("\x07""Example"));        // function category

        Excel(xlfRegister, 0, 7, &xDll,
            TempStrNoSize("\x10""addin1SetBalance"),// function code name
            TempStrNoSize("\x03""LCN"),             // parameter codes
            TempStrNoSize("\x10""addin1SetBalance"),// function display name
            TempStrNoSize("\x14""instanceName,balance"), // comma-delimited list of parameters
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

DLLEXPORT char* addin1CreateAccount(
        char *instanceName, 
        long *accountNumber,
        char *accountType) {
    try {
        ObjHandler::FunctionCall functionCall;
        ObjHandler::FunctionCall::instance().clearCell();

        ObjHandler::obj_ptr objectPointer(new AccountObject(
            boost::shared_ptr < ObjHandler::Object::InstanceName > (new ObjHandler::InstanceNameXL(instanceName)),
            *accountNumber, 
            accountType));
        objectPointer->setProperties(
            boost::shared_ptr<ObjHandler::ValueObject>(
            new AccountValueObject(instanceName, *accountNumber, accountType)));
        const std::string returnValue = ObjHandler::storeObject(objectPointer);
        static char ret[XL_MAX_STR_LEN];
        ObjHandler::stringToChar(ret, returnValue);
        return ret;
    } catch (const std::exception &e) {
        ObjHandler::logMessage(std::string("Error: addin1CreateAccount: ") + e.what(), 2);
        return 0;
    }
}

DLLEXPORT short int *addin1SetBalance(char *instanceName, long *balance) {
    try {
        OH_GET_OBJECT(accountObject, instanceName, AccountObject)
        accountObject->setBalance(*balance);
        static short int ret = TRUE;
        return &ret;
    } catch (const std::exception &e) {
        ObjHandler::logMessage(std::string("Error: addin1SetBalance: ") + e.what(), 2);
        return 0;
    }
}


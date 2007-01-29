
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

#include <account.hpp>
#include <ohxl/objhandlerxl.hpp>
#include <ohxl/Utilities/utilities.hpp>
/* Use BOOST_MSVC instead of _MSC_VER since some other vendors (Metrowerks,
   for example) also #define _MSC_VER
*/
#ifdef BOOST_MSVC
#  define BOOST_LIB_DIAGNOSTIC
#  include <oh/auto_link.hpp>
#  include <xlsdk/auto_link.hpp>
#  undef BOOST_LIB_DIAGNOSTIC
#endif
#include <sstream>

DLLEXPORT int xlAutoOpen() {
    static XLOPER xDll;
    try {
        Excel(xlGetName, &xDll, 0);

        Excel(xlfRegister, 0, 7, &xDll,
            TempStrNoSize("\x13""addin1CreateAccount"), // function code name
            TempStrNoSize("\x05""CCNC#"),           // parameter codes
            TempStrNoSize("\x13""addin1CreateAccount"), // function display name
            TempStrNoSize("\x22""objectID,accountNumber,accountType"), // comma-delimited list of parameters
            TempStrNoSize("\x01""1"),               // function type (0 = hidden function, 1 = worksheet function, 2 = command macro)
            TempStrNoSize("\x07""Example"));        // function category

        Excel(xlfRegister, 0, 7, &xDll,
            TempStrNoSize("\x10""addin1SetBalance"),// function code name
            TempStrNoSize("\x03""LCN"),             // parameter codes
            TempStrNoSize("\x10""addin1SetBalance"),// function display name
            TempStrNoSize("\x10""objectID,balance"), // comma-delimited list of parameters
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

DLLEXPORT void xlAutoFree(XLOPER *px) {
    freeOper(px);
}

DLLEXPORT char* addin1CreateAccount(
        char *objectID,
        long *accountNumber,
        char *accountType) {
    boost::shared_ptr < ObjHandler::FunctionCall > functionCall;
    try {
        functionCall = boost::shared_ptr < ObjHandler::FunctionCall > 
            ( new ObjHandler::FunctionCall("addin1CreateAccount") );
        ObjHandler::ObjectHandlerXL::instance().resetCaller(true);
        boost::shared_ptr < ObjHandler::Object > objectPointer(new AccountObject(
            *accountNumber,
            accountType));
        objectPointer->setProperties(
            boost::shared_ptr<ObjHandler::ValueObject>(
            new AccountValueObject(objectID, *accountNumber, accountType)));

        const std::string returnValue = 
            ObjHandler::ObjectHandler::instance().storeObject(objectID, objectPointer);

        static char ret[XL_MAX_STR_LEN];
        ObjHandler::stringToChar(returnValue, ret);
        return ret;
    } catch (const std::exception &e) {
        std::ostringstream err;
        err << "Error: addin1CreateAccount - ";
        if (functionCall) err << functionCall->getAddressString() << " - ";
        err << e.what();
        ObjHandler::logMessage(err.str(), 2);
        return 0;
    }
}

DLLEXPORT short int *addin1SetBalance(char *objectID, long *balance) {
    boost::shared_ptr < ObjHandler::FunctionCall > functionCall;
    try {
        functionCall = boost::shared_ptr < ObjHandler::FunctionCall > 
            ( new ObjHandler::FunctionCall("addin1SetBalance") );

        OH_GET_OBJECT(accountObject, objectID, AccountObject)

        accountObject->setBalance(*balance);
        static short int ret = TRUE;
        return &ret;
    } catch (const std::exception &e) {
        std::ostringstream err;
        err << "Error: addin1SetBalance - ";
        if (functionCall) err << functionCall->getAddressString() << " - ";
        err << e.what();
        ObjHandler::logMessage(err.str(), 2);
        return 0;
    }
}



/*
 Copyright (C) 2005, 2006, 2007 Eric Ehlers

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it
 under the terms of the QuantLib license.  You should have received a
 copy of the license along with this program; if not, please email
 <quantlib-dev@lists.sf.net>. The license is also available online at
 <http://quantlib.org/license.shtml>.

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
*/

#include <oh/enumerations/typefactory.hpp>
#include <ohxl/objecthandlerxl.hpp>
#include <ohxl/register/register_all.hpp>
#include <ohxl/functions/export.hpp>
#include <ohxl/utilities/xlutilities.hpp>
#include <ExampleObjects/accountexample.hpp>
#include <Examples/examplefactory.hpp>

/* Use BOOST_MSVC instead of _MSC_VER since some other vendors (Metrowerks,
   for example) also #define _MSC_VER
*/
#ifdef BOOST_MSVC
#  define BOOST_LIB_DIAGNOSTIC
#  include <oh/auto_link.hpp>
#  undef BOOST_LIB_DIAGNOSTIC
#endif
#include <sstream>

// Instantiate the ObjectHandler Repository
ObjectHandler::RepositoryXL repositoryXL;
// Instantiate the Enumerated Type Registry
ObjectHandler::EnumTypeRegistry enumTypeRegistry;
// Instantiate the Enumerated Class Registry
ObjectHandler::EnumClassRegistry enumClassRegistry;
// Instantiate the Enumerated Pair Registry
ObjectHandler::EnumPairRegistry enumPairRegistry;
// Instantiate the Serialization Factory
ExampleAddin::ExampleFactory factory;

DLLEXPORT int xlAutoOpen() {

    static XLOPER xDll;

    try {

        Excel(xlGetName, &xDll, 0);

        ObjectHandler::Configuration::instance().init();

        registerOhFunctions(xDll);
        AccountExample::registerEnumeratedTypes();

        Excel(xlfRegister, 0, 7, &xDll,
            TempStrNoSize("\x09""ohAccount"),           // function code name
            TempStrNoSize("\x07""CCCNPL#"),              // parameter codes
            TempStrNoSize("\x09""ohAccount"),           // function display name
            TempStrNoSize("\x26""ObjectID,Type,Number,Balance,Permanent"), // comma-delimited list of parameters
            TempStrNoSize("\x01""1"),                   // function type (0 = hidden function, 1 = worksheet function, 2 = command macro)
            TempStrNoSize("\x07""Example"));            // function category

        Excel(xlfRegister, 0, 7, &xDll,
            TempStrNoSize("\x13""ohAccountSetBalance"), // function code name
            TempStrNoSize("\x04""LCN#"),                // parameter codes
            TempStrNoSize("\x13""ohAccountSetBalance"), // function display name
            TempStrNoSize("\x10""ObjectID,Balance"),    // comma-delimited list of parameters
            TempStrNoSize("\x01""1"),                   // function type (0 = hidden function, 1 = worksheet function, 2 = command macro)
            TempStrNoSize("\x07""Example"));            // function category

        Excel(xlfRegister, 0, 7, &xDll,
            TempStrNoSize("\x10""ohAccountBalance"),    // function code name
            TempStrNoSize("\x04""NCP#"),                // parameter codes
            TempStrNoSize("\x10""ohAccountBalance"),    // function display name
            TempStrNoSize("\x10""ObjectID,Trigger"),    // comma-delimited list of parameters
            TempStrNoSize("\x01""1"),                   // function type (0 = hidden function, 1 = worksheet function, 2 = command macro)
            TempStrNoSize("\x07""Example"));            // function category

        Excel(xlfRegister, 0, 7, &xDll,
            TempStrNoSize("\x0D""ohAccountType"),       // function code name
            TempStrNoSize("\x04""CCP#"),                // parameter codes
            TempStrNoSize("\x0D""ohAccountType"),       // function code name
            TempStrNoSize("\x10""ObjectID,Trigger"),    // comma-delimited list of parameters
            TempStrNoSize("\x01""1"),                   // function type (0 = hidden function, 1 = worksheet function, 2 = command macro)
            TempStrNoSize("\x07""Example"));            // function category

        Excel(xlfRegister, 0, 7, &xDll,
            TempStrNoSize("\x05""func1"),               // function code name
            TempStrNoSize("\x04""CCP#"),                // parameter codes
            TempStrNoSize("\x05""func1"),               // function code name
            TempStrNoSize("\x10""ObjectID,Trigger"),    // comma-delimited list of parameters
            TempStrNoSize("\x01""1"),                   // function type (0 = hidden function, 1 = worksheet function, 2 = command macro)
            TempStrNoSize("\x07""Example"));            // function category

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

DLLEXPORT int xlAutoClose() {

    static XLOPER xDll;

    try {

        Excel(xlGetName, &xDll, 0);

        AccountExample::unregisterEnumeratedTypes();
        unregisterOhFunctions(xDll);

        Excel(xlFree, 0, 1, &xDll);
        return 1;

    } catch (const std::exception &e) {

        std::ostringstream err;
        err << "Error unloading ExampleXllStatic: " << e.what();
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

DLLEXPORT char *ohAccount(
        char *objectID,
        char *type,
        long *number,
        OPER *balance,
        bool *permanent) {

    boost::shared_ptr<ObjectHandler::FunctionCall> functionCall;

    try {

        functionCall = boost::shared_ptr<ObjectHandler::FunctionCall>
            (new ObjectHandler::FunctionCall("ohAccount"));

        long balanceLong = ObjectHandler::operToScalar<long>(
            *balance, "balance", 100);

        ObjectHandler::Variant balanceVariant =
            ObjectHandler::operToScalar<ObjectHandler::Variant>(
                *balance, "balance");

        AccountExample::Account::Type typeEnum =
            ObjectHandler::Create<AccountExample::Account::Type>()(type);

        boost::shared_ptr<ObjectHandler::ValueObject> valueObject(
            new AccountExample::AccountValueObject(objectID, type, *number, balanceVariant, *permanent));

        boost::shared_ptr<ObjectHandler::Object> object(
            new AccountExample::AccountObject(valueObject, typeEnum, *number, balanceLong, *permanent));

        std::string returnValue = 
            ObjectHandler::RepositoryXL::instance().storeObject(objectID, object);

        static char ret[XL_MAX_STR_LEN];
        ObjectHandler::stringToChar(returnValue, ret);
        return ret;

    } catch (const std::exception &e) {

        ObjectHandler::RepositoryXL::instance().logError(e.what(), functionCall);
        return 0;

    }
}

DLLEXPORT short int *ohAccountSetBalance(char *objectID, long *balance) {

    boost::shared_ptr<ObjectHandler::FunctionCall> functionCall;

    try {

        functionCall = boost::shared_ptr<ObjectHandler::FunctionCall>
            (new ObjectHandler::FunctionCall("ohAccountSetBalance"));

        OH_GET_OBJECT(accountObject, objectID, AccountExample::AccountObject)
        accountObject->setBalance(*balance);

        static short int ret = TRUE;
        return &ret;

    } catch (const std::exception &e) {

        ObjectHandler::RepositoryXL::instance().logError(e.what(), functionCall);
        return 0;

    }
}

DLLEXPORT long *ohAccountBalance(char *objectID, OPER *trigger) {

    boost::shared_ptr<ObjectHandler::FunctionCall> functionCall;

    try {

        functionCall = boost::shared_ptr<ObjectHandler::FunctionCall>
            (new ObjectHandler::FunctionCall("ohAccountBalance"));

        OH_GET_OBJECT(accountObject, objectID, AccountExample::AccountObject)

        static long ret;
        ret = accountObject->balance();
        return &ret;

    } catch (const std::exception &e) {

        ObjectHandler::RepositoryXL::instance().logError(e.what(), functionCall);
        return 0;

    }
}

DLLEXPORT char *ohAccountType(char *objectID, OPER *trigger) {

    boost::shared_ptr<ObjectHandler::FunctionCall> functionCall;

    try {

        functionCall = boost::shared_ptr<ObjectHandler::FunctionCall>
            (new ObjectHandler::FunctionCall("ohAccountType"));

        OH_GET_OBJECT(accountObject, objectID, AccountExample::AccountObject)

        static char ret[XL_MAX_STR_LEN];
        ObjectHandler::stringToChar(accountObject->type(), ret);
        return ret;

    } catch (const std::exception &e) {

        ObjectHandler::RepositoryXL::instance().logError(e.what(), functionCall);
        return 0;

    }
}

DLLEXPORT char *func1(char *objectID, OPER *trigger) {

    boost::shared_ptr<ObjectHandler::FunctionCall> functionCall;

    try {

        functionCall = boost::shared_ptr<ObjectHandler::FunctionCall>
            (new ObjectHandler::FunctionCall("func1"));

        AccountExample::Account::Type typeEnum =
            ObjectHandler::Create<AccountExample::Account::Type>()(objectID);

        std::ostringstream s;
        s << typeEnum;
        static char ret[XL_MAX_STR_LEN];
        ObjectHandler::stringToChar(s.str(), ret);
        return ret;

    } catch (const std::exception &e) {

        ObjectHandler::RepositoryXL::instance().logError(e.what(), functionCall);
        return 0;

    }
}



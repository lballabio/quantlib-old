
/*
 Copyright (C) 2005, 2006, 2007 Eric Ehlers
 Copyright (C) 2008 Nazcatech sprl Belgium

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

#include <ohxl/objecthandlerxl.hpp>
#include <ohxl/utilities/xlutilities.hpp>
#include <ExampleObjects/accountexample.hpp>

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

// Instantiate the Serialization Factory
//ExampleAddin::ExampleFactory factory;
AccountExample::SerializationFactory factory;

DLLEXPORT int xlAutoOpen() {

    static XLOPER xDll;

    try {

        AccountExample::registerEnumeratedTypes();

        Excel(xlGetName, &xDll, 0);

        Excel(xlfRegister, 0, 7, &xDll,
            TempStrNoSize("\x14""addin1CreateCustomer"),// function code name
            TempStrNoSize("\x06""CCCNL#"),              // parameter codes
            TempStrNoSize("\x14""addin1CreateCustomer"),// function display name
            TempStrNoSize("\x1B""ObjectID,Name,Age,Permanent"), // comma-delimited list of parameters
            TempStrNoSize("\x01""1"),                   // function type (0 = hidden function, 1 = worksheet function, 2 = command macro)
            TempStrNoSize("\x07""Example"));            // function category

        Excel(xlfRegister, 0, 7, &xDll,
            TempStrNoSize("\x13""addin1CreateAccount"), // function code name
            TempStrNoSize("\x08""CCCCNPL#"),            // parameter codes
            TempStrNoSize("\x13""addin1CreateAccount"), // function display name
            TempStrNoSize("\x2F""ObjectID,Customer,Type,Number,Balance,Permanent"), // comma-delimited list of parameters
            TempStrNoSize("\x01""1"),                   // function type (0 = hidden function, 1 = worksheet function, 2 = command macro)
            TempStrNoSize("\x07""Example"));            // function category

        Excel(xlfRegister, 0, 7, &xDll,
            TempStrNoSize("\x10""addin1SetBalance"),    // function code name
            TempStrNoSize("\x04""LCN#"),                // parameter codes
            TempStrNoSize("\x10""addin1SetBalance"),    // function display name
            TempStrNoSize("\x10""ObjectID,Balance"),    // comma-delimited list of parameters
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
        err << "Error loading ExampleXllDynamic1: " << e.what();
        Excel(xlcAlert, 0, 1, TempStrStl(err.str()));
        Excel(xlFree, 0, 1, &xDll);
        return 0;

    } catch (...) {

        Excel(xlFree, 0, 1, &xDll);
        return 0;

    }
}

DLLEXPORT int xlAutoClose() {

    try {

        AccountExample::unregisterEnumeratedTypes();
        return 1;

    } catch (const std::exception &e) {

        std::ostringstream err;
        err << "Error unloading ExampleXllDynamic1: " << e.what();
        Excel(xlcAlert, 0, 1, TempStrStl(err.str()));
        return 0;

    } catch (...) {

        return 0;

    }

}

DLLEXPORT void xlAutoFree(XLOPER *px) {

    freeOper(px);

}

DLLEXPORT char *addin1CreateCustomer(
        char *objectID,
        char *name,
        long *age,
        bool *permanent) {

    boost::shared_ptr<ObjectHandler::FunctionCall> functionCall;

    try {

        functionCall = boost::shared_ptr<ObjectHandler::FunctionCall>
            (new ObjectHandler::FunctionCall("addin1CreateCustomer"));

        boost::shared_ptr<ObjectHandler::ValueObject> valueObject(
            new AccountExample::CustomerValueObject(objectID, name, *age, *permanent));

        boost::shared_ptr<ObjectHandler::Object> object(
            new AccountExample::CustomerObject(valueObject, name, *age, *permanent));

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

DLLEXPORT char *addin1CreateAccount(
        char *objectID,
        char *customer,
        char *type,
        long *number,
        OPER *balance,
        bool *permanent) {

    boost::shared_ptr<ObjectHandler::FunctionCall> functionCall;

    try {

        functionCall = boost::shared_ptr<ObjectHandler::FunctionCall>
            (new ObjectHandler::FunctionCall("addin1CreateAccount"));

        OH_GET_REFERENCE(customerRef, customer,
            AccountExample::CustomerObject, AccountExample::Customer)

        long balanceLong = ObjectHandler::operToScalar<long>(
            *balance, "balance", 100);

        ObjectHandler::Variant balanceVariant =
            ObjectHandler::operToScalar<ObjectHandler::Variant>(
                *balance, "balance");

        AccountExample::Account::Type typeEnum =
            ObjectHandler::Create<AccountExample::Account::Type>()(type);

        boost::shared_ptr<ObjectHandler::ValueObject> valueObject(
            new AccountExample::AccountValueObject(objectID, customer, type, *number, balanceVariant, *permanent));

        boost::shared_ptr<ObjectHandler::Object> object(
            new AccountExample::AccountObject(valueObject, customerRef, typeEnum, *number, balanceLong, *permanent));

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

DLLEXPORT short int *addin1SetBalance(char *objectID, long *balance) {

    boost::shared_ptr<ObjectHandler::FunctionCall> functionCall;

    try {

        functionCall = boost::shared_ptr<ObjectHandler::FunctionCall>
            (new ObjectHandler::FunctionCall("addin1SetBalance"));

        OH_GET_OBJECT(accountObject, objectID, AccountExample::AccountObject)

        accountObject->setBalance(*balance);
        static short int ret = TRUE;
        return &ret;

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

        AccountExample::Account::Type accountTypeEnum =
            ObjectHandler::Create<AccountExample::Account::Type>()(objectID);

        std::ostringstream s;
        s << accountTypeEnum;
        static char ret[XL_MAX_STR_LEN];
        ObjectHandler::stringToChar(s.str(), ret);
        return ret;

    } catch (const std::exception &e) {

        ObjectHandler::RepositoryXL::instance().logError(e.what(), functionCall);
        return 0;

    }
}

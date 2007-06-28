
/*!
 Copyright (C) 2004, 2005, 2006, 2007 Eric Ehlers
 Copyright (C) 2006 Plamen Neykov

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

/* Use BOOST_MSVC instead of _MSC_VER since some other vendors (Metrowerks,
   for example) also #define _MSC_VER
*/

#ifdef BOOST_MSVC
#  define BOOST_LIB_DIAGNOSTIC
#  include <oh/auto_link.hpp>
#  undef BOOST_LIB_DIAGNOSTIC
#endif
#include <sstream>
#include <iostream>
#include <exception>
#include <oh/objecthandler.hpp>
#include <oh/Conversions/varianttoscalar.hpp>
#include <ExampleObjects/accountexample.hpp>
#include <Examples/examplefactory.hpp>

// Instantiate the ObjectHandler Repository
ObjectHandler::Repository repository;
// Instantiate the Enumerated Type Registry
ObjectHandler::EnumTypeRegistry enumTypeRegistry;
// Instantiate the Serialization Factory
ExampleAddin::ExampleFactory factory;

void makeAccount(
    const std::string &objectID,
    const std::string &type,
    const long &number,
    ObjectHandler::Variant::VariantDef balance2 = ObjectHandler::Other()) { // FIXME

    ObjectHandler::Variant balance(balance2);

    boost::shared_ptr <ObjectHandler::ValueObject> valueObject(
        new AccountExample::AccountValueObject(objectID, type, number, balance));

    AccountExample::Account::Type typeEnum =
        ObjectHandler::Create<AccountExample::Account::Type>()(type);

    long accountBalance = ObjectHandler::variantToScalar<ObjectHandler::Variant, long>(
        balance, "balance", 100);

    boost::shared_ptr<ObjectHandler::Object> accountObject1(
        new AccountExample::AccountObject(
            typeEnum, number, accountBalance));

    accountObject1->setProperties(valueObject);
    ObjectHandler::Repository::instance().storeObject(objectID, accountObject1);
}

int main() {

    try {

        // Specify log file
        ObjectHandler::setLogFile("example.log");
        // Also direct log messages to stdout
        ObjectHandler::setConsole(1);
        OH_LOG_MESSAGE("begin example program");

    } catch (const std::exception &e) {

        std::cout << "Unable to initialize logging: " << e.what() << std::endl;
        return 1;

    } catch (...) {

        std::cout << "Unable to initialize logging." << std::endl;
        return 1;

    }

    try {
        AccountExample::registerEnumeratedTypes();

        // Construct some objects and store them in the object handler
        makeAccount("account1", "Savings", 123456789);
        ObjectHandler::Variant::VariantDef vd(100L);
        ObjectHandler::Variant v(vd);
        makeAccount("account2", "Current", 987654321, 100L);
        //makeAccount("account2", "Current", 987654321, v);
        //makeAccount("account2", "Current", 987654321, ObjectHandler::Variant(100L));

        // High level interrogation
        OH_LOG_MESSAGE("High level interrogation - after constructor");
        ObjectHandler::logObject("account2");

        // Retrieve an object and update it
        OH_GET_OBJECT(accountObject2_retrieve,
            "account2", AccountExample::AccountObject)
        accountObject2_retrieve->setBalance(100);

        // Low-level interrogation
        OH_LOG_MESSAGE("Low-level interrogation - after update");
        OH_GET_REFERENCE(accountObjectUnderlying, "account2",
            AccountExample::AccountObject, AccountExample::Account)
        OH_LOG_MESSAGE("Result of getBalance on underlying = "
            << accountObjectUnderlying->balance());

        // Delete an object
        ObjectHandler::Repository::instance().deleteObject("account2");

        // Log all objects
        OH_LOG_MESSAGE("Log all objects after deleting account2:");
        ObjectHandler::logAllObjects();

        // Serialize an object
        ObjectHandler::Repository::instance().saveObject(
            accountObject2_retrieve, "account.xml");
        // Deserialize an object
        ObjectHandler::Repository::instance().loadObject(
            "account1_load", "account.xml");
        // Manipulate the deserialized object
        OH_GET_OBJECT(accountObject1_load,
            "account1_load", AccountExample::AccountObject)
        accountObject1_load->setBalance(200);
        OH_LOG_MESSAGE("Balance of account account1_load = "
            << accountObject1_load->balance());

        OH_LOG_MESSAGE("End example program");

        return 0;

    } catch (const std::exception &e) {

        OH_LOG_ERROR("Error: " << e.what());
        return 1;

    } catch (...) {

        OH_LOG_ERROR("Error");
        return 1;

    }
}

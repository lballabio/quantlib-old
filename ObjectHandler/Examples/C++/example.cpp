
/*!
 Copyright (C) 2004, 2005, 2006, 2007 Eric Ehlers

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
#include <ExampleObjects/accountexample.hpp>

// Instantiate the ObjectHandler Repository
ObjectHandler::Repository repository;
// Instantiate the Enumerated Type Registry
ObjectHandler::EnumTypeRegistry enumTypeRegistry;
// Instantiate the Enumerated Class Registry
ObjectHandler::EnumClassRegistry enumClassRegistry;
// Instantiate the Enumerated Pair Registry
ObjectHandler::EnumPairRegistry enumPairRegistry;

int main() {

    try {
        // specify log file
        ObjectHandler::setLogFile("example.log");
        // also direct log messages to stdout
        ObjectHandler::setConsole(1);
        ObjectHandler::logMessage("begin example program");
    } catch (const std::exception &e) {
        std::cout << "Unable to initialize logging: " << e.what() << std::endl;
        return 1;
    } catch (...) {
        std::cout << "Unable to initialize logging." << std::endl;
        return 1;
    }

    try {

        // Construct some objects and store them in the object handler
        boost::shared_ptr<ObjectHandler::Object> accountObject1(
            new AccountExample::AccountObject(
                123456789,
                AccountExample::Account::Savings));
        accountObject1->setProperties(
            boost::shared_ptr<ObjectHandler::ValueObject>(
                new AccountExample::AccountValueObject(
                    "account1", 123456789, "Savings")));
        ObjectHandler::Repository::instance().storeObject("account1", accountObject1);

        boost::shared_ptr<ObjectHandler::Object> accountObject2(
            new AccountExample::AccountObject(
                987654321,
                AccountExample::Account::Current));
        accountObject2->setProperties(
            boost::shared_ptr<ObjectHandler::ValueObject>(
                new AccountExample::AccountValueObject(
                    "account2", 987654321, "Current")));
        ObjectHandler::Repository::instance().storeObject("account2", accountObject2);

        // High level interrogation
        ObjectHandler::logMessage("High level interrogation - after constructor");
        ObjectHandler::logObject("account2");

        // Retrieve an object and update it
        OH_GET_OBJECT(accountObject2_retrieve, 
            "account2", AccountExample::AccountObject)
        accountObject2_retrieve->setBalance(100);

        // Low-level interrogation
        ObjectHandler::logMessage("Low-level interrogation - after update");
        OH_GET_REFERENCE(accountObjectUnderlying, 
            "account2", AccountExample::AccountObject, AccountExample::Account)
        std::ostringstream msg;
        msg << "Result of getBalance on underlying = " 
            << accountObjectUnderlying->balance();
        ObjectHandler::logMessage(msg.str());

        // Delete an object
        ObjectHandler::Repository::instance().deleteObject("account2");

        // Log all objects
        ObjectHandler::logMessage("Log all objects after deleting account2:");
        ObjectHandler::logAllObjects();

        ObjectHandler::logMessage("End example program");

        return 0;

    } catch (const std::exception &e) {

        std::ostringstream s;
        s << "Error: " << e.what();
        ObjectHandler::logMessage(s.str(), 1);
        return 1;

    } catch (...) {

        ObjectHandler::logMessage("Error", 1);
        return 1;

    }
}


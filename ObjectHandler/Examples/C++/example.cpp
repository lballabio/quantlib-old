
/*!
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

#include <sstream>
#include <iostream>
#include <exception>
#include <account.hpp>

int main() {
    try {
        // specify log file
        ObjHandler::setLogFile("example.log");
        // also direct log messages to stdout
        ObjHandler::setConsole(1);
        ObjHandler::logMessage("begin example program");
    } catch (const std::exception &e) {
        std::cout << "Unable to initialize logging: " << e.what() << std::endl;
        return 1;
    } catch (...) {
        std::cout << "Unable to initialize logging." << std::endl;
        return 1;
    }

    try {
        // construct some objects and store them in the object handler
        ObjHandler::obj_ptr accountObject1(new AccountObject(123456789, "savings"));
        accountObject1->setProperties(
            boost::shared_ptr<ObjHandler::ValueObject>(new AccountValueObject(
            "account1", 123456789, "savings")));
        ObjHandler::storeObject("account1", accountObject1);

        ObjHandler::obj_ptr accountObject2(new AccountObject(987654321, "current"));
        accountObject2->setProperties(
            boost::shared_ptr<ObjHandler::ValueObject>(new AccountValueObject(
            "account2", 987654321, "current")));
        ObjHandler::storeObject("account2", accountObject2);

        // high level interrogation
        ObjHandler::logMessage("high level interrogation - after constructor");
        ObjHandler::logObject("account2");

        // retrieve an object and update it
        AccountObjectPtr accountObject2_retrieve =
            OH_GET_OBJECT(AccountObject, "account2");
        if (!accountObject2_retrieve)
            throw ObjHandler::Exception("unable to retrieve object account2");
        accountObject2_retrieve->setBalance(100);

        // low-level interrogation
        ObjHandler::logMessage("low-level interrogation - after update");
        OH_GET_REFERENCE(accountObjectUnderlying, "account2",
			AccountObject, Account);
        std::ostringstream msg;
        msg << "result of getBalance on underlying = " << accountObjectUnderlying->getBalance();
        ObjHandler::logMessage(msg.str());

        ObjHandler::ObjectHandler::instance().deleteObject("account2");
        ObjHandler::logMessage("log all objects after deleting account2:");
        ObjHandler::logAllObjects();

        ObjHandler::logMessage("end example program");

        return 0;
    } catch (const std::exception &e) {
        std::ostringstream s;
        s << "Error: " << e.what();
        ObjHandler::logMessage(s.str(), 1);
        return 1;
    } catch (...) {
        ObjHandler::logMessage("Error", 1);
        return 1;
    }
}


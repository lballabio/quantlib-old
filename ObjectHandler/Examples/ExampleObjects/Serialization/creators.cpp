
/*
 Copyright (C) 2007, 2008 Eric Ehlers
 Copyright (C) 2008 Plamen Neykov

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

#if defined(HAVE_CONFIG_H)     // Dynamically created by configure
    #include <oh/config.hpp>
#endif

#include <oh/property.hpp>
#include <ExampleObjects/Serialization/creators.hpp>
#include <ExampleObjects/Objects/customerobject.hpp>
#include <ExampleObjects/Objects/accountobject.hpp>
#include <ExampleObjects/Enumerations/accountenumerations.hpp>

namespace AccountExample {

    boost::shared_ptr<ObjectHandler::Object> createAccount(
        const boost::shared_ptr<ObjectHandler::ValueObject> &valueObject) {

        std::string customer = ObjectHandler::convert2<std::string>(valueObject->getProperty("Customer"));
        bool permanent = ObjectHandler::convert2<bool>(valueObject->getProperty("Permanent"));
        std::string type = ObjectHandler::convert2<std::string>(valueObject->getProperty("Type"));
        long number = ObjectHandler::convert2<long>(valueObject->getProperty("Number"));
        ObjectHandler::property_t balance = valueObject->getProperty("Balance");

        OH_GET_REFERENCE(customerRef, customer,
            AccountExample::CustomerObject, AccountExample::Customer)

        Account::Type typeEnum = ObjectHandler::Create<Account::Type>()(type);

        double accountBalance = ObjectHandler::convert2<double>(balance, "Balance", 100.00);

        boost::shared_ptr<ObjectHandler::Object> object(
            new AccountObject(valueObject, customerRef, typeEnum, number, accountBalance, permanent));
        return object;
    }

    boost::shared_ptr<ObjectHandler::Object> createCustomer(
        const boost::shared_ptr<ObjectHandler::ValueObject> &valueObject) {

        bool permanent = ObjectHandler::convert2<bool>(valueObject->getProperty("Permanent"));
        std::string name = ObjectHandler::convert2<std::string>(valueObject->getProperty("name"));
        long age = ObjectHandler::convert2<long>(valueObject->getProperty("age"));

        boost::shared_ptr<ObjectHandler::Object> object(
            new CustomerObject(valueObject, name, age, permanent));
        return object;
    }

}


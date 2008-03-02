
/*
 Copyright (C) 2007 Eric Ehlers

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

#include <oh/variant.hpp>
#include <oh/conversions/varianttoscalar.hpp>
#include <ExampleObjects/Serialization/creators.hpp>
#include <ExampleObjects/Objects/accountobject.hpp>
#include <ExampleObjects/Objects/customerobject.hpp>
#include <ExampleObjects/Enumerations/accountenumerations.hpp>

namespace AccountExample {

    boost::shared_ptr<ObjectHandler::Object> createAccount(
        const boost::shared_ptr<ObjectHandler::ValueObject> &valueObject) {

        bool permanent = boost::any_cast<bool>(valueObject->getProperty("Permanent"));
        std::string type = boost::any_cast<std::string>(valueObject->getProperty("Type"));
        long number = boost::any_cast<long>(valueObject->getProperty("Number"));
        ObjectHandler::Variant balance = boost::any_cast<ObjectHandler::Variant>(valueObject->getProperty("Balance"));

        Account::Type typeEnum =
            ObjectHandler::Create<Account::Type>()(type);

        long accountBalance = ObjectHandler::variantToScalar<ObjectHandler::Variant, long>(
            balance, "Balance", 100);

        boost::shared_ptr<ObjectHandler::Object> object(
            new AccountObject(valueObject, typeEnum, number, accountBalance, permanent));
        return object;
    }

    boost::shared_ptr<ObjectHandler::Object> createCustomer(
        const boost::shared_ptr<ObjectHandler::ValueObject> &valueObject) {

        bool permanent = boost::any_cast<bool>(valueObject->getProperty("Permanent"));
        std::string name = boost::any_cast<std::string>(valueObject->getProperty("name"));
        long age = boost::any_cast<long>(valueObject->getProperty("age"));

        boost::shared_ptr<ObjectHandler::Object> object(
            new CustomerObject(valueObject, name, age, permanent));
        return object;
    }


}


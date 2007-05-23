
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

#include <oh/Enumerations/typefactory.hpp>
#include <ExampleObjects/Enumerations/accountenumerations.hpp>
#include <ExampleObjects/Library/account.hpp>

namespace AccountExample {

    void registerEnumeratedTypes() {

        ObjectHandler::Create<Account::Type> create;
        create.registerType("Current", new Account::Type(Account::Current));
        create.registerType("Savings", new Account::Type(Account::Savings));

    }

    void unregisterEnumeratedTypes() {

        ObjectHandler::Create<Account::Type>().unregisterTypes();

    }

}

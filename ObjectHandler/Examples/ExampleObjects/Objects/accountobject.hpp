
/*!
 Copyright (C) 2004, 2005, 2006, 2007 Eric Ehlers

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

#ifndef example_account_object_hpp
#define example_account_object_hpp

#include <oh/libraryobject.hpp>
#include <ExampleObjects/Library/account.hpp>

namespace AccountExample {

    class AccountObject : public ObjectHandler::LibraryObject<Account> {

    public:

        AccountObject(
                const Account::Type &type,
                const long &number,
                const long &balance) {
            libraryObject_ = boost::shared_ptr<Account>(
                new Account(type, number, balance));
        }

        void setBalance(const long &balance)  {
            libraryObject_->setBalance(balance);
        }

        const long &balance()  {
            return libraryObject_->balance();
        }

        std::string type() {
            return libraryObject_->type();
        }

    };

}

#endif


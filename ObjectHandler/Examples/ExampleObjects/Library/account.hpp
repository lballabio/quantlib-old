
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

#ifndef example_account_hpp
#define example_account_hpp

#include <string>
#include <sstream>
#include <exception>
#include <ExampleObjects/Library/customer.hpp>

namespace AccountExample {

    class Account {
    public:

        enum Type { Savings, Current };

        Account(
            const boost::shared_ptr<Customer>& customer,
            const Type &type,
            const long &number,
            const long &balance)
            : customer_(customer), type_(type), number_(number), balance_(balance) {}

        const std::string& customerName() { return customer_->name(); }
        void setBalance(const int &balance) { balance_ = balance; }
        const long &balance() { return balance_; }
        std::string type() {
            std::ostringstream s;
            s << type_;
            return s.str();
        }

    private:
        boost::shared_ptr<Customer> customer_;
        Type type_;
        long number_;
        long balance_;
    };

    inline std::ostream& operator<<(std::ostream& out, Account::Type type) {
        switch (type) {
          case Account::Current:
            return out << "Current";
          case Account::Savings:
            return out << "Savings";
          default:
              OH_FAIL("unknown account type");
        }
    }

}

#endif


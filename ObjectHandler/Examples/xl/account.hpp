
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

#ifndef account_hpp
#define account_hpp

#include <oh/objecthandler.hpp>
#include <oh/valueobject.hpp>

class Account {
public:

    enum Type { Savings, Current };

    Account(const int &number,
        const Type &type,
        const int &balance = 0)
        : number_(number), type_(type), balance_(balance) {}

    void setBalance(const int &balance) { balance_ = balance; }
    const int &balance() { return balance_; }
    std::string type();

private:
    int number_;
    Type type_;
    int balance_;
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

class AccountObject : public ObjectHandler::LibraryObject<Account> {
public:
    AccountObject(
        const int &number,
        const Account::Type &type);
    void setBalance(const int &balance);
    const int &balance();
    std::string type();
};

typedef boost::shared_ptr<AccountObject> AccountObjectPtr;

class AccountValueObject : public ObjectHandler::ValueObject {
public:
    AccountValueObject(
        const std::string &objectID,
        const int &number,
        const std::string &type) 
        : objectID_(objectID), number_(number), type_(type) {}
    std::vector<std::string> getPropertyNames() const;
    boost::any getProperty(const std::string& name) const;
protected:
    static const char* mPropertyNames[];
    std::string objectID_;
    int number_;
    std::string type_;
};

#endif


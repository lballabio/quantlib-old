
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

#ifndef account_hpp
#define account_hpp

#include <oh/objhandler.hpp>
#include <oh/valueobject.hpp>

class Account {
public:
    Account(const int &accountNumber,
            const std::string &accountType,
            const int &balance = 0)
        : accountNumber_(accountNumber), accountType_(accountType), balance_(balance) {}
    void setBalance(const int &balance) {
        balance_ = balance;
    }
    const int &getBalance() {
        return balance_;
    }
private:
    int accountNumber_;
    std::string accountType_;
    int balance_;
};

class AccountObject : public ObjHandler::Object {
public:
    AccountObject(const int &accountNumber,
                  const std::string &accountType);
    void setBalance(const int &balance);
    const int &getBalance();
    virtual boost::shared_ptr<void> getReference() const;
private:
    boost::shared_ptr<Account> account_;
};

typedef boost::shared_ptr<AccountObject> AccountObjectPtr;

class AccountValueObject : public ObjHandler::ValueObject {
public:
    AccountValueObject(
        const std::string &instanceName,
        const int &accountNumber,
        const std::string &accountType) 
        : instanceName_(instanceName), accountNumber_(accountNumber), accountType_(accountType) {}
    std::vector<std::string> getPropertyNames() const;
    boost::any getProperty(const std::string& name) const;
protected:
    static const char* mPropertyNames[];
    std::string instanceName_;
    int accountNumber_;
    std::string accountType_;
};

#endif



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

#ifndef example_account_valueobject_hpp
#define example_account_valueobject_hpp

#include <oh/valueobject.hpp>
#include <vector>
#include <string>
#include <boost/any.hpp>

namespace AccountExample {

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

}

#endif


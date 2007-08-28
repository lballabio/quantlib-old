
/*!
 Copyright (C) 2004, 2005, 2006, 2007 Eric Ehlers
 Copyright (C) 2006 Plamen Neykov

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

#include <ExampleObjects/ValueObjects/accountvalueobject.hpp>
#include <oh/exception.hpp>
#include <iostream>
#include <string>
#include <boost/algorithm/string/case_conv.hpp>

namespace AccountExample {

    const char* AccountValueObject::mPropertyNames[] = {
        "ObjectId",
        "ClassName",
        "Permanent",
        "Number",
        "Type",
        "Balance"};

    std::vector<std::string> AccountValueObject::getPropertyNames() const {
        return std::vector<std::string>(mPropertyNames,
            mPropertyNames + sizeof(mPropertyNames)/sizeof(const char*));
    }

    boost::any AccountValueObject::getProperty(const std::string& name) const {
        std::string nameUpper = boost::algorithm::to_upper_copy(name);
        if (strcmp(nameUpper.c_str(), "OBJECTID")==0)
            return objectId_;
        else if (strcmp(nameUpper.c_str(), "CLASSNAME")==0)
            return className_;
        else if (strcmp(nameUpper.c_str(), "PERMANENT")==0)
            return permanent_;
        else if (strcmp(nameUpper.c_str(), "NUMBER")==0)
            return number_;
        else if (strcmp(nameUpper.c_str(), "TYPE")==0)
            return type_;
        else if (strcmp(nameUpper.c_str(), "BALANCE")==0)
            return balance_;
        else 
            OH_FAIL("Error: attempt to retrieve non-existent Property: '" + name + "'");
    }

    void AccountValueObject::setProperty(const std::string& name, const boost::any& value) {
        std::string nameUpper = boost::algorithm::to_upper_copy(name);
        if (strcmp(nameUpper.c_str(), "OBJECTID")==0)
            objectId_= boost::any_cast<std::string>(value);
        else if (strcmp(nameUpper.c_str(), "CLASSNAME")==0)
            className_ = boost::any_cast<std::string>(value);
        else if (strcmp(nameUpper.c_str(), "PERMANENT")==0)
            permanent_ = boost::any_cast<bool>(value);
        else if (strcmp(nameUpper.c_str(), "NUMBER")==0)
            number_ = boost::any_cast<long>(value);
        else if (strcmp(nameUpper.c_str(), "TYPE")==0)
            type_ = boost::any_cast<std::string>(value);
        else if (strcmp(nameUpper.c_str(), "BALANCE")==0)
            balance_ = boost::any_cast<ObjectHandler::Variant>(value);
        else 
            OH_FAIL("Error: attempt to retrieve non-existent Property: '" + name + "'");
    }

}


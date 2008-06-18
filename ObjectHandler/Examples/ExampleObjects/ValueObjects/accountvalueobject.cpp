
/*
 Copyright (C) 2004, 2005, 2006, 2007 Eric Ehlers
 Copyright (C) 2006, 2008 Plamen Neykov
 Copyright (C) 2008 Nazcatech sprl Belgium

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
        "Customer",
        "Number",
        "Type",
        "Balance"};

    const std::set<std::string>& AccountValueObject::getSystemPropertyNames() const {
        static std::set<std::string> ret;
        if(ret.empty())
            ret = std::set<std::string>(mPropertyNames,
                mPropertyNames + sizeof(mPropertyNames)/sizeof(const char*));
        return ret;
    }

    ObjectHandler::property_t AccountValueObject::getSystemProperty(const std::string& name) const {
        std::string nameUpper = boost::algorithm::to_upper_copy(name);
        if (strcmp(nameUpper.c_str(), "OBJECTID")==0)
            return objectId_;
        else if (strcmp(nameUpper.c_str(), "CLASSNAME")==0)
            return className_;
        else if (strcmp(nameUpper.c_str(), "PERMANENT")==0)
            return permanent_;
        else if (strcmp(nameUpper.c_str(), "CUSTOMER")==0)
            return customer_;
        else if (strcmp(nameUpper.c_str(), "NUMBER")==0)
            return number_;
        else if (strcmp(nameUpper.c_str(), "TYPE")==0)
            return type_;
        else if (strcmp(nameUpper.c_str(), "BALANCE")==0)
            return balance_;
        else 
            OH_FAIL("Error: attempt to retrieve non-existent Property: '" + name + "'");
    }

    void AccountValueObject::setSystemProperty(const std::string& name, const ObjectHandler::property_t& value) {
        std::string nameUpper = boost::algorithm::to_upper_copy(name);
        if (strcmp(nameUpper.c_str(), "OBJECTID")==0)
            objectId_= boost::get<std::string>(value);
        else if (strcmp(nameUpper.c_str(), "CLASSNAME")==0)
            className_ = boost::get<std::string>(value);
        else if (strcmp(nameUpper.c_str(), "PERMANENT")==0)
            permanent_ = boost::get<bool>(value);
        else if (strcmp(nameUpper.c_str(), "CUSTOMER")==0)
            customer_ = boost::get<std::string>(value);
        else if (strcmp(nameUpper.c_str(), "NUMBER")==0)
            number_ = boost::get<long>(value);
        else if (strcmp(nameUpper.c_str(), "TYPE")==0)
            type_ = boost::get<std::string>(value);
        else if (strcmp(nameUpper.c_str(), "BALANCE")==0)
            balance_ = value;
        else 
            OH_FAIL("Error: attempt to retrieve non-existent Property: '" + name + "'");
    }

}


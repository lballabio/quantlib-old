
/*!
 Copyright (C) 2007 Eric Ehlers
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

#include <ExampleObjects/ValueObjects/customervalueobject.hpp>
#include <oh/exception.hpp>
#include <iostream>
#include <string>
#include <boost/algorithm/string/case_conv.hpp>

namespace AccountExample {

    const char* CustomerValueObject::mPropertyNames[] = {
        "ObjectId",
        "ClassName",
        "Permanent",
        "Name",
        "Age"};

    const std::set<std::string>& CustomerValueObject::getSystemPropertyNames() const {
        static std::set<std::string> ret;
        if(ret.empty())
            ret = std::set<std::string>(mPropertyNames,
                mPropertyNames + sizeof(mPropertyNames)/sizeof(const char*));
        return ret;
    }

    ObjectHandler::property_t CustomerValueObject::getSystemProperty(const std::string& name) const {
        std::string nameUpper = boost::algorithm::to_upper_copy(name);
        if (strcmp(nameUpper.c_str(), "OBJECTID")==0)
            return objectId_;
        else if (strcmp(nameUpper.c_str(), "CLASSNAME")==0)
            return className_;
        else if (strcmp(nameUpper.c_str(), "PERMANENT")==0)
            //return (long)permanent_;
            return (bool)permanent_;
        else if (strcmp(nameUpper.c_str(), "NAME")==0)
            return name_;
        else if (strcmp(nameUpper.c_str(), "AGE")==0)
            return age_;
        else 
            OH_FAIL("Error: attempt to retrieve non-existent Property: '" + name + "'");
    }

    void CustomerValueObject::setSystemProperty(const std::string& name, const ObjectHandler::property_t& value) {
        std::string nameUpper = boost::algorithm::to_upper_copy(name);
        if (strcmp(nameUpper.c_str(), "OBJECTID")==0)
            objectId_ = boost::get<std::string>(value);
        else if (strcmp(nameUpper.c_str(), "CLASSNAME")==0)
            className_ = boost::get<std::string>(value);
        else if (strcmp(nameUpper.c_str(), "PERMANENT")==0)
            permanent_ = boost::get<bool>(value);
        else if (strcmp(nameUpper.c_str(), "NAME")==0)
            name_ = boost::get<std::string>(value);
        else if (strcmp(nameUpper.c_str(), "AGE")==0)
            age_ = boost::get<long>(value);
        else 
            OH_FAIL("Error: attempt to retrieve non-existent Property: '" + name + "'");
    }

}



/*!
 Copyright (C) 2004, 2005 Eric Ehlers

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

#include <objectfoo.hpp>
#include <iostream>
#include <sstream>

using namespace std;

ObjectFoo::ObjectFoo(
        const string &s,
        const int &i) {
    foo_        = boost::shared_ptr<Foo>(new Foo(s, i));
    // populate base class Property vector
    any_ptr anyString(new boost::any(foo_->s()));
    any_ptr anyInt(new boost::any(foo_->i()));
    ObjectProperty propString(PROPERTY_STR, anyString);
    ObjectProperty propInt(PROPERTY_INT, anyInt);
    properties_.push_back(propString);
    properties_.push_back(propInt);    
}

// wrapper for underlying member function
void ObjectFoo::update(const string &s, const int &i) {
    foo_->update(s, i);
    // update Property vector
    *properties_[IDX_STR]() = s;
    *properties_[IDX_INT]() = i;    
}

boost::shared_ptr<void> ObjectFoo::getReference() const {
    return boost::static_pointer_cast<void>(foo_);
}

// utility function for updating object of class Foo
const Properties& updateFoo(
        const string &handle,
        const string &s,
        const int &i) {
    boost::shared_ptr<ObjectFoo> object =
        OH_GET_OBJECT(ObjectFoo, handle);
    if (!object) {
        ostringstream msg;
        msg << "FOO_UPDATE: unable to retrieve object " << handle;
        throw exception(msg.str().c_str());
    }
    object->update(s, i);
    return object->getProperties();
}


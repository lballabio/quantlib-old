
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

#ifndef objectfoo_hpp
#define objectfoo_hpp

#include <oh/objhandler.hpp>
#include <foo.hpp>

using namespace ObjHandler;

//! FIXME not sure if this is the correct approach
#define PROPERTY_STR    "PROPERTY_STR"
#define PROPERTY_INT    "PROPERTY_INT"
#define IDX_STR            0
#define IDX_INT            1

class ObjectFoo : public Object {
public:
    ObjectFoo(ArgStack &args);
    virtual boost::shared_ptr<void> getReference() const;
    void update(const std::string &s, const int &i);
private:
    boost::shared_ptr<Foo> foo_;
};

#endif


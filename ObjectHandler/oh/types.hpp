/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2007 Eric Ehlers

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

/*! \file
    \brief enumeration Type - Identify the types supported by Variant classes.
*/

#ifndef oh_types_hpp
#define oh_types_hpp

#include <oh/ohdefines.hpp>
#include <oh/exception.hpp>
#include <iostream>

namespace ObjectHandler {

    //! An enumeration of the types supported by Variant classes.
    /*! These are the types that must be supported by any class which is
        specified as the Variant template argument to a conversion algorithm.
    */
    enum Type { Long, Double, Boolean, String, Array, Null, Error };

    inline std::ostream &operator<<(std::ostream &out, const Type &type) {
        switch (type) {
            case Long:
                return out << "Long";
            case Double:
                return out << "Double";
            case Boolean:
                return out << "Boolean";
            case String:
                return out << "String";
            case Null:
                return out << "Null";
            case Error:
                return out << "Error";
            default:
                OH_FAIL("Unexpected type enumeration: '" << type << "'");
        }
    }

}

#endif


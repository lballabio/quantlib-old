/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2005 Plamen Neykov
 Copyright (C) 2006, 2007 Eric Ehlers

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

#include <oh/Enumerations/enumregistry.hpp>

// This file contains implementations of the three concrete instances of
// the abstract base class Registry.  Each derived class instantiates its own
// instance of the AllTypeMap structure where the enumerations are stored.

namespace ObjectHandler {

    // EnumTypeRegistry

    EnumTypeRegistry *EnumTypeRegistry::instance_;

    EnumTypeRegistry &EnumTypeRegistry::instance() {
        OH_REQUIRE(instance_, "Attempt to reference uninitialized EnumTypeRegistry object");
        return *instance_;
    }

    EnumTypeRegistry::AllTypeMap& EnumTypeRegistry::getAllTypesMap() const {
        static EnumTypeRegistry::AllTypeMap enumTypeMap_;
        return enumTypeMap_;
    }

    // EnumClassRegistry

    EnumClassRegistry *EnumClassRegistry::instance_;

    EnumClassRegistry &EnumClassRegistry::instance() {
        OH_REQUIRE(instance_, "Attempt to reference uninitialized EnumClassRegistry object");
        return *instance_;
    }

    EnumClassRegistry::AllTypeMap& EnumClassRegistry::getAllTypesMap() const {
        static EnumClassRegistry::AllTypeMap enumClassMap_;
        return enumClassMap_;
    }

    // EnumPairRegistry

    EnumPairRegistry *EnumPairRegistry::instance_;

    EnumPairRegistry &EnumPairRegistry::instance() {
        OH_REQUIRE(instance_, "Attempt to reference uninitialized EnumPairRegistry object");
        return *instance_;
    }

    EnumPairRegistry::AllTypeMap& EnumPairRegistry::getAllTypesMap() const {
        static EnumPairRegistry::AllTypeMap enumPairMap_;
        return enumPairMap_;
    }

}


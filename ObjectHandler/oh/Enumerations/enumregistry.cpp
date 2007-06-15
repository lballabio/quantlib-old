
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

    EnumTypeRegistry::AllTypeMap enumTypeMap_;
    EnumTypeRegistry *EnumTypeRegistry::instance_;

    EnumTypeRegistry::EnumTypeRegistry() {
        instance_ = this;
    }

    EnumTypeRegistry::~EnumTypeRegistry() {
        instance_ = 0;
    }

    EnumTypeRegistry &EnumTypeRegistry::instance() {
        OH_REQUIRE(instance_, "Attempt to reference uninitialized EnumTypeRegistry object");
        return *instance_;
    }

    void EnumTypeRegistry::registerType(const std::string &mapID, const std::string &typeID, void *type) const {
        registerTypeImpl(enumTypeMap_, mapID, typeID, type);
    }

    const EnumTypeRegistry::AllTypeMap& EnumTypeRegistry::getAllTypesMap() const {
        return enumTypeMap_;
    }

    std::vector<std::string> EnumTypeRegistry::getAllRegisteredTypes() const {
        return getAllRegisteredTypesImpl(enumTypeMap_);
    }

    std::vector<std::string> EnumTypeRegistry::getTypeElements(const std::string &id) const {
        return getTypeElementsImpl(enumTypeMap_, id);
    }

    void EnumTypeRegistry::deleteTypeMap(const std::string &mapID) {
        enumTypeMap_.erase(mapID);
    }

    // EnumClassRegistry

    EnumClassRegistry::AllTypeMap enumClassMap_;
    EnumClassRegistry *EnumClassRegistry::instance_;

    EnumClassRegistry::EnumClassRegistry() {
        instance_ = this;
    }

    EnumClassRegistry::~EnumClassRegistry() {
        instance_ = 0;
    }

    EnumClassRegistry &EnumClassRegistry::instance() {
        OH_REQUIRE(instance_, "Attempt to reference uninitialized EnumClassRegistry object");
        return *instance_;
    }

    void EnumClassRegistry::registerType(const std::string &mapID, const std::string &typeID, void *type) const {
        registerTypeImpl(enumClassMap_, mapID, typeID, type);
    }

    const EnumClassRegistry::AllTypeMap& EnumClassRegistry::getAllTypesMap() const {
        return enumClassMap_;
    }

    std::vector<std::string> EnumClassRegistry::getAllRegisteredTypes() const {
        return getAllRegisteredTypesImpl(enumClassMap_);
    }

    std::vector<std::string> EnumClassRegistry::getTypeElements(const std::string &id) const {
        return getTypeElementsImpl(enumClassMap_, id);
    }

    // EnumPairRegistry

    EnumPairRegistry::AllTypeMap enumClassPairMap_;
    EnumPairRegistry *EnumPairRegistry::instance_;

    EnumPairRegistry::EnumPairRegistry() {
        instance_ = this;
    }

    EnumPairRegistry::~EnumPairRegistry() {
        instance_ = 0;
    }

    EnumPairRegistry &EnumPairRegistry::instance() {
        OH_REQUIRE(instance_, "Attempt to reference uninitialized EnumPairRegistry object");
        return *instance_;
    }

    void EnumPairRegistry::registerType(const std::string &mapID, const KeyPair &typeID, void *type) const {
        registerTypeImpl(enumClassPairMap_, mapID, typeID, type);
    }

    const EnumPairRegistry::AllTypeMap& EnumPairRegistry::getAllTypesMap() const {
        return enumClassPairMap_;
    }

    std::vector<std::string> EnumPairRegistry::getAllRegisteredTypes() const {
        return getAllRegisteredTypesImpl(enumClassPairMap_);
    }

    std::vector<std::string> EnumPairRegistry::getTypeElements(const std::string &id) const {
        return getTypeElementsImpl(enumClassPairMap_, id);
    }

}



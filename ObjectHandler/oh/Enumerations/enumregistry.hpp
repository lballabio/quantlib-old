
/*
 Copyright (C) 2005 Plamen Neykov
 Copyright (C) 2006, 2007 Eric Ehlers

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

/*! \file
    \brief EnumRegistry Classes - Concrete implementations of class Registry.
*/

#ifndef oh_enumregistry_hpp
#define oh_enumregistry_hpp

#include <oh/ohdefines.hpp>
#include <oh/singleton.hpp>
#include <oh/Enumerations/registry.hpp>

namespace ObjectHandler {

    //! A concrete instantiation of the Registry class, for enumerated types.
    class DLL_API EnumTypeRegistry : public Registry<std::string>, public Singleton<EnumTypeRegistry> {
        friend class Singleton<EnumTypeRegistry>;
    public:
        virtual void registerType(const std::string &mapID, const std::string &typeID, void *type) const;
        virtual const AllTypeMap& getAllTypesMap() const;
        virtual std::vector<std::string> getAllRegisteredTypes() const;
        virtual std::vector<std::string> getTypeElements(const std::string &id) const;
        void deleteTypeMap(const std::string &mapID);

    };

    //! A concrete instantiation of the Registry class, for enumerated classes.
    class DLL_API EnumClassRegistry : public Registry<std::string>, public Singleton<EnumClassRegistry> {
        friend class Singleton<EnumClassRegistry>;
    public:
        virtual void registerType(const std::string &mapID, const std::string &typeID, void *type) const;
        virtual const AllTypeMap& getAllTypesMap() const;
        virtual std::vector<std::string> getAllRegisteredTypes() const;
        virtual std::vector<std::string> getTypeElements(const std::string &id) const;
    };

    //! A concrete instantiation of the Registry class, for "pairs".
    /*! "Pairs" are enumerated classes which are keyed not by strings but by
            std::pair<std::string, std::string>
        This additional flexibility is required for some application domains.
    */
    class DLL_API EnumPairRegistry : public Registry<KeyPair>, public Singleton<EnumPairRegistry> {
        friend class Singleton<EnumPairRegistry>;
    public:
        virtual void registerType(const std::string &mapID, const KeyPair &typeID, void *type) const;
        virtual const AllTypeMap& getAllTypesMap() const;
        virtual std::vector<std::string> getAllRegisteredTypes() const;
        virtual std::vector<std::string> getTypeElements(const std::string &id) const;
    };

}

#endif


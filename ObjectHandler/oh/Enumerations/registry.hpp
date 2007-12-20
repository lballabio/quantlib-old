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

/*! \file
    \brief Class Registry - A registry of enumerated types and classes
*/

#ifndef oh_registry_hpp
#define oh_registry_hpp

#include <oh/ohdefines.hpp>
#include <oh/exception.hpp>
#include <string>
#include <vector>
#include <map>
#include <boost/shared_ptr.hpp>

namespace ObjectHandler {

    //! A pair of strings.
    /*! Derived class EnumPairRegistry instantiates base template class Registry
        with KeyPair instead of std::string, resulting in a structure in which
        enumerations are indexed by two strings instead of one.
    */
    typedef std::pair<std::string, std::string> KeyPair;

    //! A registry of enumerated types and classes.
    /*! Maintain a mapping of text strings to datatypes.
    */
    template <typename KeyClass>
    class Registry {
    public:
        //! A mapping of keys to type instances.
        typedef std::map<KeyClass, void*> TypeMap;
        //! Shared pointer to a type map.
        typedef boost::shared_ptr<TypeMap> TypeMapPtr;
        //! A store of type maps indexed by type.
        typedef std::map<std::string, TypeMapPtr> AllTypeMap;

        //! \name Management of Enumerations
        //@{
        //! Return a reference to the type map.
        /*! The type map would normally be declared as a member variable but that
        isn't possible because std::map cannot be exported across DLL boundaries.
        Instead this pure virtual member function is declared in the base class.
        Derived classes must implement a concrete override of the function, returning
        a (non-const) reference to a static variable declared in a cpp file.

        \todo Make this function protected, and provide a public function returning
        a const reference for use by client classes (e.g. RegistryManager).
        */
        virtual AllTypeMap& getAllTypesMap() const = 0;
        //! Store a type in the registry.
        void registerType(const std::string &mapID, const KeyClass &typeID, void *type) const;
        //! Retrieve the list of types in the AllTypeMap.
        std::vector<std::string> getAllRegisteredTypes() const;
        //! Retrieve the list of instances for a given type.
        std::vector<std::string> getTypeElements(const std::string&) const;
        //! Free any memory allocated to the type map.
        /*! If the derived class has called "new" when initializing the type map,
        then when the application shuts down, a (harmless) memory leak will occur.
        This function should be called to ensure a clean shutdown.
        */
        void deleteTypeMap(const std::string &mapID) const;
        //@}
    protected:
        //! \name Structors
        //@{
        //! Empty constructor.
        Registry() {}
        //! Empty virtual destructor.
        virtual ~Registry() {}
        //@}
    };

    template <typename KeyClass>
    inline void Registry<KeyClass>::registerType(
            const std::string &mapID,
            const KeyClass &typeID,
            void *type) const {
        AllTypeMap &allTypeMap = getAllTypesMap();
        TypeMapPtr typeMapPtr;
        typename AllTypeMap::const_iterator i = allTypeMap.find(mapID);
        if (i == allTypeMap.end()) {
            typeMapPtr = TypeMapPtr(new TypeMap);
            allTypeMap[mapID] = typeMapPtr;
        } else {
            typeMapPtr = i->second;
        }
        (*typeMapPtr)[typeID] = type;
    }

    template <typename KeyClass>
    inline std::vector<std::string> Registry<KeyClass>::getAllRegisteredTypes() const {
        AllTypeMap &allTypeMap = getAllTypesMap();
        std::vector<std::string> ret;
        for(typename AllTypeMap::const_iterator i = allTypeMap.begin(); i != allTypeMap.end(); ++i)
            ret.push_back(i->first);
        return ret;
    }

    template <typename KeyClass>
    inline std::vector<std::string> Registry<KeyClass>::getTypeElements(
        const std::string& id) const {

        AllTypeMap &allTypeMap = getAllTypesMap();
        typename AllTypeMap::const_iterator map = allTypeMap.find(id);
        OH_REQUIRE(map != allTypeMap.end(), "Registry::getTypeElements: invalid enum id: " + id);
        std::vector<std::string> ret;
        for(typename TypeMap::const_iterator i = map->second->begin(); i != map->second->end(); ++i)
            ret.push_back(i->first);
        return ret;
    }

    template <>
    inline std::vector<std::string> Registry<KeyPair>::getTypeElements(
        const std::string& id) const {

        AllTypeMap &allTypeMap = getAllTypesMap();
        AllTypeMap::const_iterator map = allTypeMap.find(id);
        OH_REQUIRE(map != allTypeMap.end(), "Registry::getTypeElements: invalid enum id: " + id);
        std::vector<std::string> ret;
        for(TypeMap::const_iterator i = map->second->begin(); i != map->second->end(); ++i) {
            std::ostringstream s;
            s << i->first.first << " | " << i->first.second;
            ret.push_back(s.str());
        }
        return ret;
    }

    template <typename KeyClass>
    inline void Registry<KeyClass>::deleteTypeMap(const std::string &mapID) const {
        AllTypeMap &allTypeMap = getAllTypesMap();
        allTypeMap.erase(mapID);
    }

}

#endif


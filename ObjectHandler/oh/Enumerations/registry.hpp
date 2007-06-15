
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

        This class incorporates some workarounds to compensate for the fact
        that std::map cannot be exported across DLL boundaries.  Normally the
        AllTypeMap would be a protected member variable, but instead derived
        classes must declare a AllTypeMap as a static variable within their cpp
        files. Base member functions which access the AllTypeMap are declared as
        xxxImpl() and base classes invoke these functions, passing in a reference
        to their respective static AllTypeMap variables.
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
        //! Store a type in the registry.
        virtual void registerType(const std::string &mapID, const KeyClass &typeID, void *type) const = 0;
        //! Retrieve the map of TypeMaps.
        virtual const AllTypeMap& getAllTypesMap() const = 0;
        //! Retrieve the list of types in the AllTypeMap.
        virtual std::vector<std::string> getAllRegisteredTypes() const = 0;
        //! Retrieve the list of instances for a given type.
        virtual std::vector<std::string> getTypeElements(const std::string&) const = 0;
        //@}

    protected:
        //! \name Structors
        //@{
        //! Empty constructor.
        Registry() {}
        //! Empty virtual destructor.
        virtual ~Registry() {}
        //@}

        //! \name Management of Enumerations
        //@{
        //! Store a type in the registry.
        /*! Concrete implementation of functionality common to all base classes.
            Base classes invoke this function from within their concrete instantiations
            of pure virtual function registerType().
        */
        void registerTypeImpl(
                AllTypeMap &allTypeMap,
                const std::string &mapID,
                const KeyClass &typeID,
                void *type) const;
        //! Retrieve the list of types in the AllTypeMap.
        /*! Concrete implementation of functionality common to all base classes.
            Base classes invoke this function from within their concrete instantiations
            of pure virtual function getAllRegisteredTypes().
        */
        std::vector<std::string> getAllRegisteredTypesImpl(const AllTypeMap &allTypeMap) const;
        //! Retrieve the list of instances for a given type.
        /*! Concrete implementation of functionality common to all base classes.
            Base classes invoke this function from within their concrete instantiations
            of pure virtual function getTypeElements().
        */
        std::vector<std::string> getTypeElementsImpl(const AllTypeMap &allTypeMap, const std::string& id) const;
        //@}

    };

    template <typename KeyClass>
    inline void Registry<KeyClass>::registerTypeImpl(
            AllTypeMap &allTypeMap,
            const std::string &mapID,
            const KeyClass &typeID,
            void *type) const {
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
    inline std::vector<std::string> Registry<KeyClass>::getAllRegisteredTypesImpl(
            const AllTypeMap &allTypeMap) const {
        std::vector<std::string> ret;
        for(typename AllTypeMap::const_iterator i = allTypeMap.begin(); i != allTypeMap.end(); ++i)
            ret.push_back(i->first);
        return ret;
    }

    template <typename KeyClass>
    inline std::vector<std::string> Registry<KeyClass>::getTypeElementsImpl(
            const AllTypeMap &allTypeMap, const std::string& id) const {
        typename AllTypeMap::const_iterator map = allTypeMap.find(id);
        OH_REQUIRE(map != allTypeMap.end(), "Registry::getTypeElements: invalid enum id: " + id);
        std::vector<std::string> ret;
        for(typename TypeMap::const_iterator i = map->second->begin(); i != map->second->end(); ++i)
            ret.push_back(i->first);
        return ret;
    }

    template <>
    inline std::vector<std::string> Registry<KeyPair>::getTypeElementsImpl(
            const AllTypeMap &allTypeMap, const std::string& id) const {
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

}

#endif


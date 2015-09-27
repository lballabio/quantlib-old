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
    \brief Class RegistryManager - Manage access to an Enumeration Registry
*/

#ifndef oh_typefactory_hpp
#define oh_typefactory_hpp

#include <oh/enumerations/enumregistry.hpp>
#include <oh/exception.hpp>
#include <boost/algorithm/string/case_conv.hpp>

namespace ObjectHandler {

    //! Convert a std::string to uppercase.
    inline std::string uppercase(const std::string &s) {
        return boost::algorithm::to_upper_copy(s);
    }
    //! Convert a KeyPair to uppercase.
    inline KeyPair uppercase(const KeyPair &s) {
        return KeyPair(boost::algorithm::to_upper_copy(s.first),
                       boost::algorithm::to_upper_copy(s.second));
    }
    //! Write a KeyPair to the given stream.
    inline std::ostream& operator<<(std::ostream& left, const KeyPair &right) {
        left << right.first << ":" << right.second;
        return left;
    }

    //! Manage access to an Enumeration Registry.
    /*! This template class manages the intersection between the given
        template parameters:
        \param T - The type of the Enumeration to be stored
        \param RegistryClass - The structure in which to store it
    */
    template<typename T, typename RegistryClass>
    class RegistryManager {

    protected:

        //! \name Management of Types
        //@{
        //! Retrieve an enumerated type.
        /*! If no type corresponds to the given ID then an exception is thrown.
        */
        template<typename KeyClass>
        void *getType(const KeyClass& id) {
            typename RegistryClass::TypeMapPtr typeMap = getTypeMap();
            KeyClass idUpper = uppercase(id);
            typename RegistryClass::TypeMap::iterator i;
            for (i = typeMap->begin(); i != typeMap->end(); ++i)
                if (uppercase(i->first) == idUpper)
                    return i->second;
            OH_FAIL("Unknown id for Type: " << id);
        }

        //! Determine whether a given type has been registered.
        bool checkType(const std::string& id) {
            typename RegistryClass::TypeMapPtr type_map;
            typename RegistryClass::AllTypeMap::const_iterator i =
                RegistryClass::instance().getAllTypesMap().find(typeid(T).name());
            if (i == RegistryClass::instance().getAllTypesMap().end()) {
                return false;
            } else {
                type_map = i->second;
            }
            std::string idUpper = boost::algorithm::to_upper_copy(id);
            for (typename RegistryClass::TypeMap::iterator i = type_map->begin(); i != type_map->end(); ++i)
                if (boost::algorithm::to_upper_copy(i->first) == idUpper)
                    return true;
            return false;
        }

        //! Register an enumerated type.
        template<typename KeyClass>
        void registerType(const KeyClass& id, void *type) {
            RegistryClass::instance().registerType(typeid(T).name(), id, type);
        }

        //! Unregister an enumerated type.
        /*! All type instances associated with T are deleted.
        */
        void unregisterTypes() {
            typename RegistryClass::TypeMapPtr typeMap = getTypeMap();
            typename RegistryClass::TypeMap::iterator i;
            for (i = typeMap->begin(); i != typeMap->end(); ++i)
                delete static_cast<T*>(i->second);
            RegistryClass::instance().deleteTypeMap(typeid(T).name());
        }
        //@}

    protected:
        const typename RegistryClass::TypeMapPtr &getTypeMap() {
            static typename RegistryClass::TypeMapPtr typeMap;
            if(!typeMap) {
                typename RegistryClass::AllTypeMap::const_iterator i =
                    RegistryClass::instance().getAllTypesMap().find(typeid(T).name());
                OH_REQUIRE(i != RegistryClass::instance().getAllTypesMap().end(),
                    "Error retrieving Enumeration from Registry - the type '"
                    << typeid(T).name() << "' is not available!");
                typeMap = i->second;
            }
            return typeMap;
        }
    };

    //! %Create the enumerated type associated with a string.
    /*! Functor comprising a concrete instantiation of base template class
        RegistryManager.  The () operator returns the required type, e.g.
        \code
        Account::Type accountTypeEnumeration =
            ObjectHandler::Create<Account::Type>()("Savings");
        \endcode
    */
    template<typename T>
    class Create : private RegistryManager<T, EnumTypeRegistry> {
    public:

        T operator()(const std::string& id) {
            return *(static_cast<T*>(this->getType(id)));
        }

        using RegistryManager<T, EnumTypeRegistry>::registerType;
        using RegistryManager<T, EnumTypeRegistry>::unregisterTypes;
    };

    //! Convert a list of strings into the associated enumerations.
    template <class T>
    std::vector<T> vectorStringToEnum(
        const std::vector<std::string> ids,
        const std::string &paramName) {

        try {
            std::vector<T> returnValue;
            for (std::vector<std::string>::const_iterator i = ids.begin(); i != ids.end(); ++i)
                returnValue.push_back(Create<T>()(*i));
            return returnValue;
        } catch (const std::exception &e) {
            OH_FAIL("vectorStringToEnum: error converting parameter '" << paramName 
                << "' to type '" << typeid(T).name() << "' : " << e.what());
        }
    }

 }

#endif

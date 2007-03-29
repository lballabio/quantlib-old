
/*
 Copyright (C) 2005 Plamen Neykov
 Copyright (C) 2006 Eric Ehlers

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

#ifndef qla_typefactory_hpp
#define qla_typefactory_hpp

#include <ql/errors.hpp>
#include <qlo/typeregistry.hpp>
#include <oh/exception.hpp>
#include <boost/algorithm/string/case_conv.hpp>

namespace QuantLibAddin {

    // some utilities required by class RegistryManager

    inline std::string uppercase(const std::string &s) {
        return boost::algorithm::to_upper_copy(s);
    }

    inline KeyPair uppercase(const KeyPair &s) {
        return KeyPair(boost::algorithm::to_upper_copy(s.first),
                       boost::algorithm::to_upper_copy(s.second));
    }

    inline std::ostream& operator<<(std::ostream& left, const KeyPair &right) {
        left << right.first << ":" << right.second;
        return left;
    }

    template<typename T, typename RegistryClass>
    class RegistryManager {
    protected:
        template<typename KeyClass>
        void *getType(const KeyClass& id) {
            typename RegistryClass::TypeMapPtr type_map = getTypeMap();
            KeyClass idUpper = QuantLibAddin::uppercase(id);
            typename RegistryClass::TypeMap::iterator i;
            for (i = type_map->begin(); i != type_map->end(); ++i)
                if (uppercase(i->first) == idUpper)
                    return i->second;
            QL_FAIL("Unknown id for Type: " << id);
        }

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

        void registerType(const std::string& id, void *type) {
            typename RegistryClass::TypeMapPtr type_map = getTypeMap();
            (*type_map)[id] = type;
        }
    private:
        const typename RegistryClass::TypeMapPtr &getTypeMap() {
            static typename RegistryClass::TypeMapPtr type_map;
            if(!type_map) {
                typename RegistryClass::AllTypeMap::const_iterator i =
                    RegistryClass::instance().getAllTypesMap().find(typeid(T).name());
                QL_REQUIRE(i != RegistryClass::instance().getAllTypesMap().end(), 
                    "Error retrieving Enumeration from Registry - the type '"
                    << typeid(T).name() << "' is not available!");
                type_map = i->second;
            }
            return type_map;
        }
    };

    /* *** Enumerated Types *** */

    /* *** Generic *** */
    template<typename T>
    class Create : private RegistryManager<T, EnumTypeRegistry> {
    public:
        T operator()(const std::string& id) {
            return *(static_cast<T*>(this->getType(id)));
        }
        using RegistryManager<T, EnumTypeRegistry>::checkType;
    };
  
 }

#endif


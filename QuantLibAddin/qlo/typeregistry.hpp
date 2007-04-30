
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

#ifndef qla_typeregistry_hpp
#define qla_typeregistry_hpp

#include <oh/ohdefines.hpp>
#include <ql/patterns/singleton.hpp>
#include <map>
#include <vector>
#include <string>

namespace QuantLibAddin {

    template <typename KeyClass>
    class Registry {
    public:
        typedef std::map<KeyClass, void*> TypeMap;
        typedef boost::shared_ptr<TypeMap> TypeMapPtr;
        typedef std::map<std::string, TypeMapPtr> AllTypeMap;

        std::vector<std::string> getAllRegisteredTypes() const;
        std::vector<std::string> getTypeElements(const std::string&) const;
        const AllTypeMap& getAllTypesMap() const { return allTypesMap; }

    protected:
        AllTypeMap allTypesMap;
        Registry() {}
    };

    class EnumTypeRegistry : public Registry<std::string>, public QuantLib::Singleton<EnumTypeRegistry> {
        friend class QuantLib::Singleton<EnumTypeRegistry>;
    public:
        ~EnumTypeRegistry();
    private:
        EnumTypeRegistry();
    };

    class EnumClassRegistry : public Registry<std::string>, public QuantLib::Singleton<EnumClassRegistry> {
        friend class QuantLib::Singleton<EnumClassRegistry>;
    private:
        EnumClassRegistry();
    };

    typedef std::pair<std::string, std::string> KeyPair;
    class EnumCurveRegistry : public Registry<KeyPair>, public QuantLib::Singleton<EnumCurveRegistry> {
        friend class QuantLib::Singleton<EnumCurveRegistry>;
    private:
        EnumCurveRegistry();
    };

}

#endif


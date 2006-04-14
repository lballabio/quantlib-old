
/*
 Copyright (C) 2005 Plamen Neykov

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

#include <oh/objhandlerdefines.hpp>
#include <ql/Patterns/singleton.hpp>
#include <map>
#include <vector>
#include <string>

namespace QuantLibAddin {

    typedef std::map<std::string, void*> TypeMap;
    typedef boost::shared_ptr<TypeMap> TypeMapPtr;
    typedef std::map<std::string, TypeMapPtr> AllTypeMap;

    class Registry {
    public:
        std::vector<std::string> getAllRegisteredTypes() const;
        std::vector<std::string> getTypeElements(const std::string&) const;
        const AllTypeMap& getAllTypesMap() const { return allTypesMap; }

    protected:
        AllTypeMap allTypesMap;
        Registry() {}
    };

    class EnumRegistry : public Registry, public QuantLib::Singleton<EnumRegistry> {
        friend class QuantLib::Singleton<EnumRegistry>;
    private:
        EnumRegistry();
    };

    class ComplexTypeRegistry : public Registry, public QuantLib::Singleton<ComplexTypeRegistry> {
        friend class QuantLib::Singleton<ComplexTypeRegistry>;
    private:
        ComplexTypeRegistry();
    };
}

#endif


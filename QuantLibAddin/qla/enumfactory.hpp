
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

#ifndef qla_enumfactory_hpp
#define qla_enumfactory_hpp

#include <ql/Patterns/singleton.hpp>
#include <ql/basicdataformatters.hpp>
#include <map>
#include <vector>

namespace QuantLibAddin {

    typedef std::map<std::string, void*> TypeMap;
	typedef boost::shared_ptr<TypeMap> TypeMapPtr;

	class EnumTypeFactory : public QuantLib::Singleton<EnumTypeFactory> {
		friend class QuantLib::Singleton<EnumTypeFactory>;
	public:
		std::vector<std::string> getAllRegisteredEnums() const;
		std::vector<std::string> getEnumElements(const std::string&) const;

		std::map<std::string, TypeMapPtr> allTypesMap;
	private:
		EnumTypeFactory();
	};

    template<typename T>
    class CreateEnum {
    public:
	    static T create(std::string& id) {
		    static TypeMapPtr type_map;
		    if(!type_map) {
			    QL_REQUIRE(
                    EnumTypeFactory::instance().allTypesMap.find(typeid(T).name()) != 
                    EnumTypeFactory::instance().allTypesMap.end(), 
                    "Type not registered!");
			    type_map = 
                    EnumTypeFactory::instance().allTypesMap.find(typeid(T).name())->second;
		    }
            std::string idUpper = QuantLib::StringFormatter::toUppercase(id);
		    TypeMap::iterator type = type_map->find(idUpper);
		    QL_REQUIRE(type != type_map->end(), "Unknown id for Type: " + id);
		    return *(static_cast<T*>(type->second));
	    }
    };

}

#endif

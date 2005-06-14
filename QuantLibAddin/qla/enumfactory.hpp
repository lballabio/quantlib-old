
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

	class EnumTypeFactory : public QuantLib::Singleton<EnumTypeFactory> {
		friend class QuantLib::Singleton<EnumTypeFactory>;
	public:
        typedef std::map<std::string, void*> TypeMap;
	    typedef boost::shared_ptr<TypeMap> TypeMapPtr;

		std::vector<std::string> getAllRegisteredEnums() const;
		std::vector<std::string> getEnumElements(const std::string&) const;

        template<typename T>
#ifdef QL_PATCH_MSVC6
    #define ALL_TYPES_MAP instance().allTypesMap
        class CreateEnum {
        public:
	        static 
#else
    #define ALL_TYPES_MAP allTypesMap
#endif
            T create(const std::string& id) {
		        static TypeMapPtr type_map;
		        if(!type_map) {
			        QL_REQUIRE(
                        ALL_TYPES_MAP.find(typeid(T).name()) != 
                        ALL_TYPES_MAP.end(), 
                        "Type not registered!");
			        type_map = 
                        ALL_TYPES_MAP.find(typeid(T).name())->second;
		        }
                std::string idUpper = QuantLib::StringFormatter::toUppercase(id);
		        TypeMap::iterator type = type_map->find(idUpper);
		        QL_REQUIRE(type != type_map->end(), "Unknown id for Type: " + id);
		        return *(static_cast<T*>(type->second));
	        }
#ifdef QL_PATCH_MSVC6
        };

		std::map<std::string, TypeMapPtr> allTypesMap;
	private:
#else
	private:
		std::map<std::string, TypeMapPtr> allTypesMap;
#endif

		EnumTypeFactory();
	};

#ifdef QL_PATCH_MSVC6
    #define CREATE_ENUM( CLASS, VARIABLE ) \
    EnumTypeFactory::CreateEnum< CLASS >::create( VARIABLE )
#else
    #define CREATE_ENUM( CLASS, VARIABLE ) \
    EnumTypeFactory::instance().create< CLASS >( VARIABLE )
#endif

}

#endif


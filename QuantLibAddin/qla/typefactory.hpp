
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

#ifndef qla_typefactory_hpp
#define qla_typefactory_hpp

#include <qla/typeregistry.hpp>
#include <ql/Instruments/payoffs.hpp>
#include <ql/Utilities/strings.hpp>

namespace QuantLibAddin {

    template<typename T, typename RegistryClass>
    class RegistryManager {
    protected:
        void* getType(const std::string& id) {
            static TypeMapPtr type_map;
            if(!type_map) {
                QL_REQUIRE(                    
                    RegistryClass::instance().getAllTypesMap().find(typeid(T).name()) != 
                    RegistryClass::instance().getAllTypesMap().end(), 
                    "Type not registered!");
                type_map = 
                    RegistryClass::instance().getAllTypesMap().find(typeid(T).name())->second;
            }
            std::string idUpper = QuantLib::uppercase(id);
            TypeMap::iterator type = type_map->find(idUpper);
            QL_REQUIRE(type != type_map->end(), "Unknown id for Type: " + id);
            return type->second;
        }
    };

    template<typename T>
    class Create : private RegistryManager<T, EnumRegistry> {
    public:
        T operator()(const std::string& id) {
            return *(static_cast<T*>(this->getType(id)));
        }
    };

    template<>
    class Create<boost::shared_ptr<QuantLib::Exercise> > : 
        private RegistryManager<QuantLib::Exercise, ComplexTypeRegistry> {
    public:
        boost::shared_ptr<QuantLib::Exercise> operator()(const std::string& exerciseID,
                                                            const long &exerciseDate,
                                                            const long &settlementDate) {
            boost::shared_ptr<QuantLib::Exercise>(*ctor)(const long&, const long&) =
                (boost::shared_ptr<QuantLib::Exercise>(*)(const long&, const long&)) getType(exerciseID);
            return ctor(exerciseDate, settlementDate);
        }
    };
    
    template<>
    class Create<boost::shared_ptr<QuantLib::StrikedTypePayoff> > :
        private RegistryManager<QuantLib::StrikedTypePayoff, ComplexTypeRegistry> {
    public:
        boost::shared_ptr<QuantLib::StrikedTypePayoff> operator()(const std::string& optionTypeID,
                                                                    const std::string& payoffID,
                                                                    const double& input1,
                                                                    const double& input2 = 0.) {
            boost::shared_ptr<QuantLib::StrikedTypePayoff>(*ctor)(const std::string&, const double&, const double&) =
                (boost::shared_ptr<QuantLib::StrikedTypePayoff>(*)(const std::string&, const double&, const double&))
                    getType(payoffID);
            return ctor(optionTypeID, input1, input2);
        }
    };

    template<>
    class Create<boost::shared_ptr<QuantLib::PricingEngine> > :
        private RegistryManager<QuantLib::PricingEngine, ComplexTypeRegistry> {
    public:
        boost::shared_ptr<QuantLib::PricingEngine> operator()(const std::string& engineID,
                                                                const long& timeSteps) {
            boost::shared_ptr<QuantLib::PricingEngine>(*ctor)(const long&) = 
                (boost::shared_ptr<QuantLib::PricingEngine>(*)(const long&)) getType(engineID);
            return ctor(timeSteps);
        }
    };
 }

#endif


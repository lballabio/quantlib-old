
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

#ifndef qla_pricingenginesfactory_hpp
#define qla_pricingenginesfactory_hpp


#include <qlo/typefactory.hpp>
#include <ql/pricingengine.hpp>
#include <qlo/pricingengines.hpp>

namespace QuantLibAddin {



    /* *** PricingEngine *** */
    typedef boost::shared_ptr<QuantLib::PricingEngine>(*PricingEngineConstructor)(
        const long&);

    template<>
    class Create<boost::shared_ptr<QuantLib::PricingEngine> > :
        private RegistryManager<QuantLib::PricingEngine, EnumClassRegistry> {
    public:
        boost::shared_ptr<QuantLib::PricingEngine> operator()(const std::string& engineID,
                                                              const long& timeSteps) {
            // FIXME move this validation into QL
            QL_REQUIRE(timeSteps>0, "timeSteps must be positive");  
            PricingEngineConstructor pricingEngineConstructor =
                (PricingEngineConstructor)(getType(engineID));
            return pricingEngineConstructor(timeSteps);
        }
    };

 }

#endif


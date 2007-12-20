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

#ifndef qla_pricingenginesfactory_hpp
#define qla_pricingenginesfactory_hpp

#include <oh/Enumerations/typefactory.hpp>
#include <ql/pricingengine.hpp>
#include <qlo/pricingengines.hpp>

namespace ObjectHandler {

    // This factory uses a slightly different design than the others.  Usually Create
    // - retrieves the constructor
    // - calls the constructor
    // - returns the constructed object
    // In this case Create simply returns the constructor, and the caller must invoke
    // it.  This approach is required to allow two different Create<> class template
    // specializations for the same type (QuantLib::PricingEngine).  One specialization
    // is for PricingEngines that do not require the timesteps parameter and the other
    // specialization is for PricingEngines that do require the timesteps parameter.

    // A function to construct a PricingEngine - without timesteps
    typedef boost::shared_ptr<QuantLib::PricingEngine>(*PricingEngineConstructor1)(
        const boost::shared_ptr<QuantLib::GeneralizedBlackScholesProcess>&);

    // A function to construct a PricingEngine - with timesteps
    typedef boost::shared_ptr<QuantLib::PricingEngine>(*PricingEngineConstructor2)(
        const boost::shared_ptr<QuantLib::GeneralizedBlackScholesProcess>&, const long&);

    // Create class template specialization for PricingEngine - without timesteps
    template<>
    class Create<PricingEngineConstructor1> :
        private RegistryManager<PricingEngineConstructor1, EnumClassRegistry> {
    public:
        PricingEngineConstructor1 operator()(const std::string& engineID) {
            return reinterpret_cast<PricingEngineConstructor1>(getType(engineID));
        }
        using RegistryManager<PricingEngineConstructor1, EnumClassRegistry>::registerType;
    };

    // Create class template specialization for PricingEngine - with timesteps
    template<>
    class Create<PricingEngineConstructor2> :
        private RegistryManager<PricingEngineConstructor2, EnumClassRegistry> {
    public:
        PricingEngineConstructor2 operator()(const std::string& engineID) {
            return reinterpret_cast<PricingEngineConstructor2>(getType(engineID));
        }
        using RegistryManager<PricingEngineConstructor2, EnumClassRegistry>::registerType;
    };

    // A wrapper for Create - without timesteps
    inline boost::shared_ptr<QuantLib::PricingEngine> createPricingEngine(
        const std::string& engineID,
        const boost::shared_ptr<QuantLib::GeneralizedBlackScholesProcess>& process) {

        return Create<PricingEngineConstructor1>()(engineID)(process);
    }

    // A wrapper for Create - with timesteps
    inline boost::shared_ptr<QuantLib::PricingEngine> createPricingEngine(
        const std::string& engineID,
        const boost::shared_ptr<QuantLib::GeneralizedBlackScholesProcess>& process,
        const long& timeSteps) {

        return Create<PricingEngineConstructor2>()(engineID)(process, timeSteps);
    }

 }

#endif

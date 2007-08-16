
/*
 Copyright (C) 2006, 2007 Ferdinando Ametrano

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


#include <qlo/pricingengines.hpp>
#include <qlo/swaption.hpp>
#include <qlo/Enumerations/Factories/pricingenginesfactory.hpp>

#include <ql/pricingengines/blackscholescalculator.hpp>
#include <ql/pricingengines/capfloor/analyticcapfloorengine.hpp>
#include <ql/pricingengines/capfloor/blackcapfloorengine.hpp> 
#include <ql/pricingengines/capfloor/marketmodelcapfloorengine.hpp>
#include <ql/pricingengines/bond/bondengine.hpp>

namespace QuantLibAddin {

    PricingEngine::PricingEngine(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const std::string& engineID,
            const long& timeSteps,
            bool permanent) : ObjectHandler::LibraryObject<QuantLib::PricingEngine>(properties, permanent)
    {
        libraryObject_ = ObjectHandler::Create<boost::shared_ptr<
            QuantLib::PricingEngine> >()(engineID, timeSteps);
    }

    PricingEngine::PricingEngine(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const std::string& engineID,
            bool permanent) : ObjectHandler::LibraryObject<QuantLib::PricingEngine>(properties, permanent)
    {
        libraryObject_ = ObjectHandler::Create<boost::shared_ptr<
            QuantLib::PricingEngine> >()(engineID, 1);
    }

    BlackSwaptionEngine::BlackSwaptionEngine(
        const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
        const QuantLib::Handle<QuantLib::SwaptionVolatilityStructure>& vol,
        const QuantLib::Handle<QuantLib::YieldTermStructure>& discountCurve,
        bool permanent) : PricingEngine(properties, permanent)
    {
        libraryObject_ = boost::shared_ptr<QuantLib::PricingEngine>(new
            QuantLib::BlackSwaptionEngine(vol, discountCurve));
    }

    BlackCapFloorEngine::BlackCapFloorEngine(
        const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
        const QuantLib::Handle<QuantLib::Quote>& vol,
        bool permanent) : PricingEngine(properties, permanent)
    {
        libraryObject_ = boost::shared_ptr<QuantLib::PricingEngine>(new
            QuantLib::BlackCapFloorEngine(vol));
    }

    BlackCapFloorEngine::BlackCapFloorEngine(
        const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
        const QuantLib::Handle<QuantLib::CapletVolatilityStructure>& vol,
        bool permanent) : PricingEngine(properties, permanent)
    {
        libraryObject_ = boost::shared_ptr<QuantLib::PricingEngine>(new
            QuantLib::BlackCapFloorEngine(vol));
    }

    AnalyticCapFloorEngine::AnalyticCapFloorEngine(
        const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
        const boost::shared_ptr < QuantLib::AffineModel >& model,
        bool permanent) : PricingEngine(properties, permanent)
    {
        libraryObject_ = boost::shared_ptr<QuantLib::PricingEngine>(new
            QuantLib::AnalyticCapFloorEngine(model));
    }
    
    MarketModelCapFloorEngine::MarketModelCapFloorEngine(
        const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
        const boost::shared_ptr<QuantLib::MarketModelFactory>& factory,
        bool permanent) : PricingEngine(properties, permanent)
    {
        libraryObject_ = boost::shared_ptr<QuantLib::PricingEngine>(new
            QuantLib::MarketModelCapFloorEngine(factory));
    }

    BlackCalculator::BlackCalculator(
        const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
        const boost::shared_ptr<QuantLib::StrikedTypePayoff>& payoff,
        QuantLib::Real forward,
        QuantLib::Real stdDev,
        QuantLib::DiscountFactor discount,
        bool permanent)
        : ObjectHandler::LibraryObject<QuantLib::BlackCalculator>(properties, permanent) {
        libraryObject_ = boost::shared_ptr<QuantLib::BlackCalculator>(new
            QuantLib::BlackCalculator(payoff, forward, stdDev, discount));
    }

    BlackScholesCalculator::BlackScholesCalculator(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const boost::shared_ptr<QuantLib::StrikedTypePayoff>& payoff,
            QuantLib::Real spot,
            QuantLib::DiscountFactor growth,
            QuantLib::Real stdDev,
            QuantLib::DiscountFactor discount,
            bool permanent) : BlackCalculator(properties, permanent)
    {
        libraryObject_ = boost::shared_ptr<QuantLib::BlackCalculator>(new
            QuantLib::BlackScholesCalculator(payoff, spot, growth, stdDev,
                                             discount));
    }

    BondEngine::BondEngine(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const QuantLib::Handle<QuantLib::YieldTermStructure>& discountCurve,
            bool permanent) : PricingEngine(properties, permanent)
    {
        libraryObject_ = boost::shared_ptr<QuantLib::PricingEngine>(new
            QuantLib::BondEngine(discountCurve));
    }

}


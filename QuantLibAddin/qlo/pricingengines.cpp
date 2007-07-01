
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

namespace QuantLibAddin {

    PricingEngine::PricingEngine(const std::string& engineID,
                                 const long& timeSteps)
    {
        libraryObject_ = ObjectHandler::Create<boost::shared_ptr<
            QuantLib::PricingEngine> >()(engineID, timeSteps);
    }

    PricingEngine::PricingEngine(const std::string& engineID)
    {
        libraryObject_ = ObjectHandler::Create<boost::shared_ptr<
            QuantLib::PricingEngine> >()(engineID, 1);
    }

    BlackSwaptionEngine::BlackSwaptionEngine(
        const QuantLib::Handle<QuantLib::SwaptionVolatilityStructure>& vol)
    {
        libraryObject_ = boost::shared_ptr<QuantLib::PricingEngine>(new
            QuantLib::BlackSwaptionEngine(vol));
    }

    BlackCapFloorEngine::BlackCapFloorEngine(
        const QuantLib::Handle<QuantLib::Quote>& vol)
    {
        libraryObject_ = boost::shared_ptr<QuantLib::PricingEngine>(new
            QuantLib::BlackCapFloorEngine(vol));
    }

    BlackCapFloorEngine::BlackCapFloorEngine(
        const QuantLib::Handle<QuantLib::CapletVolatilityStructure>& vol)
    {
        libraryObject_ = boost::shared_ptr<QuantLib::PricingEngine>(new
            QuantLib::BlackCapFloorEngine(vol));
    }

    AnalyticCapFloorEngine::AnalyticCapFloorEngine(
        const boost::shared_ptr < QuantLib::AffineModel >& model)
    {
        libraryObject_ = boost::shared_ptr<QuantLib::PricingEngine>(new
            QuantLib::AnalyticCapFloorEngine(model));
    }
    
    MarketModelCapFloorEngine::MarketModelCapFloorEngine(
            const boost::shared_ptr<QuantLib::MarketModelFactory>& factory)
    {
        libraryObject_ = boost::shared_ptr<QuantLib::PricingEngine>(new
            QuantLib::MarketModelCapFloorEngine(factory));
    }

    BlackCalculator::BlackCalculator(
            const boost::shared_ptr<QuantLib::StrikedTypePayoff>& payoff,
            QuantLib::Real forward,
            QuantLib::Real stdDev,
            QuantLib::DiscountFactor discount) {
        libraryObject_ = boost::shared_ptr<QuantLib::BlackCalculator>(new
            QuantLib::BlackCalculator(payoff, forward, stdDev, discount));
    }

    BlackScholesCalculator::BlackScholesCalculator(
            const boost::shared_ptr<QuantLib::StrikedTypePayoff>& payoff,
            QuantLib::Real spot,
            QuantLib::DiscountFactor growth,
            QuantLib::Real stdDev,
            QuantLib::DiscountFactor discount)
    {
        libraryObject_ = boost::shared_ptr<QuantLib::BlackCalculator>(new
            QuantLib::BlackScholesCalculator(payoff, spot, growth, stdDev,
                                             discount));
    }

}

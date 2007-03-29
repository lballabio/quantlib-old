
/*
 Copyright (C) 2006 Ferdinando Ametrano

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


#include <qlo/pricingengines.hpp>
#include <qlo/Factories/pricingenginesfactory.hpp>
#include <ql/PricingEngines/blackscholescalculator.hpp>

namespace QuantLibAddin {

    PricingEngine::PricingEngine(const std::string &engineID, const long& timeSteps)
    {
        libraryObject_ = Create<boost::shared_ptr<QuantLib::PricingEngine> >()(engineID, timeSteps);
    }

    PricingEngine::PricingEngine(const std::string &engineID)
    {
        libraryObject_ = Create<boost::shared_ptr<QuantLib::PricingEngine> >()(engineID, 1);
    }

    BlackSwaptionEngine::BlackSwaptionEngine(
        const QuantLib::Handle<QuantLib::SwaptionVolatilityStructure>& vol)
    {
        libraryObject_ = boost::shared_ptr<QuantLib::PricingEngine>(
            new QuantLib::BlackSwaptionEngine(vol));
    }

    BlackCapFloorEngine::BlackCapFloorEngine(
        const QuantLib::Handle<QuantLib::CapletVolatilityStructure>& vol)
    {
        libraryObject_ = boost::shared_ptr<QuantLib::PricingEngine>(
            new QuantLib::BlackCapFloorEngine(vol));
    }

    BlackCapFloorEngine::BlackCapFloorEngine(QuantLib::Volatility vol)
    {

        quote_ = boost::shared_ptr<QuantLib::SimpleQuote>(
            new QuantLib::SimpleQuote(vol));
        quoteHandle_.linkTo(quote_);

        libraryObject_ = boost::shared_ptr<QuantLib::PricingEngine>(
            new QuantLib::BlackCapFloorEngine(quoteHandle_));

        //QuantLib::Handle<QuantLib::Quote> vol_hq(
        //    boost::shared_ptr<QuantLib::Quote>(
        //        new QuantLib::SimpleQuote(vol)));

        //libraryObject_ = boost::shared_ptr<QuantLib::PricingEngine>(
        //    new QuantLib::BlackCapFloorEngine(vol_hq));
    }

    AnalyticCapFloorEngine::AnalyticCapFloorEngine(
        const boost::shared_ptr < QuantLib::AffineModel >& model) {

        libraryObject_ = boost::shared_ptr<QuantLib::PricingEngine>(
            new QuantLib::AnalyticCapFloorEngine(model));
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
        QL_REQUIRE(discount>0.0, "discount must be greater than zero: " <<
                   discount << " not allowed");
        libraryObject_ = boost::shared_ptr<QuantLib::BlackCalculator>(new
            QuantLib::BlackScholesCalculator(payoff, spot, growth, stdDev,
                                             discount));
    }

}


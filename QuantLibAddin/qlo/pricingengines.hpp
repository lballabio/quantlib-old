
/*
 Copyright (C) 2006 Ferdinando Ametrano
 Copyright (C) 2006 Cristina Duminuco

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

#ifndef qla_pricingengine_hpp
#define qla_pricingengine_hpp

#include <oh/objhandler.hpp>
#include <ql/pricingengine.hpp>
#include <ql/pricingengines/blackcalculator.hpp>
#include <ql/pricingengines/swaption/blackswaptionengine.hpp>
#include <ql/pricingengines/capfloor/blackcapfloorengine.hpp>
#include <ql/pricingengines/capfloor/analyticcapfloorengine.hpp>
#include <ql/pricingengines/capfloor/marketmodelcapfloorengine.hpp>

namespace QuantLibAddin {

    class PricingEngine : public ObjHandler::LibraryObject<QuantLib::PricingEngine> {
      public:
          PricingEngine(const std::string &engineID);
          PricingEngine(const std::string &engineID, const long& timeSteps);
      protected:
          PricingEngine() {}
    };

    class BlackSwaptionEngine : public PricingEngine {
      public:
          BlackSwaptionEngine(
              const QuantLib::Handle<QuantLib::SwaptionVolatilityStructure>&);
    };

    class BlackCapFloorEngine : public PricingEngine {
      public:
        BlackCapFloorEngine(QuantLib::Volatility);
        BlackCapFloorEngine(
            const QuantLib::Handle<QuantLib::CapletVolatilityStructure>&);
    private:
        boost::shared_ptr<QuantLib::SimpleQuote> quote_;
        QuantLib::RelinkableHandle<QuantLib::Quote> quoteHandle_;
    };

    class AnalyticCapFloorEngine : public PricingEngine {
      public:
        AnalyticCapFloorEngine(
            const boost::shared_ptr<QuantLib::AffineModel>& model);
    };
    class MarketModelCapFloorEngine : public PricingEngine  {
      public:
        MarketModelCapFloorEngine(const boost::shared_ptr<QuantLib::MarketModelFactory>&);
    }; 
    class BlackCalculator : public ObjHandler::LibraryObject<QuantLib::BlackCalculator> {
      public:
        BlackCalculator(
            const boost::shared_ptr<QuantLib::StrikedTypePayoff>& payoff,
            QuantLib::Real forward,
            QuantLib::Real variance,
            QuantLib::DiscountFactor discount);
      protected:
          BlackCalculator() {}
    };

    class BlackScholesCalculator : public BlackCalculator {
      public:
        BlackScholesCalculator(
            const boost::shared_ptr<QuantLib::StrikedTypePayoff>& payoff,
            QuantLib::Real spot,
            QuantLib::DiscountFactor growth,
            QuantLib::Real variance,
            QuantLib::DiscountFactor discount);
    };

}

#endif

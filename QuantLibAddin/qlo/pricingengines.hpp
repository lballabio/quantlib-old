
/*
 Copyright (C) 2006 Ferdinando Ametrano
 Copyright (C) 2006 Cristina Duminuco

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

#ifndef qla_pricingengine_hpp
#define qla_pricingengine_hpp

#include <oh/libraryobject.hpp>

#include <ql/types.hpp>

namespace QuantLib {
    class SimpleQuote;
    class AffineModel;
    class MarketModelFactory;
    class SwaptionVolatilityStructure;
    class OptionletVolatilityStructure;
    class BlackCapFloorEngine;
    class AnalyticCapFloorEngine;
    class MarketModelCapFloorEngine;
    class BlackCalculator;
    class StrikedTypePayoff;
    class PricingEngine;
    class Quote;
    class YieldTermStructure;
    class DiscountingBondEngine;

    template <class T>
    class Handle;
}

namespace QuantLibAddin {

    class PricingEngine : public ObjectHandler::LibraryObject<QuantLib::PricingEngine> {
      public:
        PricingEngine(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const std::string& engineID,
            bool permanent);
        PricingEngine(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const std::string& engineID,
            const long& timeSteps,
            bool permanent);
      protected:
        OH_LIB_CTOR(PricingEngine, QuantLib::PricingEngine);
    };

    class BlackSwaptionEngine : public PricingEngine {
      public:
          BlackSwaptionEngine(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const QuantLib::Handle<QuantLib::SwaptionVolatilityStructure>&,
            const QuantLib::Handle<QuantLib::YieldTermStructure>&,
            bool permanent);
    };

    class BlackCapFloorEngine : public PricingEngine {
      public:
        BlackCapFloorEngine(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const QuantLib::Handle<QuantLib::Quote>& vol,
            bool permanent);
        BlackCapFloorEngine(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const QuantLib::Handle<QuantLib::OptionletVolatilityStructure>&,
            bool permanent);
    };

    class AnalyticCapFloorEngine : public PricingEngine {
      public:
        AnalyticCapFloorEngine(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const boost::shared_ptr<QuantLib::AffineModel>& model,
            bool permanent);
    };

    class MarketModelCapFloorEngine : public PricingEngine  {
      public:
        MarketModelCapFloorEngine(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const boost::shared_ptr<QuantLib::MarketModelFactory>&,
            bool permanent);
    };

    class BlackCalculator : public ObjectHandler::LibraryObject<QuantLib::BlackCalculator> {
      public:
        BlackCalculator(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const boost::shared_ptr<QuantLib::StrikedTypePayoff>& payoff,
            QuantLib::Real forward,
            QuantLib::Real variance,
            QuantLib::DiscountFactor discount,
            bool permanent);
      protected:
        //BlackCalculator() {}
        OH_LIB_CTOR(BlackCalculator, QuantLib::BlackCalculator);
    };

    class BlackScholesCalculator : public BlackCalculator {
      public:
        BlackScholesCalculator(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const boost::shared_ptr<QuantLib::StrikedTypePayoff>& payoff,
            QuantLib::Real spot,
            QuantLib::DiscountFactor growth,
            QuantLib::Real variance,
            QuantLib::DiscountFactor discount,
            bool permanent);
    };

    class BondEngine : public PricingEngine {
      public:
          BondEngine(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const QuantLib::Handle<QuantLib::YieldTermStructure>& discountCurve,
            bool permanent);
    };
}

#endif


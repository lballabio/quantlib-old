
/*
 Copyright (C) 2005, 2006 Eric Ehlers

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

#ifndef qla_asianoption_hpp
#define qla_asianoption_hpp

#include <qlo/baseinstruments.hpp>

#include <ql/instruments/averagetype.hpp>
#include <ql/types.hpp>

namespace QuantLib {
    class GeneralizedBlackScholesProcess;
    class StrikedTypePayoff;
    class Exercise;
    class PricingEngine;
    class Date;
}   

namespace QuantLibAddin {

    class ContinuousAveragingAsianOption : public OneAssetOption {
      public:
        ContinuousAveragingAsianOption(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            QuantLib::Average::Type averageType,
            const boost::shared_ptr<QuantLib::GeneralizedBlackScholesProcess >& blackScholesProcess,
            const boost::shared_ptr<QuantLib::StrikedTypePayoff>& payoff,
            const boost::shared_ptr<QuantLib::Exercise>& exercise,
            const boost::shared_ptr<QuantLib::PricingEngine>& pricingEngine,
            bool permanent);
    };

    class DiscreteAveragingAsianOption : public OneAssetOption {
      public:
        DiscreteAveragingAsianOption(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            QuantLib::Average::Type averageType,
            QuantLib::Real runningAccumulator,
            QuantLib::Size pastFixings,
            const std::vector<QuantLib::Date>& fixingDates,
            const boost::shared_ptr<QuantLib::GeneralizedBlackScholesProcess >& blackScholesProcess,
            const boost::shared_ptr<QuantLib::StrikedTypePayoff>& payoff,
            const boost::shared_ptr<QuantLib::Exercise>& exercise,
            const boost::shared_ptr<QuantLib::PricingEngine>& pricingEngine,
            bool permanent);
    };

}

#endif


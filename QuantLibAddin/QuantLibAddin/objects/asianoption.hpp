
/*
 Copyright (C) 2005 Eric Ehlers

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

#ifndef qla_asianoption_hpp
#define qla_asianoption_hpp

#include <QuantLibAddin/objects/stochasticprocess.hpp>
#include <ql/Instruments/asianoption.hpp>

namespace QuantLibAddin {

    class ContinuousAveragingAsianOption : public ObjHandler::Object {
    public:
        ContinuousAveragingAsianOption(
            boost::shared_ptr<StochasticProcess>,
            const std::string &typeAverage,
            const std::string &typeOption,
            const std::string &typePayoff,
            const float &strike,
            const std::string &typeExercise,
            const long &exerciseDate,
            const long &settlementDate,
            const std::string &typeEngine,
            const long &timeSteps);
        virtual boost::shared_ptr<void> getReference() const {
            return boost::static_pointer_cast<void>(continuousAveragingAsianOption_);
        }
    private:
        boost::shared_ptr<QuantLib::ContinuousAveragingAsianOption> continuousAveragingAsianOption_;
    };

/*  this class expects a vector which QLA doesn't yet support
    class DiscreteAveragingAsianOption : public ObjHandler::Object {
    public:
        DiscreteAveragingAsianOption(
            boost::shared_ptr<StochasticProcess>,
            const std::string &typeAverage,
            const float runningAccumulator,
            const long pastFixings,
            const std::vector<QuantLib::Date> fixingDates,
            const std::string &typeOption,
            const std::string &typePayoff,
            const float &strike,
            const std::string &typeExercise,
            const long &exerciseDate,
            const long &settlementDate,
            const std::string &typeEngine,
            const long &timeSteps);
        virtual boost::shared_ptr<void> getReference() const {
            return boost::static_pointer_cast<void>(discreteAveragingAsianOption_);
        }
    private:
        boost::shared_ptr<QuantLib::DiscreteAveragingAsianOption> discreteAveragingAsianOption_;
    };
*/
}

#endif

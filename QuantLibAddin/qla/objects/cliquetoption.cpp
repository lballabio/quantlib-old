
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

#include <qla/objects/cliquetoption.hpp>
#include <qla/objects/optionutils.hpp>

namespace QuantLibAddin {

    CliquetOption::CliquetOption(
            const boost::shared_ptr<StochasticProcess> &stochasticProcess,
            const std::vector < long > &resetDatesLong,
            const std::string &optionTypeID,
            const std::string &payoffID,
            const float &strike,
            const std::string &exerciseID,
            const long &exerciseDate,
            const long &settlementDate,
            const std::string &engineID,
            const long &timeSteps) {
        boost::shared_ptr<QuantLib::PercentageStrikePayoff> payoff =
            boost::dynamic_pointer_cast<QuantLib::PercentageStrikePayoff>
            (IDtoPayoff(optionTypeID, payoffID, strike));
        if (!payoff)
            QL_FAIL("CliquetOption: unrecognized payoffID: " + payoffID);
        boost::shared_ptr<QuantLib::EuropeanExercise> exercise = 
            boost::dynamic_pointer_cast<QuantLib::EuropeanExercise>
            (IDtoExercise(exerciseID, QuantLib::Date(exerciseDate),
                QuantLib::Date(settlementDate)));
        if (!exercise)
            QL_FAIL("CliquetOption: unrecognized exerciseID: " + exerciseID);
        boost::shared_ptr<QuantLib::PricingEngine> pricingEngine =
            IDtoEngine(engineID, timeSteps);
        const boost::shared_ptr<QuantLib::BlackScholesProcess> stochasticProcessQL = 
            boost::static_pointer_cast<QuantLib::BlackScholesProcess>
            (stochasticProcess->getReference());
        std::vector<QuantLib::Date> resetDates =
            longVectorToDateVector(resetDatesLong);
        cliquetOption_ = boost::shared_ptr<QuantLib::CliquetOption>(
            new QuantLib::CliquetOption(
                stochasticProcessQL, 
                payoff, 
                exercise, 
                resetDates,
                pricingEngine));
        ObjHandler::any_ptr any_npv(new boost::any(cliquetOption_->NPV()));
        ObjHandler::any_ptr any_engine(new boost::any(std::string(engineID)));
        ObjHandler::ObjectProperty prop_npv(FIELD_NPV, any_npv);
        ObjHandler::ObjectProperty prop_engine(FIELD_ENGINE, any_engine);
        properties_.push_back(prop_npv);
        properties_.push_back(prop_engine);
    }

    void CliquetOption::setEngine(
            const std::string &engineID,
            const long &timeSteps) {
        boost::shared_ptr<QuantLib::PricingEngine> pricingEngine =
            IDtoEngine(engineID, timeSteps);
        cliquetOption_->setPricingEngine(pricingEngine);
        *properties_[IDX_NPV]() = cliquetOption_->NPV();
        *properties_[IDX_ENGINE]() = engineID;
    }

}



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

#if defined(HAVE_CONFIG_H)     // Dynamically created by configure
    #include <qla/config.hpp>
#endif
#include <qla/dividendvanillaoption.hpp>
#include <qla/optionutils.hpp>

namespace QuantLibAddin {

    DividendVanillaOption::DividendVanillaOption(ObjHandler::ArgumentStack &arguments) {
        long timeSteps                  = OH_POP_ARGUMENT(long, arguments);
        std::string engineID            = OH_POP_ARGUMENT(std::string, arguments);
        long settlementDate             = OH_POP_ARGUMENT(long, arguments);
        long exerciseDate               = OH_POP_ARGUMENT(long, arguments);
        std::string exerciseID          = OH_POP_ARGUMENT(std::string, arguments);
        double strike                   = OH_POP_ARGUMENT(double, arguments);
        std::string payoffID            = OH_POP_ARGUMENT(std::string, arguments);
        std::string optionTypeID        = OH_POP_ARGUMENT(std::string, arguments);
        std::vector < double > dividends 
            = OH_POP_ARGUMENT(std::vector < double >, arguments);
        std::vector < long > dividendDates 
            = OH_POP_ARGUMENT(std::vector < long >, arguments);
        std::string handleBlackScholes  = OH_POP_ARGUMENT(std::string, arguments);

        boost::shared_ptr<BlackScholesProcess> blackScholesProcess =
            OH_GET_OBJECT(BlackScholesProcess, handleBlackScholes);
        if (!blackScholesProcess)
            QL_FAIL("DividendVanillaOption: error retrieving object " + handleBlackScholes);
        const boost::shared_ptr<QuantLib::BlackScholesProcess> blackScholesProcessQL = 
            OH_GET_REFERENCE(QuantLib::BlackScholesProcess, blackScholesProcess);

        boost::shared_ptr<QuantLib::StrikedTypePayoff> payoff =
            IDtoPayoff(optionTypeID, payoffID, strike);
        boost::shared_ptr<QuantLib::Exercise> exercise = 
            IDtoExercise(exerciseID, exerciseDate, settlementDate);
        boost::shared_ptr<QuantLib::PricingEngine> pricingEngine =
            IDtoEngine(engineID, timeSteps);
        const std::vector<QuantLib::Date> dividendDatesQL = 
            longVectorToDateVector(dividendDates);
        dividendVanillaOption_ = boost::shared_ptr<QuantLib::DividendVanillaOption>(
            new QuantLib::DividendVanillaOption(
                blackScholesProcessQL, 
                payoff, 
                exercise, 
                dividendDatesQL,
                dividends,
                pricingEngine));
        ObjHandler::any_ptr any_npv(new boost::any(dividendVanillaOption_->NPV()));
        ObjHandler::any_ptr any_engine(new boost::any(std::string(engineID)));
        ObjHandler::ObjectProperty prop_npv(FIELD_NPV, any_npv);
        ObjHandler::ObjectProperty prop_engine(FIELD_ENGINE, any_engine);
        properties_.push_back(prop_npv);
        properties_.push_back(prop_engine);
    }

    void DividendVanillaOption::setEngine(
            const std::string &engineID,
            const long &timeSteps) {
        boost::shared_ptr<QuantLib::PricingEngine> pricingEngine =
            IDtoEngine(engineID, timeSteps);
        dividendVanillaOption_->setPricingEngine(pricingEngine);
        *properties_[IDX_NPV]() = dividendVanillaOption_->NPV();
        *properties_[IDX_ENGINE]() = engineID;
    }

}


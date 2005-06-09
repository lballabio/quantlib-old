
/*
 Copyright (C) 2005 Plamen Neykov
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
#include <qla/cliquetoption.hpp>
#include <qla/optionutils.hpp>
#include <qla/enumfactory.hpp>

namespace QuantLibAddin {

    CliquetOption::CliquetOption(ObjHandler::ArgumentStack &arguments) {
        long timeSteps                  = OH_POP_ARGUMENT(long, arguments);
        std::string engineID            = OH_POP_ARGUMENT(std::string, arguments);
        long exerciseDate               = OH_POP_ARGUMENT(long, arguments);
        double strike                   = OH_POP_ARGUMENT(double, arguments);
        std::string optionTypeID        = OH_POP_ARGUMENT(std::string, arguments);
        std::vector < long > resetDates 
            = OH_POP_ARGUMENT(std::vector < long >, arguments);
        std::string handleBlackScholes  = OH_POP_ARGUMENT(std::string, arguments);

        boost::shared_ptr<BlackScholesProcess> blackScholesProcess =
            OH_GET_OBJECT(BlackScholesProcess, handleBlackScholes);
        if (!blackScholesProcess)
            QL_FAIL("CliquetOption: error retrieving object " + handleBlackScholes);
        const boost::shared_ptr<QuantLib::BlackScholesProcess> blackScholesProcessQL = 
            OH_GET_REFERENCE(QuantLib::BlackScholesProcess, blackScholesProcess);

		QuantLib::Option::Type type = 
			CreateEnum<QuantLib::Option::Type>::create(optionTypeID);

        boost::shared_ptr<QuantLib::PercentageStrikePayoff> payoff(
            new QuantLib::PercentageStrikePayoff(type, strike));
        boost::shared_ptr<QuantLib::EuropeanExercise> exercise(
            new QuantLib::EuropeanExercise(QuantLib::Date(exerciseDate)));
        boost::shared_ptr<QuantLib::PricingEngine> pricingEngine =
            IDtoEngine(engineID, timeSteps);
        std::vector<QuantLib::Date> resetDatesQL =
            longVectorToDateVector(resetDates);
        cliquetOption_ = boost::shared_ptr<QuantLib::CliquetOption>(
            new QuantLib::CliquetOption(
                blackScholesProcessQL, 
                payoff, 
                exercise, 
                resetDatesQL,
                pricingEngine));
        ObjHandler::any_ptr any_npv(new boost::any(cliquetOption_->NPV()));
        ObjHandler::any_ptr any_engine(new boost::any(std::string(engineID)));
        ObjHandler::ObjectProperty prop_npv(FIELD_NPV, any_npv);
        ObjHandler::ObjectProperty prop_engine(FIELD_ENGINE, any_engine);
        properties_.push_back(prop_npv);
        properties_.push_back(prop_engine);
    }

}




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
#include <qla/asianoption.hpp>
#include <qla/optionutils.hpp>

namespace QuantLibAddin {

    QuantLib::Average::Type IDtoAverageType(const std::string &averageID) {
        std::string idUpper = QuantLib::StringFormatter::toUppercase(averageID);
        if (idUpper.compare("A") ==0)
            return QuantLib::Average::Arithmetic;
        else if (idUpper.compare("G") == 0)
            return QuantLib::Average::Geometric;
        else
            QL_FAIL("IDtoAverageType: unrecognized averageID: " + averageID);
    }

    ContinuousAveragingAsianOption::ContinuousAveragingAsianOption(ObjHandler::ArgStack &args) {
        long timeSteps                  = ObjHandler::Args<long>::popArg(args);
        std::string engineID            = ObjHandler::Args<std::string>::popArg(args);
        long settlementDate             = ObjHandler::Args<long>::popArg(args);
        long exerciseDate               = ObjHandler::Args<long>::popArg(args);
        std::string exerciseID          = ObjHandler::Args<std::string>::popArg(args);
        double strike                   = ObjHandler::Args<double>::popArg(args);
        std::string payoffID            = ObjHandler::Args<std::string>::popArg(args);
        std::string optionTypeID        = ObjHandler::Args<std::string>::popArg(args);
        std::string averageID           = ObjHandler::Args<std::string>::popArg(args);
        std::string handleBlackScholes  = ObjHandler::Args<std::string>::popArg(args);

        boost::shared_ptr<BlackScholesProcess> blackScholesProcess =
            boost::dynamic_pointer_cast<BlackScholesProcess>
            (QL_OBJECT_GET(handleBlackScholes));
        if (!blackScholesProcess)
            QL_FAIL("ContinuousAveragingAsianOption: error retrieving object " + handleBlackScholes);
        const boost::shared_ptr<QuantLib::BlackScholesProcess> blackScholesProcessQL = 
            boost::static_pointer_cast<QuantLib::BlackScholesProcess>
            (blackScholesProcess->getReference());

        QuantLib::Average::Type averageType =
            IDtoAverageType(averageID);
        boost::shared_ptr<QuantLib::StrikedTypePayoff> payoff =
            IDtoPayoff(optionTypeID, payoffID, strike);
        boost::shared_ptr<QuantLib::Exercise> exercise = 
            IDtoExercise(exerciseID, exerciseDate, settlementDate);
        boost::shared_ptr<QuantLib::PricingEngine> pricingEngine =
            IDtoEngine(engineID, timeSteps);
        continuousAveragingAsianOption_ = 
            boost::shared_ptr<QuantLib::ContinuousAveragingAsianOption>(
                new QuantLib::ContinuousAveragingAsianOption(
                    averageType,
                    blackScholesProcessQL, 
                    payoff, 
                    exercise, 
                    pricingEngine));
        ObjHandler::any_ptr any_npv(
            new boost::any(continuousAveragingAsianOption_->NPV()));
        ObjHandler::any_ptr any_engine(
            new boost::any(std::string(engineID)));
        ObjHandler::ObjectProperty prop_npv(FIELD_NPV, any_npv);
        ObjHandler::ObjectProperty prop_engine(FIELD_ENGINE, any_engine);
        properties_.push_back(prop_npv);
        properties_.push_back(prop_engine);
    }

    DiscreteAveragingAsianOption::DiscreteAveragingAsianOption(ObjHandler::ArgStack &args) {
        long timeSteps                  = ObjHandler::Args<long>::popArg(args);
        std::string engineID            = ObjHandler::Args<std::string>::popArg(args);
        long settlementDate             = ObjHandler::Args<long>::popArg(args);
        long exerciseDate               = ObjHandler::Args<long>::popArg(args);
        std::string exerciseID          = ObjHandler::Args<std::string>::popArg(args);
        double strike                   = ObjHandler::Args<double>::popArg(args);
        std::string payoffID            = ObjHandler::Args<std::string>::popArg(args);
        std::string optionTypeID        = ObjHandler::Args<std::string>::popArg(args);
        std::vector < long > fixingDates 
            = ObjHandler::Args< std::vector < long > >::popArg(args);
        long pastFixings = ObjHandler::Args<long>::popArg(args);
        double runningAccumulator       = ObjHandler::Args<double>::popArg(args);
        std::string averageID           = ObjHandler::Args<std::string>::popArg(args);
        std::string handleBlackScholes  = ObjHandler::Args<std::string>::popArg(args);

        boost::shared_ptr<BlackScholesProcess> blackScholesProcess =
            boost::dynamic_pointer_cast<BlackScholesProcess>
            (QL_OBJECT_GET(handleBlackScholes));
        if (!blackScholesProcess)
            QL_FAIL("DiscreteAveragingAsianOption: error retrieving object " + handleBlackScholes);
        const boost::shared_ptr<QuantLib::BlackScholesProcess> blackScholesProcessQL = 
            boost::static_pointer_cast<QuantLib::BlackScholesProcess>
            (blackScholesProcess->getReference());

        QuantLib::Average::Type averageType =
            IDtoAverageType(averageID);
        boost::shared_ptr<QuantLib::StrikedTypePayoff> payoff =
            IDtoPayoff(optionTypeID, payoffID, strike);
        boost::shared_ptr<QuantLib::Exercise> exercise = 
            IDtoExercise(exerciseID, exerciseDate, settlementDate);
        boost::shared_ptr<QuantLib::PricingEngine> pricingEngine =
            IDtoEngine(engineID, timeSteps);
        std::vector<QuantLib::Date> fixingDatesQL;
        std::vector<long>::const_iterator i;
        for (i = fixingDates.begin(); i != fixingDates.end(); i++)
            fixingDatesQL.push_back(QuantLib::Date(*i));
        discreteAveragingAsianOption_ = 
            boost::shared_ptr<QuantLib::DiscreteAveragingAsianOption>(
                new QuantLib::DiscreteAveragingAsianOption(
                    averageType,
                    runningAccumulator,
                    pastFixings,
                    fixingDatesQL,
                    blackScholesProcessQL, 
                    payoff, 
                    exercise, 
                    pricingEngine));
        ObjHandler::any_ptr any_npv(
            new boost::any(discreteAveragingAsianOption_->NPV()));
        ObjHandler::any_ptr any_engine(
            new boost::any(std::string(engineID)));
        ObjHandler::ObjectProperty prop_npv(FIELD_NPV, any_npv);
        ObjHandler::ObjectProperty prop_engine(FIELD_ENGINE, any_engine);
        properties_.push_back(prop_npv);
        properties_.push_back(prop_engine);
    }

}



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

    ContinuousAveragingAsianOption::ContinuousAveragingAsianOption(va_list list) {
        char *handleStochastic = va_arg(list, char *);
        char *averageID = va_arg(list, char *);
        char *optionTypeID = va_arg(list, char *);
        char *payoffID = va_arg(list, char *);
        double strike = va_arg(list, double);
        char *exerciseID = va_arg(list, char *);
        long exerciseDate = va_arg(list, long);
        long settlementDate = va_arg(list, long);
        char *engineID = va_arg(list, char *);
        long timeSteps = va_arg(list, long);

        std::string handleStochasticStr(handleStochastic);
        boost::shared_ptr<StochasticProcess> stochasticProcess =
            boost::dynamic_pointer_cast<StochasticProcess>
            (ObjHandler::ObjectHandler::instance().retrieveObject(handleStochasticStr));
        if (!stochasticProcess)
            QL_FAIL("ContinuousAveragingAsianOption: error retrieving object " + handleStochasticStr);

        QuantLib::Average::Type averageType =
            IDtoAverageType(averageID);
        boost::shared_ptr<QuantLib::StrikedTypePayoff> payoff =
            IDtoPayoff(optionTypeID, payoffID, strike);
        boost::shared_ptr<QuantLib::Exercise> exercise = 
            IDtoExercise(exerciseID, exerciseDate, settlementDate);
        boost::shared_ptr<QuantLib::PricingEngine> pricingEngine =
            IDtoEngine(engineID, timeSteps);
        const boost::shared_ptr<QuantLib::BlackScholesProcess> stochasticProcessQL = 
            boost::static_pointer_cast<QuantLib::BlackScholesProcess>
            (stochasticProcess->getReference());
        continuousAveragingAsianOption_ = 
            boost::shared_ptr<QuantLib::ContinuousAveragingAsianOption>(
                new QuantLib::ContinuousAveragingAsianOption(
                    averageType,
                    stochasticProcessQL, 
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

    DiscreteAveragingAsianOption::DiscreteAveragingAsianOption(va_list list) {
        char *handleStochastic = va_arg(list, char *);
        char *averageID = va_arg(list, char *);
        double runningAccumulator = va_arg(list, double);
        long pastFixings = va_arg(list, long);
        long fixingDatesSize = va_arg(list, long);
        long *fixingDates = va_arg(list, long *);
        char *optionTypeID = va_arg(list, char *);
        char *payoffID = va_arg(list, char *);
        double strike = va_arg(list, double);
        char *exerciseID = va_arg(list, char *);
        long exerciseDate = va_arg(list, long);
        long settlementDate = va_arg(list, long);
        char *engineID = va_arg(list, char *);
        long timeSteps = va_arg(list, long);

        std::string handleStochasticStr(handleStochastic);
        boost::shared_ptr<StochasticProcess> stochasticProcess =
            boost::dynamic_pointer_cast<StochasticProcess>
            (ObjHandler::ObjectHandler::instance().retrieveObject(handleStochasticStr));
        if (!stochasticProcess)
            QL_FAIL("DiscreteAveragingAsianOption: error retrieving object " + handleStochasticStr);

        std::vector <long> fixingDatesVector =
            Conversion<long>::arrayToVector(fixingDatesSize, fixingDates);
        QuantLib::Average::Type averageType =
            IDtoAverageType(averageID);
        boost::shared_ptr<QuantLib::StrikedTypePayoff> payoff =
            IDtoPayoff(optionTypeID, payoffID, strike);
        boost::shared_ptr<QuantLib::Exercise> exercise = 
            IDtoExercise(exerciseID, exerciseDate, settlementDate);
        boost::shared_ptr<QuantLib::PricingEngine> pricingEngine =
            IDtoEngine(engineID, timeSteps);
        const boost::shared_ptr<QuantLib::BlackScholesProcess> stochasticProcessQL = 
            boost::static_pointer_cast<QuantLib::BlackScholesProcess>
            (stochasticProcess->getReference());
        std::vector<QuantLib::Date> fixingDatesQL;
        std::vector<long>::const_iterator i;
        for (i = fixingDatesVector.begin(); i != fixingDatesVector.end(); i++)
            fixingDatesQL.push_back(QuantLib::Date(*i));
        discreteAveragingAsianOption_ = 
            boost::shared_ptr<QuantLib::DiscreteAveragingAsianOption>(
                new QuantLib::DiscreteAveragingAsianOption(
                    averageType,
                    runningAccumulator,
                    pastFixings,
                    fixingDatesQL,
                    stochasticProcessQL, 
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



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
#include <qla/basketoption.hpp>
#include <qla/optionutils.hpp>

namespace QuantLibAddin {

    QuantLib::BasketOption::BasketType IDtoBasketType(const std::string &basketID) {
        std::string idUpper = QuantLib::StringFormatter::toUppercase(basketID);
        if (idUpper.compare("MIN") ==0)
            return QuantLib::BasketOption::Min;
        else if (idUpper.compare("MAX") == 0)
            return QuantLib::BasketOption::Max;
        else
            QL_FAIL("IDtoBasketType: unrecognized typeID: " + basketID);
    }

    BasketOption::BasketOption(va_list list) {
        long handleStochasticSize = va_arg(list, long);
        char **handleStochastic = va_arg(list, char **);
        char *basketID = va_arg(list, char *);
        long correlationsRows = va_arg(list, long);
        long correlationsCols = va_arg(list, long);
        double **correlations = va_arg(list, double **);
        char *optionTypeID = va_arg(list, char *);
        double strike = va_arg(list, double);
        char *exerciseID = va_arg(list, char *);
        long exerciseDate = va_arg(list, long);
        long settlementDate = va_arg(list, long);
        char *engineID = va_arg(list, char *);
        long timeSteps = va_arg(list, long);

        std::vector <std::string> handleStochasticVector =
            Conversion<std::string>::arrayToVector(handleStochasticSize, handleStochastic);

        std::vector < std::vector <double> >correlationsMatrix =
            Conversion<double>::arrayToMatrix(correlationsRows, correlationsCols, correlations);
        QuantLib::BasketOption::BasketType basketType = 
            IDtoBasketType(basketID);
        QuantLib::Matrix correlation =
            vectorVectorToMatrix(correlationsMatrix);
        QuantLib::Option::Type type = IDtoOptionType(optionTypeID);
        boost::shared_ptr<QuantLib::PlainVanillaPayoff> payoff(
            new QuantLib::PlainVanillaPayoff(type, strike));
        boost::shared_ptr<QuantLib::Exercise> exercise = 
            IDtoExercise(exerciseID, exerciseDate, settlementDate);
        boost::shared_ptr<QuantLib::PricingEngine> pricingEngine =
            IDtoEngine(engineID, timeSteps);
        std::vector < boost::shared_ptr<QuantLib::BlackScholesProcess> >
            stochasticProcsQL;
        std::vector < std::string >::const_iterator i;
        for (i = handleStochasticVector.begin(); i != handleStochasticVector.end(); i++) {
            std::string handleStochasticStr = *i;
            boost::shared_ptr<StochasticProcess> stochasticProcess =
                boost::dynamic_pointer_cast<StochasticProcess>
                (ObjHandler::ObjectHandler::instance().retrieveObject(handleStochasticStr));
            if (!stochasticProcess)
                QL_FAIL("BasketOption: error retrieving object " + handleStochasticStr);
            const boost::shared_ptr<QuantLib::BlackScholesProcess> stochasticProcessQL =
                boost::static_pointer_cast<QuantLib::BlackScholesProcess>
                (stochasticProcess->getReference());
            stochasticProcsQL.push_back(stochasticProcessQL);
        }
        barrierOption_ = boost::shared_ptr<QuantLib::BasketOption>(
            new QuantLib::BasketOption(
                basketType,
                stochasticProcsQL, 
                payoff, 
                exercise, 
                correlation,
                pricingEngine));
        ObjHandler::any_ptr any_npv(new boost::any(barrierOption_->NPV()));
        ObjHandler::any_ptr any_engine(new boost::any(std::string(engineID)));
        ObjHandler::ObjectProperty prop_npv(FIELD_NPV, any_npv);
        ObjHandler::ObjectProperty prop_engine(FIELD_ENGINE, any_engine);
        properties_.push_back(prop_npv);
        properties_.push_back(prop_engine);
    }

}


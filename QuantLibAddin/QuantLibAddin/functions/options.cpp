
/*
 Copyright (C) 2004, 2005 Eric Ehlers

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

#include <QuantLibAddin/functions/options.hpp>
#include <QuantLibAddin/objects/vanillaoption.hpp>
#include <QuantLibAddin/objects/asianoption.hpp>
#include <QuantLibAddin/objects/barrieroption.hpp>

namespace QuantLibAddin {

    const ObjHandler::Properties& QL_STOCHASTIC_PROCESS(
            const std::string &handleStochastic,
            const double &underlying,
            const std::string &dayCounterID,
            const long &settlementDate,
            const double &riskFreeRate,
            const double &dividendYield,
            const double &volatility) {
        ObjHandler::obj_ptr stochasticProcess(
            new StochasticProcess(
                underlying,
                dayCounterID,
                QuantLib::Date(settlementDate),
                riskFreeRate,
                dividendYield,
                volatility));
        ObjHandler::ObjectHandler::instance().storeObject(
            handleStochastic, stochasticProcess);
        return stochasticProcess->getProperties();
    }

  const ObjHandler::Properties& QL_OPTION_VANILLA(
            const std::string &handleOption, 
            const std::string &handleStochastic, 
            const std::string &typeOption,
            const std::string &typePayoff,
            const double &strike,
            const std::string &typeExercise,
            const long &exerciseDate,
            const long &settlementDate,
            const std::string &typeEngine,
            const long &timeSteps) {
        boost::shared_ptr<StochasticProcess> stochasticProcess = 
            boost::dynamic_pointer_cast<StochasticProcess>
            (ObjHandler::ObjectHandler::instance().retrieveObject(handleStochastic));
        if (!stochasticProcess)
            QL_FAIL("QL_OPTION_VANILLA: error retrieving object " + handleStochastic);
        ObjHandler::obj_ptr vanillaOption(
            new VanillaOption(
                stochasticProcess,
                typeOption,
                typePayoff,
                strike,
                typeExercise,
                QuantLib::Date(exerciseDate),
                QuantLib::Date(settlementDate),
                typeEngine,
                timeSteps));
        ObjHandler::ObjectHandler::instance().storeObject(
            handleOption, vanillaOption);
        return vanillaOption->getProperties();
    }

  const ObjHandler::Properties& QL_OPTION_ASIAN_C(
            const std::string &handleOption, 
            const std::string &handleStochastic, 
            const std::string &typeAverage,
            const std::string &typeOption,
            const std::string &typePayoff,
            const double &strike,
            const std::string &typeExercise,
            const long &exerciseDate,
            const long &settlementDate,
            const std::string &typeEngine,
            const long &timeSteps) {
        boost::shared_ptr<StochasticProcess> stochasticProcess = 
            boost::dynamic_pointer_cast<StochasticProcess>
            (ObjHandler::ObjectHandler::instance().retrieveObject(handleStochastic));
        if (!stochasticProcess)
            QL_FAIL("QL_OPTION_ASIAN_C: error retrieving object " + handleStochastic);
        ObjHandler::obj_ptr asianOption(
            new ContinuousAveragingAsianOption(
                stochasticProcess,
                typeAverage,
                typeOption,
                typePayoff,
                strike,
                typeExercise,
                QuantLib::Date(exerciseDate),
                QuantLib::Date(settlementDate),
                typeEngine,
                timeSteps));
        ObjHandler::ObjectHandler::instance().storeObject(
            handleOption, asianOption);
        return asianOption->getProperties();
    }

  const ObjHandler::Properties& QL_OPTION_BARRIER(
            const std::string &handleOption, 
            const std::string &handleStochastic, 
            const std::string &typeBarrier,
            const double &barrier,
            const double &rebate,
            const std::string &typeOption,
            const std::string &typePayoff,
            const double &strike,
            const std::string &typeExercise,
            const long &exerciseDate,
            const long &settlementDate,
            const std::string &typeEngine,
            const long &timeSteps) {
        boost::shared_ptr<StochasticProcess> stochasticProcess = 
            boost::dynamic_pointer_cast<StochasticProcess>
            (ObjHandler::ObjectHandler::instance().retrieveObject(handleStochastic));
        if (!stochasticProcess)
            QL_FAIL("QL_OPTION_BARRIER: error retrieving object " + handleStochastic);
        ObjHandler::obj_ptr barrierOption(
            new BarrierOption(
                stochasticProcess,
                typeBarrier,
                barrier,
                rebate,
                typeOption,
                typePayoff,
                strike,
                typeExercise,
                QuantLib::Date(exerciseDate),
                QuantLib::Date(settlementDate),
                typeEngine,
                timeSteps));
        ObjHandler::ObjectHandler::instance().storeObject(
            handleOption, barrierOption);
        return barrierOption->getProperties();
    }
    const ObjHandler::Properties& QL_OPTION_SETENGINE(
        const std::string &handleOption, 
        const std::string &engineName,
        const long &timeSteps) {
        boost::shared_ptr<VanillaOption> vanillaOption = 
            boost::dynamic_pointer_cast<VanillaOption>
            (ObjHandler::ObjectHandler::instance().retrieveObject(handleOption));
        if (!vanillaOption)
            QL_FAIL("QL_OPTION_SETENGINE: error retrieving object " + handleOption);
        vanillaOption->setEngine(engineName, timeSteps);
        return vanillaOption->getProperties();
    }

}


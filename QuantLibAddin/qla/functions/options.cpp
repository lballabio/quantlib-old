
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

// this file generated automatically by autogen.py
// editing this file manually is not recommended

#include <qla/functions/options.hpp>
#include <qla/functions/conversions.hpp>
#include <qla/objects/options.hpp>

namespace QuantLibAddin {

    const ObjHandler::Properties& QL_OPTION_ASIAN_C(
            const std::string &handleObject,
            const std::string &handleStochastic,
            const std::string &average,
            const std::string &optionType,
            const std::string &payoff,
            const double &strike,
            const std::string &exercise,
            const long &exerciseDate,
            const long &settlementDate,
            const std::string &engine,
            const long &timeSteps) {

        boost::shared_ptr<StochasticProcess> handleStochasticProcess =
            boost::dynamic_pointer_cast<StochasticProcess>
            (ObjHandler::ObjectHandler::instance().retrieveObject(handleStochastic));
        if (!handleStochasticProcess)
            QL_FAIL("QL_OPTION_ASIAN_C: error retrieving object " + handleStochastic);

        ObjHandler::obj_ptr objectPointer(
            new ContinuousAveragingAsianOption(
                handleStochasticProcess,
                average,
                optionType,
                payoff,
                strike,
                exercise,
                exerciseDate,
                settlementDate,
                engine,
                timeSteps));
        ObjHandler::ObjectHandler::instance().storeObject(
            handleObject, objectPointer);
        return objectPointer->getProperties();

    }

    const ObjHandler::Properties& QL_OPTION_ASIAN_D(
            const std::string &handleObject,
            const std::string &handleStochastic,
            const std::string &average,
            const double &runningAccumulator,
            const long &pastFixings,
            const std::vector< long  >&fixingDates,
            const std::string &optionType,
            const std::string &payoff,
            const double &strike,
            const std::string &exercise,
            const long &exerciseDate,
            const long &settlementDate,
            const std::string &engine,
            const long &timeSteps) {

        boost::shared_ptr<StochasticProcess> handleStochasticProcess =
            boost::dynamic_pointer_cast<StochasticProcess>
            (ObjHandler::ObjectHandler::instance().retrieveObject(handleStochastic));
        if (!handleStochasticProcess)
            QL_FAIL("QL_OPTION_ASIAN_D: error retrieving object " + handleStochastic);

        ObjHandler::obj_ptr objectPointer(
            new DiscreteAveragingAsianOption(
                handleStochasticProcess,
                average,
                runningAccumulator,
                pastFixings,
                fixingDates,
                optionType,
                payoff,
                strike,
                exercise,
                exerciseDate,
                settlementDate,
                engine,
                timeSteps));
        ObjHandler::ObjectHandler::instance().storeObject(
            handleObject, objectPointer);
        return objectPointer->getProperties();

    }

    const ObjHandler::Properties& QL_OPTION_BARRIER(
            const std::string &handleObject,
            const std::string &handleStochastic,
            const std::string &typeBarrier,
            const double &barrier,
            const double &rebate,
            const std::string &optionType,
            const std::string &payoff,
            const double &strike,
            const std::string &exercise,
            const long &exerciseDate,
            const long &settlementDate,
            const std::string &engine,
            const long &timeSteps) {

        boost::shared_ptr<StochasticProcess> handleStochasticProcess =
            boost::dynamic_pointer_cast<StochasticProcess>
            (ObjHandler::ObjectHandler::instance().retrieveObject(handleStochastic));
        if (!handleStochasticProcess)
            QL_FAIL("QL_OPTION_BARRIER: error retrieving object " + handleStochastic);

        ObjHandler::obj_ptr objectPointer(
            new BarrierOption(
                handleStochasticProcess,
                typeBarrier,
                barrier,
                rebate,
                optionType,
                payoff,
                strike,
                exercise,
                exerciseDate,
                settlementDate,
                engine,
                timeSteps));
        ObjHandler::ObjectHandler::instance().storeObject(
            handleObject, objectPointer);
        return objectPointer->getProperties();

    }

    const ObjHandler::Properties& QL_OPTION_BASKET(
            const std::string &handleObject,
            const std::vector< std::string  >&handleStochastic,
            const std::string &basket,
            const std::vector < std::vector < double  > >&correlations,
            const std::string &optionType,
            const double &strike,
            const std::string &exercise,
            const long &exerciseDate,
            const long &settlementDate,
            const std::string &engine,
            const long &timeSteps) {

        std::vector < boost::shared_ptr< StochasticProcess > >
            handleStochasticProcess;
            handleVectorToObjectVector(
                handleStochastic,
                handleStochasticProcess);

        ObjHandler::obj_ptr objectPointer(
            new BasketOption(
                handleStochasticProcess,
                basket,
                correlations,
                optionType,
                strike,
                exercise,
                exerciseDate,
                settlementDate,
                engine,
                timeSteps));
        ObjHandler::ObjectHandler::instance().storeObject(
            handleObject, objectPointer);
        return objectPointer->getProperties();

    }

    const ObjHandler::Properties& QL_OPTION_CLIQUET(
            const std::string &handleObject,
            const std::string &handleStochastic,
            const std::vector< long  >&resetDates,
            const std::string &optionType,
            const double &strike,
            const long &exerciseDate,
            const std::string &engine,
            const long &timeSteps) {

        boost::shared_ptr<StochasticProcess> handleStochasticProcess =
            boost::dynamic_pointer_cast<StochasticProcess>
            (ObjHandler::ObjectHandler::instance().retrieveObject(handleStochastic));
        if (!handleStochasticProcess)
            QL_FAIL("QL_OPTION_CLIQUET: error retrieving object " + handleStochastic);

        ObjHandler::obj_ptr objectPointer(
            new CliquetOption(
                handleStochasticProcess,
                resetDates,
                optionType,
                strike,
                exerciseDate,
                engine,
                timeSteps));
        ObjHandler::ObjectHandler::instance().storeObject(
            handleObject, objectPointer);
        return objectPointer->getProperties();

    }

    const ObjHandler::Properties& QL_OPTION_DIVIDENDVANILLA(
            const std::string &handleObject,
            const std::string &handleStochastic,
            const std::vector< long  >&dividendDates,
            const std::vector< double  >&dividends,
            const std::string &optionType,
            const std::string &payoff,
            const double &strike,
            const std::string &exercise,
            const long &exerciseDate,
            const long &settlementDate,
            const std::string &engine,
            const long &timeSteps) {

        boost::shared_ptr<StochasticProcess> handleStochasticProcess =
            boost::dynamic_pointer_cast<StochasticProcess>
            (ObjHandler::ObjectHandler::instance().retrieveObject(handleStochastic));
        if (!handleStochasticProcess)
            QL_FAIL("QL_OPTION_DIVIDENDVANILLA: error retrieving object " + handleStochastic);

        ObjHandler::obj_ptr objectPointer(
            new DividendVanillaOption(
                handleStochasticProcess,
                dividendDates,
                dividends,
                optionType,
                payoff,
                strike,
                exercise,
                exerciseDate,
                settlementDate,
                engine,
                timeSteps));
        ObjHandler::ObjectHandler::instance().storeObject(
            handleObject, objectPointer);
        return objectPointer->getProperties();

    }

    const ObjHandler::Properties& QL_OPTION_FORWARDVANILLA(
            const std::string &handleObject,
            const std::string &handleStochastic,
            const double &moneyness,
            const long &resetDate,
            const std::string &optionType,
            const std::string &payoff,
            const double &strike,
            const std::string &exercise,
            const long &exerciseDate,
            const long &settlementDate,
            const std::string &engine,
            const long &timeSteps) {

        boost::shared_ptr<StochasticProcess> handleStochasticProcess =
            boost::dynamic_pointer_cast<StochasticProcess>
            (ObjHandler::ObjectHandler::instance().retrieveObject(handleStochastic));
        if (!handleStochasticProcess)
            QL_FAIL("QL_OPTION_FORWARDVANILLA: error retrieving object " + handleStochastic);

        ObjHandler::obj_ptr objectPointer(
            new ForwardVanillaOption(
                handleStochasticProcess,
                moneyness,
                resetDate,
                optionType,
                payoff,
                strike,
                exercise,
                exerciseDate,
                settlementDate,
                engine,
                timeSteps));
        ObjHandler::ObjectHandler::instance().storeObject(
            handleObject, objectPointer);
        return objectPointer->getProperties();

    }

    const ObjHandler::Properties& QL_OPTION_SETENGINE(
            const std::string &handle,
            const std::string &engineName,
            const long &timeSteps) {

        boost::shared_ptr<VanillaOption> objectPointer =
            boost::dynamic_pointer_cast<VanillaOption>
            (ObjHandler::ObjectHandler::instance().retrieveObject(handle));
        if (!objectPointer)
            QL_FAIL("QL_OPTION_SETENGINE: error retrieving object " + handle);

        objectPointer->setEngine(engineName, timeSteps);
        return objectPointer->getProperties();

    }

    const ObjHandler::Properties& QL_STOCHASTIC_PROCESS(
            const std::string &handleObject,
            const double &underlying,
            const std::string &dayCounter,
            const long &settlementDate,
            const double &riskFreeRate,
            const double &dividendYield,
            const double &volatility) {

        ObjHandler::obj_ptr objectPointer(
            new StochasticProcess(
                underlying,
                dayCounter,
                settlementDate,
                riskFreeRate,
                dividendYield,
                volatility));
        ObjHandler::ObjectHandler::instance().storeObject(
            handleObject, objectPointer);
        return objectPointer->getProperties();

    }

    const ObjHandler::Properties& QL_OPTION_VANILLA(
            const std::string &handleObject,
            const std::string &handleStochastic,
            const std::string &optionType,
            const std::string &payoff,
            const double &strike,
            const std::string &exercise,
            const long &exerciseDate,
            const long &settlementDate,
            const std::string &engine,
            const long &timeSteps) {

        boost::shared_ptr<StochasticProcess> handleStochasticProcess =
            boost::dynamic_pointer_cast<StochasticProcess>
            (ObjHandler::ObjectHandler::instance().retrieveObject(handleStochastic));
        if (!handleStochasticProcess)
            QL_FAIL("QL_OPTION_VANILLA: error retrieving object " + handleStochastic);

        ObjHandler::obj_ptr objectPointer(
            new VanillaOption(
                handleStochasticProcess,
                optionType,
                payoff,
                strike,
                exercise,
                exerciseDate,
                settlementDate,
                engine,
                timeSteps));
        ObjHandler::ObjectHandler::instance().storeObject(
            handleObject, objectPointer);
        return objectPointer->getProperties();

    }

}


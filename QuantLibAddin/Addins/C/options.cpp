
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

#include <qla/qladdin.hpp>
extern "C" {
#include <Addins/C/varies.h>
#include <Addins/C/defines.h>
#include <Addins/C/options.h>
}
#include <Addins/C/varies.hpp>

using std::string;

int QL_OPTION_ASIAN_C(
        const char* handle,
        const char* handleStochastic,
        const char* average,
        const char* optionType,
        const char* payoff,
        const double strike,
        const char* exercise,
        const long exerciseDate,
        const long settlementDate,
        const char* engine,
        const long timeSteps,
        VariesList *result) {
    try {
        ObjHandler::Properties properties = QuantLibAddin::QL_OPTION_ASIAN_C(
            handle,
            handleStochastic,
            average,
            optionType,
            payoff,
            strike,
            exercise,
            exerciseDate,
            settlementDate,
            engine,
            timeSteps);
        propertiesToVaries(properties, result);
        return SUCCESS;
    } catch (const std::exception &e) {
        QuantLibAddin::QL_LOGMESSAGE("QL_OPTION_ASIAN_C Error: "
            + std::string(e.what()));
        result = 0;
        return FAIL;
    }
}

int QL_OPTION_ASIAN_D(
        const char* handle,
        const char* handleStochastic,
        const char* average,
        const double runningAccumulator,
        const long pastFixings,
        const long fixingDatesSize,
        const long * fixingDates,
        const char* optionType,
        const char* payoff,
        const double strike,
        const char* exercise,
        const long exerciseDate,
        const long settlementDate,
        const char* engine,
        const long timeSteps,
        VariesList *result) {
    try {
        std::vector <long> fixingDatesVector;
        arrayToVector(fixingDatesSize, fixingDates, fixingDatesVector);
        ObjHandler::Properties properties = QuantLibAddin::QL_OPTION_ASIAN_D(
            handle,
            handleStochastic,
            average,
            runningAccumulator,
            pastFixings,
            fixingDatesVector,
            optionType,
            payoff,
            strike,
            exercise,
            exerciseDate,
            settlementDate,
            engine,
            timeSteps);
        propertiesToVaries(properties, result);
        return SUCCESS;
    } catch (const std::exception &e) {
        QuantLibAddin::QL_LOGMESSAGE("QL_OPTION_ASIAN_D Error: "
            + std::string(e.what()));
        result = 0;
        return FAIL;
    }
}

int QL_OPTION_BARRIER(
        const char* handle,
        const char* handleStochastic,
        const char* typeBarrier,
        const double barrier,
        const double rebate,
        const char* optionType,
        const char* payoff,
        const double strike,
        const char* exercise,
        const long exerciseDate,
        const long settlementDate,
        const char* engine,
        const long timeSteps,
        VariesList *result) {
    try {
        ObjHandler::Properties properties = QuantLibAddin::QL_OPTION_BARRIER(
            handle,
            handleStochastic,
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
            timeSteps);
        propertiesToVaries(properties, result);
        return SUCCESS;
    } catch (const std::exception &e) {
        QuantLibAddin::QL_LOGMESSAGE("QL_OPTION_BARRIER Error: "
            + std::string(e.what()));
        result = 0;
        return FAIL;
    }
}

int QL_OPTION_BASKET(
        const char* handle,
        const long handleStochasticSize,
        const char* * handleStochastic,
        const char* basket,
        const long correlationsRows,
        const long correlationsCols,
        const double ** correlations,
        const char* optionType,
        const double strike,
        const char* exercise,
        const long exerciseDate,
        const long settlementDate,
        const char* engine,
        const long timeSteps,
        VariesList *result) {
    try {
        std::vector <string> handleStochasticVector;
        arrayToVector(handleStochasticSize, handleStochastic, handleStochasticVector);
        std::vector < std::vector <double> >correlationsMatrix;
        arrayToMatrix(correlationsRows, correlationsCols, correlations, correlationsMatrix);
        ObjHandler::Properties properties = QuantLibAddin::QL_OPTION_BASKET(
            handle,
            handleStochasticVector,
            basket,
            correlationsMatrix,
            optionType,
            strike,
            exercise,
            exerciseDate,
            settlementDate,
            engine,
            timeSteps);
        propertiesToVaries(properties, result);
        return SUCCESS;
    } catch (const std::exception &e) {
        QuantLibAddin::QL_LOGMESSAGE("QL_OPTION_BASKET Error: "
            + std::string(e.what()));
        result = 0;
        return FAIL;
    }
}

int QL_OPTION_CLIQUET(
        const char* handle,
        const char* handleStochastic,
        const long resetDatesSize,
        const long * resetDates,
        const char* optionType,
        const double strike,
        const long exerciseDate,
        const char* engine,
        const long timeSteps,
        VariesList *result) {
    try {
        std::vector <long> resetDatesVector;
        arrayToVector(resetDatesSize, resetDates, resetDatesVector);
        ObjHandler::Properties properties = QuantLibAddin::QL_OPTION_CLIQUET(
            handle,
            handleStochastic,
            resetDatesVector,
            optionType,
            strike,
            exerciseDate,
            engine,
            timeSteps);
        propertiesToVaries(properties, result);
        return SUCCESS;
    } catch (const std::exception &e) {
        QuantLibAddin::QL_LOGMESSAGE("QL_OPTION_CLIQUET Error: "
            + std::string(e.what()));
        result = 0;
        return FAIL;
    }
}

int QL_OPTION_DIVIDENDVANILLA(
        const char* handle,
        const char* handleStochastic,
        const long dividendDatesSize,
        const long * dividendDates,
        const long dividendsSize,
        const double * dividends,
        const char* optionType,
        const char* payoff,
        const double strike,
        const char* exercise,
        const long exerciseDate,
        const long settlementDate,
        const char* engine,
        const long timeSteps,
        VariesList *result) {
    try {
        std::vector <long> dividendDatesVector;
        arrayToVector(dividendDatesSize, dividendDates, dividendDatesVector);
        std::vector <double> dividendsVector;
        arrayToVector(dividendsSize, dividends, dividendsVector);
        ObjHandler::Properties properties = QuantLibAddin::QL_OPTION_DIVIDENDVANILLA(
            handle,
            handleStochastic,
            dividendDatesVector,
            dividendsVector,
            optionType,
            payoff,
            strike,
            exercise,
            exerciseDate,
            settlementDate,
            engine,
            timeSteps);
        propertiesToVaries(properties, result);
        return SUCCESS;
    } catch (const std::exception &e) {
        QuantLibAddin::QL_LOGMESSAGE("QL_OPTION_DIVIDENDVANILLA Error: "
            + std::string(e.what()));
        result = 0;
        return FAIL;
    }
}

int QL_OPTION_FORWARDVANILLA(
        const char* handle,
        const char* handleStochastic,
        const double moneyness,
        const long resetDate,
        const char* optionType,
        const char* payoff,
        const double strike,
        const char* exercise,
        const long exerciseDate,
        const long settlementDate,
        const char* engine,
        const long timeSteps,
        VariesList *result) {
    try {
        ObjHandler::Properties properties = QuantLibAddin::QL_OPTION_FORWARDVANILLA(
            handle,
            handleStochastic,
            moneyness,
            resetDate,
            optionType,
            payoff,
            strike,
            exercise,
            exerciseDate,
            settlementDate,
            engine,
            timeSteps);
        propertiesToVaries(properties, result);
        return SUCCESS;
    } catch (const std::exception &e) {
        QuantLibAddin::QL_LOGMESSAGE("QL_OPTION_FORWARDVANILLA Error: "
            + std::string(e.what()));
        result = 0;
        return FAIL;
    }
}

int QL_OPTION_SETENGINE(
        const char* handle,
        const char* engineName,
        const long timeSteps,
        VariesList *result) {
    try {
        ObjHandler::Properties properties = QuantLibAddin::QL_OPTION_SETENGINE(
            handle,
            engineName,
            timeSteps);
        propertiesToVaries(properties, result);
        return SUCCESS;
    } catch (const std::exception &e) {
        QuantLibAddin::QL_LOGMESSAGE("QL_OPTION_SETENGINE Error: "
            + std::string(e.what()));
        result = 0;
        return FAIL;
    }
}

int QL_STOCHASTIC_PROCESS(
        const char* handle,
        const double underlying,
        const char* dayCounter,
        const long settlementDate,
        const double riskFreeRate,
        const double dividendYield,
        const double volatility,
        VariesList *result) {
    try {
        ObjHandler::Properties properties = QuantLibAddin::QL_STOCHASTIC_PROCESS(
            handle,
            underlying,
            dayCounter,
            settlementDate,
            riskFreeRate,
            dividendYield,
            volatility);
        propertiesToVaries(properties, result);
        return SUCCESS;
    } catch (const std::exception &e) {
        QuantLibAddin::QL_LOGMESSAGE("QL_STOCHASTIC_PROCESS Error: "
            + std::string(e.what()));
        result = 0;
        return FAIL;
    }
}

int QL_OPTION_VANILLA(
        const char* handle,
        const char* handleStochastic,
        const char* optionType,
        const char* payoff,
        const double strike,
        const char* exercise,
        const long exerciseDate,
        const long settlementDate,
        const char* engine,
        const long timeSteps,
        VariesList *result) {
    try {
        ObjHandler::Properties properties = QuantLibAddin::QL_OPTION_VANILLA(
            handle,
            handleStochastic,
            optionType,
            payoff,
            strike,
            exercise,
            exerciseDate,
            settlementDate,
            engine,
            timeSteps);
        propertiesToVaries(properties, result);
        return SUCCESS;
    } catch (const std::exception &e) {
        QuantLibAddin::QL_LOGMESSAGE("QL_OPTION_VANILLA Error: "
            + std::string(e.what()));
        result = 0;
        return FAIL;
    }
}


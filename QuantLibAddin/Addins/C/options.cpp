
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

// this file generated automatically by autogen.py on Fri Jan 28 22:36:30 2005
// editing this file manually is not recommended

#include <qla/qladdin.hpp>
extern "C" {
#include <Addins/C/varies.h>
#include <Addins/C/defines.h>
#include <Addins/C/options.h>
}
#include <Addins/C/varies.hpp>

int QL_STOCHASTIC_PROCESS(
        const char *handle,
        const double underlying,
        const char* dayCounterID,
        const long settlementDate,
        const double riskFreeRate,
        const double dividendYield,
        const double volatility,
        VariesList *result) {
    try {
        ObjHandler::Properties properties = QuantLibAddin::QL_STOCHASTIC_PROCESS(
            handle,
            underlying,
            dayCounterID,
            settlementDate,
            riskFreeRate,
            dividendYield,
            volatility);
        propertiesToVaries(properties, result);
        return SUCCESS;
    } catch (const std::exception &e) {
        QuantLibAddin::QL_LOGMESSAGE("QL_STOCHASTIC_PROCESS_C Error: " + std::string(e.what()));
        result = 0;
        return FAIL;
    }
}

int QL_OPTION_VANILLA(
        const char *handle,
        const char* handleStochastic,
        const char* typeOption,
        const char* typePayoff,
        const double strike,
        const char* typeExercise,
        const long exerciseDate,
        const long settlementDate,
        const char* typeEngine,
        const long timeSteps,
        VariesList *result) {
    try {
        ObjHandler::Properties properties = QuantLibAddin::QL_OPTION_VANILLA(
            handle,
            handleStochastic,
            typeOption,
            typePayoff,
            strike,
            typeExercise,
            exerciseDate,
            settlementDate,
            typeEngine,
            timeSteps);
        propertiesToVaries(properties, result);
        return SUCCESS;
    } catch (const std::exception &e) {
        QuantLibAddin::QL_LOGMESSAGE("QL_OPTION_VANILLA_C Error: " + std::string(e.what()));
        result = 0;
        return FAIL;
    }
}

int QL_OPTION_ASIAN_C(
        const char *handle,
        const char* handleStochastic,
        const char* typeAverage,
        const char* typeOption,
        const char* typePayoff,
        const double strike,
        const char* typeExercise,
        const long exerciseDate,
        const long settlementDate,
        const char* typeEngine,
        const long timeSteps,
        VariesList *result) {
    try {
        ObjHandler::Properties properties = QuantLibAddin::QL_OPTION_ASIAN_C(
            handle,
            handleStochastic,
            typeAverage,
            typeOption,
            typePayoff,
            strike,
            typeExercise,
            exerciseDate,
            settlementDate,
            typeEngine,
            timeSteps);
        propertiesToVaries(properties, result);
        return SUCCESS;
    } catch (const std::exception &e) {
        QuantLibAddin::QL_LOGMESSAGE("QL_OPTION_ASIAN_C_C Error: " + std::string(e.what()));
        result = 0;
        return FAIL;
    }
}

int QL_OPTION_BARRIER(
        const char *handle,
        const char* handleStochastic,
        const char* typeBarrier,
        const double barrier,
        const double rebate,
        const char* typeOption,
        const char* typePayoff,
        const double strike,
        const char* typeExercise,
        const long exerciseDate,
        const long settlementDate,
        const char* typeEngine,
        const long timeSteps,
        VariesList *result) {
    try {
        ObjHandler::Properties properties = QuantLibAddin::QL_OPTION_BARRIER(
            handle,
            handleStochastic,
            typeBarrier,
            barrier,
            rebate,
            typeOption,
            typePayoff,
            strike,
            typeExercise,
            exerciseDate,
            settlementDate,
            typeEngine,
            timeSteps);
        propertiesToVaries(properties, result);
        return SUCCESS;
    } catch (const std::exception &e) {
        QuantLibAddin::QL_LOGMESSAGE("QL_OPTION_BARRIER_C Error: " + std::string(e.what()));
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
        QuantLibAddin::QL_LOGMESSAGE("QL_OPTION_SETENGINE_C Error: " + std::string(e.what()));
        result = 0;
        return FAIL;
    }
}



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

// this file generated automatically by autogen.py on Thu Jan 20 00:11:04 2005
// editing this file manually is not recommended

#include <QuantLibAddin/qladdin.hpp>
#include <Addins/Excel/utilities.hpp>

using namespace ObjHandler;
using namespace QuantLibAddin;

DLLEXPORT LPXLOPER qlStochasticProcess(
        double *underlying,
        char *dayCounterID,
        long *settlementDate,
        double *riskFreeRate,
        double *dividendYield,
        double *volatility) {
    try {
        std::string handle = getCaller();
        Properties properties = QL_STOCHASTIC_PROCESS(
            handle,
            *underlying,
            std::string(dayCounterID),
            *settlementDate,
            *riskFreeRate,
            *dividendYield,
            *volatility);
        static XLOPER xRet;
        setValues(&xRet, properties, handle);
        return &xRet;
    } catch (const exception &e) {
        QL_LOGMESSAGE(std::string("ERROR: QL_STOCHASTIC_PROCESS: ") + e.what());
        return 0;
    }
}

DLLEXPORT LPXLOPER qlOptionVanilla(
        char *handleStochastic,
        char *typeOption,
        char *typePayoff,
        double *strike,
        char *typeExercise,
        long *exerciseDate,
        long *settlementDate,
        char *typeEngine,
        long *timeSteps) {
    try {
        std::string handle = getCaller();
        Properties properties = QL_OPTION_VANILLA(
            handle,
            std::string(handleStochastic),
            std::string(typeOption),
            std::string(typePayoff),
            *strike,
            std::string(typeExercise),
            *exerciseDate,
            *settlementDate,
            std::string(typeEngine),
            *timeSteps);
        static XLOPER xRet;
        setValues(&xRet, properties, handle);
        return &xRet;
    } catch (const exception &e) {
        QL_LOGMESSAGE(std::string("ERROR: QL_OPTION_VANILLA: ") + e.what());
        return 0;
    }
}

DLLEXPORT LPXLOPER qlOptionAsianC(
        char *handleStochastic,
        char *typeAverage,
        char *typeOption,
        char *typePayoff,
        double *strike,
        char *typeExercise,
        long *exerciseDate,
        long *settlementDate,
        char *typeEngine,
        long *timeSteps) {
    try {
        std::string handle = getCaller();
        Properties properties = QL_OPTION_ASIAN_C(
            handle,
            std::string(handleStochastic),
            std::string(typeAverage),
            std::string(typeOption),
            std::string(typePayoff),
            *strike,
            std::string(typeExercise),
            *exerciseDate,
            *settlementDate,
            std::string(typeEngine),
            *timeSteps);
        static XLOPER xRet;
        setValues(&xRet, properties, handle);
        return &xRet;
    } catch (const exception &e) {
        QL_LOGMESSAGE(std::string("ERROR: QL_OPTION_ASIAN_C: ") + e.what());
        return 0;
    }
}

DLLEXPORT LPXLOPER qlOptionBarrier(
        char *handleStochastic,
        char *typeBarrier,
        double *barrier,
        double *rebate,
        char *typeOption,
        char *typePayoff,
        double *strike,
        char *typeExercise,
        long *exerciseDate,
        long *settlementDate,
        char *typeEngine,
        long *timeSteps) {
    try {
        std::string handle = getCaller();
        Properties properties = QL_OPTION_BARRIER(
            handle,
            std::string(handleStochastic),
            std::string(typeBarrier),
            *barrier,
            *rebate,
            std::string(typeOption),
            std::string(typePayoff),
            *strike,
            std::string(typeExercise),
            *exerciseDate,
            *settlementDate,
            std::string(typeEngine),
            *timeSteps);
        static XLOPER xRet;
        setValues(&xRet, properties, handle);
        return &xRet;
    } catch (const exception &e) {
        QL_LOGMESSAGE(std::string("ERROR: QL_OPTION_BARRIER: ") + e.what());
        return 0;
    }
}

DLLEXPORT LPXLOPER qlOptionSetEngine(
        char *handle,
        char *engineName,
        long *timeSteps) {
    try {
        Properties properties = QL_OPTION_SETENGINE(
            std::string(handle),
            std::string(engineName),
            *timeSteps);
        static XLOPER xRet;
        setValues(&xRet, properties, handle);
        return &xRet;
    } catch (const exception &e) {
        QL_LOGMESSAGE(std::string("ERROR: QL_OPTION_SETENGINE: ") + e.what());
        return 0;
    }
}


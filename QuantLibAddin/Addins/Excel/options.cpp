
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
#include <Addins/Excel/utilities.hpp>

using namespace ObjHandler;
using namespace QuantLibAddin;

using std::string;
DLLEXPORT LPXLOPER qlOptionAsianC(
        char *handleChar,
        char *handleStochastic,
        char *average,
        char *optionType,
        char *payoff,
        double *strike,
        char *exercise,
        long *exerciseDate,
        long *settlementDate,
        char *engine,
        long *timeSteps) {
    try {
        std::string handle = std::string(handleChar) + getCaller();
        Properties properties = QL_OPTION_ASIAN_C(
            handle,
            std::string(handleStochastic),
            std::string(average),
            std::string(optionType),
            std::string(payoff),
            *strike,
            std::string(exercise),
            *exerciseDate,
            *settlementDate,
            std::string(engine),
            *timeSteps);
        static XLOPER xRet;
        setValues(&xRet, properties, handle);
        return &xRet;
    } catch (const exception &e) {
        QL_LOGMESSAGE(std::string("ERROR: QL_OPTION_ASIAN_C: ") + e.what());
        return 0;
    }
}

DLLEXPORT LPXLOPER qlOptionAsianD(
        char *handleChar,
        char *handleStochastic,
        char *average,
        double *runningAccumulator,
        long *pastFixings,
        LPXLOPER fixingDates,
        char *optionType,
        char *payoff,
        double *strike,
        char *exercise,
        long *exerciseDate,
        long *settlementDate,
        char *engine,
        long *timeSteps) {
    try {
        std::vector <long> fixingDatesVector = 
            longXLOPERToVector(fixingDates);
        std::string handle = std::string(handleChar) + getCaller();
        Properties properties = QL_OPTION_ASIAN_D(
            handle,
            std::string(handleStochastic),
            std::string(average),
            *runningAccumulator,
            *pastFixings,
            fixingDatesVector,
            std::string(optionType),
            std::string(payoff),
            *strike,
            std::string(exercise),
            *exerciseDate,
            *settlementDate,
            std::string(engine),
            *timeSteps);
        static XLOPER xRet;
        setValues(&xRet, properties, handle);
        return &xRet;
    } catch (const exception &e) {
        QL_LOGMESSAGE(std::string("ERROR: QL_OPTION_ASIAN_D: ") + e.what());
        return 0;
    }
}

DLLEXPORT LPXLOPER qlOptionBarrier(
        char *handleChar,
        char *handleStochastic,
        char *typeBarrier,
        double *barrier,
        double *rebate,
        char *optionType,
        char *payoff,
        double *strike,
        char *exercise,
        long *exerciseDate,
        long *settlementDate,
        char *engine,
        long *timeSteps) {
    try {
        std::string handle = std::string(handleChar) + getCaller();
        Properties properties = QL_OPTION_BARRIER(
            handle,
            std::string(handleStochastic),
            std::string(typeBarrier),
            *barrier,
            *rebate,
            std::string(optionType),
            std::string(payoff),
            *strike,
            std::string(exercise),
            *exerciseDate,
            *settlementDate,
            std::string(engine),
            *timeSteps);
        static XLOPER xRet;
        setValues(&xRet, properties, handle);
        return &xRet;
    } catch (const exception &e) {
        QL_LOGMESSAGE(std::string("ERROR: QL_OPTION_BARRIER: ") + e.what());
        return 0;
    }
}

DLLEXPORT LPXLOPER qlOptionBasket(
        char *handleChar,
        LPXLOPER handleStochastic,
        char *basket,
        LPXLOPER correlations,
        char *optionType,
        double *strike,
        char *exercise,
        long *exerciseDate,
        long *settlementDate,
        char *engine,
        long *timeSteps) {
    try {
        std::vector <string> handleStochasticVector = 
            stringXLOPERToVector(handleStochastic);
        std::vector < std::vector <double> >correlationsMatrix = 
            doubleXLOPERToMatrix(correlations);
        std::string handle = std::string(handleChar) + getCaller();
        Properties properties = QL_OPTION_BASKET(
            handle,
            handleStochasticVector,
            std::string(basket),
            correlationsMatrix,
            std::string(optionType),
            *strike,
            std::string(exercise),
            *exerciseDate,
            *settlementDate,
            std::string(engine),
            *timeSteps);
        static XLOPER xRet;
        setValues(&xRet, properties, handle);
        return &xRet;
    } catch (const exception &e) {
        QL_LOGMESSAGE(std::string("ERROR: QL_OPTION_BASKET: ") + e.what());
        return 0;
    }
}

DLLEXPORT LPXLOPER qlOptionCliquet(
        char *handleChar,
        char *handleStochastic,
        LPXLOPER resetDates,
        char *optionType,
        double *strike,
        long *exerciseDate,
        char *engine,
        long *timeSteps) {
    try {
        std::vector <long> resetDatesVector = 
            longXLOPERToVector(resetDates);
        std::string handle = std::string(handleChar) + getCaller();
        Properties properties = QL_OPTION_CLIQUET(
            handle,
            std::string(handleStochastic),
            resetDatesVector,
            std::string(optionType),
            *strike,
            *exerciseDate,
            std::string(engine),
            *timeSteps);
        static XLOPER xRet;
        setValues(&xRet, properties, handle);
        return &xRet;
    } catch (const exception &e) {
        QL_LOGMESSAGE(std::string("ERROR: QL_OPTION_CLIQUET: ") + e.what());
        return 0;
    }
}

DLLEXPORT LPXLOPER qlOptionDividendVanilla(
        char *handleChar,
        char *handleStochastic,
        LPXLOPER dividendDates,
        LPXLOPER dividends,
        char *optionType,
        char *payoff,
        double *strike,
        char *exercise,
        long *exerciseDate,
        long *settlementDate,
        char *engine,
        long *timeSteps) {
    try {
        std::vector <long> dividendDatesVector = 
            longXLOPERToVector(dividendDates);
        std::vector <double> dividendsVector = 
            doubleXLOPERToVector(dividends);
        std::string handle = std::string(handleChar) + getCaller();
        Properties properties = QL_OPTION_DIVIDENDVANILLA(
            handle,
            std::string(handleStochastic),
            dividendDatesVector,
            dividendsVector,
            std::string(optionType),
            std::string(payoff),
            *strike,
            std::string(exercise),
            *exerciseDate,
            *settlementDate,
            std::string(engine),
            *timeSteps);
        static XLOPER xRet;
        setValues(&xRet, properties, handle);
        return &xRet;
    } catch (const exception &e) {
        QL_LOGMESSAGE(std::string("ERROR: QL_OPTION_DIVIDENDVANILLA: ") + e.what());
        return 0;
    }
}

DLLEXPORT LPXLOPER qlOptionForwardVanilla(
        char *handleChar,
        char *handleStochastic,
        double *moneyness,
        long *resetDate,
        char *optionType,
        char *payoff,
        double *strike,
        char *exercise,
        long *exerciseDate,
        long *settlementDate,
        char *engine,
        long *timeSteps) {
    try {
        std::string handle = std::string(handleChar) + getCaller();
        Properties properties = QL_OPTION_FORWARDVANILLA(
            handle,
            std::string(handleStochastic),
            *moneyness,
            *resetDate,
            std::string(optionType),
            std::string(payoff),
            *strike,
            std::string(exercise),
            *exerciseDate,
            *settlementDate,
            std::string(engine),
            *timeSteps);
        static XLOPER xRet;
        setValues(&xRet, properties, handle);
        return &xRet;
    } catch (const exception &e) {
        QL_LOGMESSAGE(std::string("ERROR: QL_OPTION_FORWARDVANILLA: ") + e.what());
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

DLLEXPORT LPXLOPER qlStochasticProcess(
        char *handleChar,
        double *underlying,
        char *dayCounter,
        long *settlementDate,
        double *riskFreeRate,
        double *dividendYield,
        double *volatility) {
    try {
        std::string handle = std::string(handleChar) + getCaller();
        Properties properties = QL_STOCHASTIC_PROCESS(
            handle,
            *underlying,
            std::string(dayCounter),
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
        char *handleChar,
        char *handleStochastic,
        char *optionType,
        char *payoff,
        double *strike,
        char *exercise,
        long *exerciseDate,
        long *settlementDate,
        char *engine,
        long *timeSteps) {
    try {
        std::string handle = std::string(handleChar) + getCaller();
        Properties properties = QL_OPTION_VANILLA(
            handle,
            std::string(handleStochastic),
            std::string(optionType),
            std::string(payoff),
            *strike,
            std::string(exercise),
            *exerciseDate,
            *settlementDate,
            std::string(engine),
            *timeSteps);
        static XLOPER xRet;
        setValues(&xRet, properties, handle);
        return &xRet;
    } catch (const exception &e) {
        QL_LOGMESSAGE(std::string("ERROR: QL_OPTION_VANILLA: ") + e.what());
        return 0;
    }
}


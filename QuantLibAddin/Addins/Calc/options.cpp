
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
#include <Addins/Calc/qladdin.hpp>
#include <Addins/Calc/utilities.hpp>

using namespace ObjHandler;
using namespace QuantLibAddin;

SEQSEQ( ANY ) SAL_CALL QLAddin::qlStochasticProcess(
        const STRING & handle,
        double underlying,
        const STRING & dayCounterID,
        sal_Int32 settlementDate,
        double riskFreeRate,
        double dividendYield,
        double volatility) THROWDEF_RTE_IAE {
    try {
        Properties properties = QL_STOCHASTIC_PROCESS(
            OUStringToString(handle),
            underlying,
            OUStringToString(dayCounterID),
            settlementDate,
            riskFreeRate,
            dividendYield,
            volatility);
        return getArray(properties, handle);
    } catch (const std::exception &e) {
        QL_LOGMESSAGE(std::string("ERROR: QL_STOCHASTIC_PROCESS: ") + e.what());
        THROW_RTE;
    }
}

SEQSEQ( ANY ) SAL_CALL QLAddin::qlOptionVanilla(
        const STRING & handle,
        const STRING & handleStochastic,
        const STRING & typeOption,
        const STRING & typePayoff,
        double strike,
        const STRING & typeExercise,
        sal_Int32 exerciseDate,
        sal_Int32 settlementDate,
        const STRING & typeEngine,
        sal_Int32 timeSteps) THROWDEF_RTE_IAE {
    try {
        Properties properties = QL_OPTION_VANILLA(
            OUStringToString(handle),
            OUStringToString(handleStochastic),
            OUStringToString(typeOption),
            OUStringToString(typePayoff),
            strike,
            OUStringToString(typeExercise),
            exerciseDate,
            settlementDate,
            OUStringToString(typeEngine),
            timeSteps);
        return getArray(properties, handle);
    } catch (const std::exception &e) {
        QL_LOGMESSAGE(std::string("ERROR: QL_OPTION_VANILLA: ") + e.what());
        THROW_RTE;
    }
}

SEQSEQ( ANY ) SAL_CALL QLAddin::qlOptionAsianC(
        const STRING & handle,
        const STRING & handleStochastic,
        const STRING & typeAverage,
        const STRING & typeOption,
        const STRING & typePayoff,
        double strike,
        const STRING & typeExercise,
        sal_Int32 exerciseDate,
        sal_Int32 settlementDate,
        const STRING & typeEngine,
        sal_Int32 timeSteps) THROWDEF_RTE_IAE {
    try {
        Properties properties = QL_OPTION_ASIAN_C(
            OUStringToString(handle),
            OUStringToString(handleStochastic),
            OUStringToString(typeAverage),
            OUStringToString(typeOption),
            OUStringToString(typePayoff),
            strike,
            OUStringToString(typeExercise),
            exerciseDate,
            settlementDate,
            OUStringToString(typeEngine),
            timeSteps);
        return getArray(properties, handle);
    } catch (const std::exception &e) {
        QL_LOGMESSAGE(std::string("ERROR: QL_OPTION_ASIAN_C: ") + e.what());
        THROW_RTE;
    }
}

SEQSEQ( ANY ) SAL_CALL QLAddin::qlOptionAsianD(
        const STRING & handle,
        const STRING & handleStochastic,
        const STRING & typeAverage,
        double runningAccumulator,
        sal_Int32 pastFixings,
        const SEQ(sal_Int32 )& fixingDates,
        const STRING & typeOption,
        const STRING & typePayoff,
        double strike,
        const STRING & typeExercise,
        sal_Int32 exerciseDate,
        sal_Int32 settlementDate,
        const STRING & typeEngine,
        sal_Int32 timeSteps) THROWDEF_RTE_IAE {
    try {
        std::vector <long> fixingDatesVector = 
            sequenceToVector(fixingDates);
        Properties properties = QL_OPTION_ASIAN_D(
            OUStringToString(handle),
            OUStringToString(handleStochastic),
            OUStringToString(typeAverage),
            runningAccumulator,
            pastFixings,
            fixingDatesVector,
            OUStringToString(typeOption),
            OUStringToString(typePayoff),
            strike,
            OUStringToString(typeExercise),
            exerciseDate,
            settlementDate,
            OUStringToString(typeEngine),
            timeSteps);
        return getArray(properties, handle);
    } catch (const std::exception &e) {
        QL_LOGMESSAGE(std::string("ERROR: QL_OPTION_ASIAN_D: ") + e.what());
        THROW_RTE;
    }
}

SEQSEQ( ANY ) SAL_CALL QLAddin::qlOptionBarrier(
        const STRING & handle,
        const STRING & handleStochastic,
        const STRING & typeBarrier,
        double barrier,
        double rebate,
        const STRING & typeOption,
        const STRING & typePayoff,
        double strike,
        const STRING & typeExercise,
        sal_Int32 exerciseDate,
        sal_Int32 settlementDate,
        const STRING & typeEngine,
        sal_Int32 timeSteps) THROWDEF_RTE_IAE {
    try {
        Properties properties = QL_OPTION_BARRIER(
            OUStringToString(handle),
            OUStringToString(handleStochastic),
            OUStringToString(typeBarrier),
            barrier,
            rebate,
            OUStringToString(typeOption),
            OUStringToString(typePayoff),
            strike,
            OUStringToString(typeExercise),
            exerciseDate,
            settlementDate,
            OUStringToString(typeEngine),
            timeSteps);
        return getArray(properties, handle);
    } catch (const std::exception &e) {
        QL_LOGMESSAGE(std::string("ERROR: QL_OPTION_BARRIER: ") + e.what());
        THROW_RTE;
    }
}

SEQSEQ( ANY ) SAL_CALL QLAddin::qlOptionSetEngine(
        const STRING & handle,
        const STRING & engineName,
        sal_Int32 timeSteps) THROWDEF_RTE_IAE {
    try {
        Properties properties = QL_OPTION_SETENGINE(
            OUStringToString(handle),
            OUStringToString(engineName),
            timeSteps);
        return getArray(properties, handle);
    } catch (const std::exception &e) {
        QL_LOGMESSAGE(std::string("ERROR: QL_OPTION_SETENGINE: ") + e.what());
        THROW_RTE;
    }
}


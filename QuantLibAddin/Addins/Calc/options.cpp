
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

using std::string;

SEQSEQ( ANY ) SAL_CALL QLAddin::qlOptionAsianC(
        const STRING & handle,
        const STRING & handleStochastic,
        const STRING & average,
        const STRING & optionType,
        const STRING & payoff,
        double strike,
        const STRING & exercise,
        sal_Int32 exerciseDate,
        sal_Int32 settlementDate,
        const STRING & engine,
        sal_Int32 timeSteps) THROWDEF_RTE_IAE {
    try {
        Properties properties = QL_OPTION_ASIAN_C(
            OUStringToString(handle),
            OUStringToString(handleStochastic),
            OUStringToString(average),
            OUStringToString(optionType),
            OUStringToString(payoff),
            strike,
            OUStringToString(exercise),
            exerciseDate,
            settlementDate,
            OUStringToString(engine),
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
        const STRING & average,
        double runningAccumulator,
        sal_Int32 pastFixings,
        const SEQSEQ(sal_Int32 )& fixingDates,
        const STRING & optionType,
        const STRING & payoff,
        double strike,
        const STRING & exercise,
        sal_Int32 exerciseDate,
        sal_Int32 settlementDate,
        const STRING & engine,
        sal_Int32 timeSteps) THROWDEF_RTE_IAE {
    try {
        std::vector <long> fixingDatesVector =
            longSequenceToVector(fixingDates);
        Properties properties = QL_OPTION_ASIAN_D(
            OUStringToString(handle),
            OUStringToString(handleStochastic),
            OUStringToString(average),
            runningAccumulator,
            pastFixings,
            fixingDatesVector,
            OUStringToString(optionType),
            OUStringToString(payoff),
            strike,
            OUStringToString(exercise),
            exerciseDate,
            settlementDate,
            OUStringToString(engine),
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
        const STRING & optionType,
        const STRING & payoff,
        double strike,
        const STRING & exercise,
        sal_Int32 exerciseDate,
        sal_Int32 settlementDate,
        const STRING & engine,
        sal_Int32 timeSteps) THROWDEF_RTE_IAE {
    try {
        Properties properties = QL_OPTION_BARRIER(
            OUStringToString(handle),
            OUStringToString(handleStochastic),
            OUStringToString(typeBarrier),
            barrier,
            rebate,
            OUStringToString(optionType),
            OUStringToString(payoff),
            strike,
            OUStringToString(exercise),
            exerciseDate,
            settlementDate,
            OUStringToString(engine),
            timeSteps);
        return getArray(properties, handle);
    } catch (const std::exception &e) {
        QL_LOGMESSAGE(std::string("ERROR: QL_OPTION_BARRIER: ") + e.what());
        THROW_RTE;
    }
}

SEQSEQ( ANY ) SAL_CALL QLAddin::qlOptionBasket(
        const STRING & handle,
        const SEQSEQ(STRING )& handleStochastic,
        const STRING & basket,
        const SEQSEQ(double )& correlations,
        const STRING & optionType,
        double strike,
        const STRING & exercise,
        sal_Int32 exerciseDate,
        sal_Int32 settlementDate,
        const STRING & engine,
        sal_Int32 timeSteps) THROWDEF_RTE_IAE {
    try {
        std::vector <string> handleStochasticVector =
            stringSequenceToVector(handleStochastic);
        std::vector < std::vector < double> >correlationsMatrix =
            doubleSequenceToMatrix(correlations);
        Properties properties = QL_OPTION_BASKET(
            OUStringToString(handle),
            handleStochasticVector,
            OUStringToString(basket),
            correlationsMatrix,
            OUStringToString(optionType),
            strike,
            OUStringToString(exercise),
            exerciseDate,
            settlementDate,
            OUStringToString(engine),
            timeSteps);
        return getArray(properties, handle);
    } catch (const std::exception &e) {
        QL_LOGMESSAGE(std::string("ERROR: QL_OPTION_BASKET: ") + e.what());
        THROW_RTE;
    }
}

SEQSEQ( ANY ) SAL_CALL QLAddin::qlOptionCliquet(
        const STRING & handle,
        const STRING & handleStochastic,
        const SEQSEQ(sal_Int32 )& resetDates,
        const STRING & optionType,
        double strike,
        sal_Int32 exerciseDate,
        const STRING & engine,
        sal_Int32 timeSteps) THROWDEF_RTE_IAE {
    try {
        std::vector <long> resetDatesVector =
            longSequenceToVector(resetDates);
        Properties properties = QL_OPTION_CLIQUET(
            OUStringToString(handle),
            OUStringToString(handleStochastic),
            resetDatesVector,
            OUStringToString(optionType),
            strike,
            exerciseDate,
            OUStringToString(engine),
            timeSteps);
        return getArray(properties, handle);
    } catch (const std::exception &e) {
        QL_LOGMESSAGE(std::string("ERROR: QL_OPTION_CLIQUET: ") + e.what());
        THROW_RTE;
    }
}

SEQSEQ( ANY ) SAL_CALL QLAddin::qlOptionDividendVanilla(
        const STRING & handle,
        const STRING & handleStochastic,
        const SEQSEQ(sal_Int32 )& dividendDates,
        const SEQSEQ(double )& dividends,
        const STRING & optionType,
        const STRING & payoff,
        double strike,
        const STRING & exercise,
        sal_Int32 exerciseDate,
        sal_Int32 settlementDate,
        const STRING & engine,
        sal_Int32 timeSteps) THROWDEF_RTE_IAE {
    try {
        std::vector <long> dividendDatesVector =
            longSequenceToVector(dividendDates);
        std::vector <double> dividendsVector =
            doubleSequenceToVector(dividends);
        Properties properties = QL_OPTION_DIVIDENDVANILLA(
            OUStringToString(handle),
            OUStringToString(handleStochastic),
            dividendDatesVector,
            dividendsVector,
            OUStringToString(optionType),
            OUStringToString(payoff),
            strike,
            OUStringToString(exercise),
            exerciseDate,
            settlementDate,
            OUStringToString(engine),
            timeSteps);
        return getArray(properties, handle);
    } catch (const std::exception &e) {
        QL_LOGMESSAGE(std::string("ERROR: QL_OPTION_DIVIDENDVANILLA: ") + e.what());
        THROW_RTE;
    }
}

SEQSEQ( ANY ) SAL_CALL QLAddin::qlOptionForwardVanilla(
        const STRING & handle,
        const STRING & handleStochastic,
        double moneyness,
        sal_Int32 resetDate,
        const STRING & optionType,
        const STRING & payoff,
        double strike,
        const STRING & exercise,
        sal_Int32 exerciseDate,
        sal_Int32 settlementDate,
        const STRING & engine,
        sal_Int32 timeSteps) THROWDEF_RTE_IAE {
    try {
        Properties properties = QL_OPTION_FORWARDVANILLA(
            OUStringToString(handle),
            OUStringToString(handleStochastic),
            moneyness,
            resetDate,
            OUStringToString(optionType),
            OUStringToString(payoff),
            strike,
            OUStringToString(exercise),
            exerciseDate,
            settlementDate,
            OUStringToString(engine),
            timeSteps);
        return getArray(properties, handle);
    } catch (const std::exception &e) {
        QL_LOGMESSAGE(std::string("ERROR: QL_OPTION_FORWARDVANILLA: ") + e.what());
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

SEQSEQ( ANY ) SAL_CALL QLAddin::qlStochasticProcess(
        const STRING & handle,
        double underlying,
        const STRING & dayCounter,
        sal_Int32 settlementDate,
        double riskFreeRate,
        double dividendYield,
        double volatility) THROWDEF_RTE_IAE {
    try {
        Properties properties = QL_STOCHASTIC_PROCESS(
            OUStringToString(handle),
            underlying,
            OUStringToString(dayCounter),
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
        const STRING & optionType,
        const STRING & payoff,
        double strike,
        const STRING & exercise,
        sal_Int32 exerciseDate,
        sal_Int32 settlementDate,
        const STRING & engine,
        sal_Int32 timeSteps) THROWDEF_RTE_IAE {
    try {
        Properties properties = QL_OPTION_VANILLA(
            OUStringToString(handle),
            OUStringToString(handleStochastic),
            OUStringToString(optionType),
            OUStringToString(payoff),
            strike,
            OUStringToString(exercise),
            exerciseDate,
            settlementDate,
            OUStringToString(engine),
            timeSteps);
        return getArray(properties, handle);
    } catch (const std::exception &e) {
        QL_LOGMESSAGE(std::string("ERROR: QL_OPTION_VANILLA: ") + e.what());
        THROW_RTE;
    }
}


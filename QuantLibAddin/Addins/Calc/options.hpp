
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

    virtual SEQSEQ(ANY) SAL_CALL qlOptionAsianC(
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
        sal_Int32 timeSteps) THROWDEF_RTE_IAE;

    virtual SEQSEQ(ANY) SAL_CALL qlOptionAsianD(
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
        sal_Int32 timeSteps) THROWDEF_RTE_IAE;

    virtual SEQSEQ(ANY) SAL_CALL qlOptionBarrier(
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
        sal_Int32 timeSteps) THROWDEF_RTE_IAE;

    virtual SEQSEQ(ANY) SAL_CALL qlOptionBasket(
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
        sal_Int32 timeSteps) THROWDEF_RTE_IAE;

    virtual SEQSEQ(ANY) SAL_CALL qlOptionCliquet(
        const STRING & handle,
        const STRING & handleStochastic,
        const SEQSEQ(sal_Int32 )& resetDates,
        const STRING & optionType,
        double strike,
        sal_Int32 exerciseDate,
        const STRING & engine,
        sal_Int32 timeSteps) THROWDEF_RTE_IAE;

    virtual SEQSEQ(ANY) SAL_CALL qlOptionDividendVanilla(
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
        sal_Int32 timeSteps) THROWDEF_RTE_IAE;

    virtual SEQSEQ(ANY) SAL_CALL qlOptionForwardVanilla(
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
        sal_Int32 timeSteps) THROWDEF_RTE_IAE;

    virtual SEQSEQ(ANY) SAL_CALL qlOptionSetEngine(
        const STRING & handle,
        const STRING & engineName,
        sal_Int32 timeSteps) THROWDEF_RTE_IAE;

    virtual SEQSEQ(ANY) SAL_CALL qlStochasticProcess(
        const STRING & handle,
        double underlying,
        const STRING & dayCounter,
        sal_Int32 settlementDate,
        double riskFreeRate,
        double dividendYield,
        double volatility) THROWDEF_RTE_IAE;

    virtual SEQSEQ(ANY) SAL_CALL qlOptionVanilla(
        const STRING & handle,
        const STRING & handleStochastic,
        const STRING & optionType,
        const STRING & payoff,
        double strike,
        const STRING & exercise,
        sal_Int32 exerciseDate,
        sal_Int32 settlementDate,
        const STRING & engine,
        sal_Int32 timeSteps) THROWDEF_RTE_IAE;


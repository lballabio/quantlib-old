
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

#ifndef qlac_options_h
#define qlac_options_h

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
        VariesList *result);

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
        VariesList *result);

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
        VariesList *result);

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
        VariesList *result);

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
        VariesList *result);

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
        VariesList *result);

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
        VariesList *result);

int QL_OPTION_SETENGINE(
        const char* handle,
        const char* engineName,
        const long timeSteps,
        VariesList *result);

int QL_STOCHASTIC_PROCESS(
        const char* handle,
        const double underlying,
        const char* dayCounter,
        const long settlementDate,
        const double riskFreeRate,
        const double dividendYield,
        const double volatility,
        VariesList *result);

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
        VariesList *result);

#endif

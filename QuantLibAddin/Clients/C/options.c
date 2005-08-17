
/*
 Copyright (C) 2005 Eric Ehlers

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

#include <Addins/C/qladdin.h>
#include <stdio.h>
#include <malloc.h>

int main() {
    // inputs for various functions:
    double dividendYield = 0.00;
    double riskFreeRate = 0.06;
    double volatility = 0.20;
    double underlying = 36;
    double strike = 40;
    long timeSteps = 801;
    long exerciseDate = 43903;      // (13, March, 2020);
    long settlementDate = 43537;    // (13, March, 2019);
    long todaysDate = 38424;        // (13, March, 2005);
    long fixingDatesCount;
    long *fixingDates;
    char *stochHandles[] = { "stoch1", "stoch1" };
    double **correlations;
    long resetDates[] = { 43902 };
    long dividendDates[] = { 43721, 43903 };
    double dividends[] = { 5., 5. };
    long resetDate = 43813;
    char *ret = 0;                  // dummy value
    int i;                          // iterator

    OH_SET_LOGFILE("quantlib.log");
    OH_CONSOLE(1);
    OH_LOG_MESSAGE("begin options test");

    if (QL_BLACK_CONSTANT_VOL(
            "blackconstantvol", 
            settlementDate, 
            volatility, 
            "Actual360",
            ret) != SUCCESS) {
        OH_LOG_MESSAGE("Error on call to QL_BLACK_CONSTANT_VOL");
        goto fail;
    }

    if (QL_BLACK_SCHOLES_PROCESS(
            "stoch1", 
            "blackconstantvol", 
            underlying, 
            "Actual360",
            settlementDate, 
            riskFreeRate, 
            dividendYield, 
            ret) != SUCCESS) {
        OH_LOG_MESSAGE("Error on call to QL_BLACK_SCHOLES_PROCESS");
        goto fail;
    }

    OH_LOG_OBJECT("stoch1");

    if (QL_VANILLA_OPTION(
            "opt_van",                      // option handle
            "stoch1",                       // stochastic process handle
            "Put",                          // option type
            "Vanilla",                      // payoff type
            strike,                         // strike price
            "American",                     // exercise type
            exerciseDate,                   // exercise date
            settlementDate,                 // settlement date
            "JR",                           // engine type (jarrow rudd)
            timeSteps,                      // time steps
            ret) != SUCCESS) {
        OH_LOG_MESSAGE("Error on call to QL_VANILLA_OPTION");
        goto fail;
    }

    OH_LOG_OBJECT("opt_van");

    if (QL_CA_ASIAN_OPTION(
            "opt_asian_cont",               // option handle
            "stoch1",                       // stochastic process handle
            "Geometric",                    // average type
            "Put",                          // option type
            "Vanilla",                      // payoff type
            strike,                         // strike price
            "European",                     // exercise type
            exerciseDate,                   // exercise date
            0,                              // settlement date ignored when exercise = European
            "ACGAPA",                       // engine type (AnalyticContinuousGeometricAveragePriceAsianEngine)
            timeSteps,                      // time steps
            ret) != SUCCESS) {
        OH_LOG_MESSAGE("Error on call to QL_CA_ASIAN_OPTION");
        goto fail;
    }

    OH_LOG_OBJECT("opt_asian_cont");

    fixingDatesCount = exerciseDate - todaysDate + 1;
    fixingDates = (long *) malloc(sizeof(long) * fixingDatesCount);
    for (i = 0; i < fixingDatesCount; i++)
        fixingDates[i] = todaysDate + i;
    if (QL_DA_ASIAN_OPTION(
            "opt_asian_disc",               // option handle
            "stoch1",                       // stochastic process handle
            "Geometric",                    // average type
            1.0,                            // running accumulator
            0,                              // past fixings
            fixingDatesCount,               // #/fixingDates
            fixingDates,                    // fixingDates
            "Put",                          // option type
            "Vanilla",                      // payoff type
            strike,                         // strike price
            "European",                     // exercise type
            exerciseDate,                   // exercise date
            0,                              // settlement date ignored when exercise = European
            "ADGAPA",                       // engine type (AnalyticDiscreteGeometricAveragePriceAsianEngine)
            timeSteps,                      // time steps
            ret) != SUCCESS) {
        OH_LOG_MESSAGE("Error on call to QL_DA_ASIAN_OPTION");
        goto fail;
    }

    OH_LOG_OBJECT("opt_asian_disc");

    if (QL_BARRIER_OPTION(
            "opt_barrier",                  // option handle
            "stoch1",                       // stochastic process handle
            "DownIn",                       // barrier type
            35.0,                           // barrier
            3.0,                            // rebate
            "Put",                          // option type
            "Vanilla",                      // payoff type
            strike,                         // strike price
            "European",                     // exercise type
            exerciseDate,                   // exercise date
            0,                              // settlement date ignored when exercise = European
            "AB",                           // engine type (AnalyticBarrierEngine)
            timeSteps,                      // time steps
            ret) != SUCCESS) {
        OH_LOG_MESSAGE("Error on call to QL_BARRIER_OPTION");
        goto fail;
    }

    OH_LOG_OBJECT("opt_barrier");

    correlations = (double**)malloc(sizeof(double*)*2);
    correlations[0] = (double*)malloc(sizeof(double)*2);
    correlations[1] = (double*)malloc(sizeof(double)*2);
    correlations[0][0] = 1.0;
    correlations[0][1] = 0.9;
    correlations[1][0] = 0.9;
    correlations[1][1] = 1.0;
    if (QL_BASKET_OPTION(
            "opt_basket",                   // option handle
            2,                              // #/stochastic processes
            stochHandles,                   // array of stoch process handles
            "Min",                          // basket type
            2,                              // #/rows in correlations matrix
            2,                              // #/cols in correlations matrix
            correlations,                   // correlations matrix
            "Call",                         // option type
            40.0,                           // strike price
            "European",                     // exercise type
            exerciseDate,                   // exercise date
            0,                              // settlement date ignored when exercise = European
            "SE",                           // engine type (StulzEngine)
            timeSteps,                      // time steps
            ret) != SUCCESS) {
        OH_LOG_MESSAGE("Error on call to QL_BASKET_OPTION");
        goto fail;
    }

    OH_LOG_OBJECT("opt_basket");

    if (QL_CLIQUET_OPTION(
            "opt_cliquet",                  // option handle
            "stoch1",                       // stochastic process handle
            1,                              // #/reset dates
            resetDates,                     // reset dates
            "Put",                          // option type
            strike,                         // strike price
            exerciseDate,                   // exercise date
            "AC",                           // engine type (AnalyticCliquetEngine)
            timeSteps,                      // time steps
            ret) != SUCCESS) {
        OH_LOG_MESSAGE("Error on call to QL_CLIQUET_OPTION");
        goto fail;
    }

    OH_LOG_OBJECT("opt_cliquet");

    if (QL_DIVIDEND_VANILLA_OPTION(
            "opt_divvan",                   // option handle
            "stoch1",                       // stochastic process handle
            2,                              // #/dividend dates
            dividendDates,                  // dividend dates
            2,                              // #/dividends
            dividends,                      // dividends
            "Call",                         // option type
            "Vanilla",                      // payoff type
            10.0,                           // strike price
            "European",                     // exercise type
            exerciseDate,                   // exercise date
            0,                              // settlement date ignored when exercise = European
            "ADE",                          // engine type (AnalyticDividendEuropeanEngine)
            timeSteps,                      // time steps
            ret) != SUCCESS) {
        OH_LOG_MESSAGE("Error on call to QL_DIVIDEND_VANILLA_OPTION");
        goto fail;
    }

    OH_LOG_OBJECT("opt_divvan");

    if (QL_FORWARD_VANILLA_OPTION(
            "opt_fwdvan",                   // option handle
            "stoch1",                       // stochastic process handle
            12.,                            // moneyness
            resetDate,                      // reset date
            "Put",                          // option type
            "Vanilla",                      // payoff type
            strike,                         // strike price
            "European",                     // exercise type
            exerciseDate,                   // exercise date
            0,                              // settlement date ignored when exercise = European
            "FE",                           // engine type (ForwardEngine)
            timeSteps,                      // time steps
            ret) != SUCCESS) {
        OH_LOG_MESSAGE("Error on call to QL_FORWARD_VANILLA_OPTION");
        goto fail;
    }

    OH_LOG_OBJECT("opt_fwdvan");

    free(fixingDates);
    for (i=0;i<2;i++)
        free(correlations[i]);
    free(correlations);

    OH_LOG_MESSAGE("end options test");

    return 0;

fail:

    return 1;
}


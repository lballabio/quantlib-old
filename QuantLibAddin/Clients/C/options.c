
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

void printVariesList(const char *s, const VariesList vo) {
    int i;
    printf("%s\n", s);
    for (i=0; i<vo.count; i++)
        printf("field = %s, value = %s\n", vo.varies[i].Label, 
            variesToString(&vo.varies[i]));
    printf("\n");
}

int main() {
    VariesList vbs;                 // black scholes
    VariesList vo;                  // vanilla option
    VariesList voac;                // asian continuous
    VariesList voad;                // asian discrete
    VariesList voba;                // barrier
    VariesList vobs;                // basket
    VariesList voc;                 // cliquet
    VariesList vod;                 // dividend
    VariesList vof;                 // forward
    int i;                          // iterators
    // inputs for various functions:
    double dividendYield = 0.00;
    double riskFreeRate = 0.06;
    double volatility = 0.20;
    double underlying = 36;
    double strike = 40;
    long timeSteps = 801;
    long exerciseDate = 36297;      // (17, May, 1999);
    long settlementDate = 35932;    // (17, May, 1998);
    long todaysDate = 35930;        // (15, May, 1998);
    long fixingDatesCount;
    long *fixingDates;
    char *stochHandles[] = { "stoch1", "stoch1" };
    double **correlations;
    long resetDates[] = { 36020 };
    long dividendDates[] = { 36022, 36206 };
    double dividends[] = { 5., 5. };
    long resetDate = 36020;

    printf("begin options test\n\n");

    QL_LOGFILE("quantlib.log");
    QL_LOGMESSAGE("begin options test");

    if (QL_STOCHASTIC_PROCESS(
            "stoch1", 
            underlying, 
            "ACT360",
            settlementDate, 
            riskFreeRate, 
            dividendYield, 
            volatility, 
            &vbs) != SUCCESS) {
        printf("Error on call to QL_STOCHASTIC_PROCESS\n");
        goto fail;
    }

    printVariesList("QL_STOCHASTIC_PROCESS", vbs);

    if (QL_OPTION_VANILLA(
            "opt_van",                      // option handle
            "stoch1",                       // stochastic process handle
            "PUT",                          // option type
            "VAN",                          // payoff type (plain vanilla)
			strike,                         // strike price
            "AM",                           // exercise type (american)
            exerciseDate,                   // exercise date
            settlementDate,                 // settlement date
            "JR",                           // engine type (jarrow rudd)
            timeSteps,                      // time steps
            &vo) != SUCCESS) {
        printf("Error on call to QL_OPTION_VANILLA\n");
        goto fail;
    }

    printVariesList("QL_OPTION_VANILLA", vo);

    if (QL_OPTION_ASIAN_C(
            "opt_asian_cont",               // option handle
            "stoch1",                       // stochastic process handle
            "G",                            // average type ("A"verage/"G"eometric)
            "PUT",                          // option type
            "VAN",                          // payoff type
            strike,                         // strike price
            "EU",                           // exercise type
            exerciseDate,                   // exercise date
            settlementDate,                 // settlement date
            "ACGAPA",                       // engine type
            timeSteps,                      // time steps
            &voac) != SUCCESS) {
        printf("Error on call to QL_OPTION_VANILLA\n");
        goto fail;
    }

    printVariesList("QL_OPTION_ASIAN_C", voac);

    fixingDatesCount = exerciseDate - todaysDate + 1;
    fixingDates = (long *) malloc(sizeof(long) * fixingDatesCount);
    for (i = 0; i < fixingDatesCount; i++)
        fixingDates[i] = todaysDate + i;
    if (QL_OPTION_ASIAN_D(
            "opt_asian_disc",               // option handle
            "stoch1",                       // stochastic process handle
            "G",                            // average type ("A"verage/"G"eometric)
            1.0,                            // running accumulator
            0,                              // past fixings
            fixingDatesCount,               // #/fixingDates
            fixingDates,                    // fixingDates
            "PUT",                          // option type
            "VAN",                          // payoff type (plain vanilla)
            strike,                         // strike price
            "EU",                           // exercise type (european)
            exerciseDate,                   // exercise date
            settlementDate,                 // settlement date
            "ADGAPA",                       // engine type (AnalyticDiscreteGeometricAveragePriceAsianEngine)
            timeSteps,                      // time steps
            &voad) != SUCCESS) {
        printf("Error on call to QL_OPTION_ASIAN_D\n");
        goto fail;
    }

    printVariesList("QL_OPTION_ASIAN_D", voad);

    if (QL_OPTION_BARRIER(
            "opt_barrier",                  // option handle
            "stoch1",                       // stochastic process handle
            "DOWNIN",                       // barrier type
            35.0,                           // barrier
            3.0,                            // rebate
            "PUT",                          // option type
            "VAN",                          // payoff type
            strike,                         // strike price
            "EU",                           // exercise type
            exerciseDate,                   // exercise date
            settlementDate,                 // settlement date
            "AB",                           // engine type
            timeSteps,                      // time steps
            &voba) != SUCCESS) {
        printf("Error on call to QL_OPTION_BARRIER\n");
        goto fail;
    }

    printVariesList("QL_OPTION_BARRIER", voba);

    correlations = (double**)malloc(sizeof(double*)*2);
    correlations[0] = (double*)malloc(sizeof(double)*2);
    correlations[1] = (double*)malloc(sizeof(double)*2);
    correlations[0][0] = 1.0;
    correlations[0][1] = 0.9;
    correlations[1][0] = 0.9;
    correlations[1][1] = 1.0;
    if (QL_OPTION_BASKET(
            "opt_basket",                   // option handle
            2,                              // #/stochastic processes
            stochHandles,                   // array of stoch process handles
            "MIN",                          // basket type
            2,                              // #/rows in correlations matrix
            2,                              // #/cols in correlations matrix
            correlations,                   // correlations matrix
            "CALL",                         // option type
            40.0,                           // strike price
            "EU",                           // exercise type
            exerciseDate,                   // exercise date
            settlementDate,                 // settlement date
            "SE",                           // engine type
            timeSteps,                      // time steps
            &vobs) != SUCCESS) {
        printf("Error on call to QL_OPTION_BASKET\n");
        goto fail;
    }

    printVariesList("QL_OPTION_BASKET", vobs);

    if (QL_OPTION_CLIQUET(
            "opt_cliquet",                  // option handle
            "stoch1",                       // stochastic process handle
            1,                              // #/reset dates
            resetDates,                     // reset dates
            "PUT",                          // option type
            strike,                         // strike price
            exerciseDate,                   // exercise date
            "AC",                           // engine type
            timeSteps,                      // time steps
            &voc) != SUCCESS) {
        printf("Error on call to QL_OPTION_CLIQUET\n");
        goto fail;
    }

    printVariesList("QL_OPTION_CLIQUET", voc);

    if (QL_OPTION_DIVIDENDVANILLA(
            "opt_divvan",                   // option handle
            "stoch1",                       // stochastic process handle
            2,                              // #/dividend dates
            dividendDates,                  // dividend dates
            2,                              // #/dividends
            dividends,                      // dividends
            "CALL",                         // option type
            "VAN",                          // payoff type
            10.0,                           // strike price
            "EU",                           // exercise type
            exerciseDate,                   // exercise date
            settlementDate,                 // settlement date
            "ADE",                          // engine type
            timeSteps,                      // time steps
            &vod) != SUCCESS) {
        printf("Error on call to QL_OPTION_DIVIDENDVANILLA\n");
        goto fail;
    }

    printVariesList("QL_OPTION_DIVIDENDVANILLA", vod);

    if (QL_OPTION_FORWARDVANILLA(
            "opt_fwdvan",                   // option handle
            "stoch1",                       // stochastic process handle
            12.,                            // moneyness
            resetDate,                      // reset date
            "PUT",                          // option type
            "VAN",                          // payoff type
            strike,                         // strike price
            "EU",                           // exercise type
            exerciseDate,                   // exercise date
            settlementDate,                 // settlement date
            "FE",                           // engine type
            timeSteps,                      // time steps
            &vof) != SUCCESS) {
        printf("Error on call to QL_OPTION_FORWARDVANILLA\n");
        goto fail;
    }

    printVariesList("QL_OPTION_FORWARDVANILLA", vof);

    free(fixingDates);
    for (i=0;i<2;i++)
        free(correlations[i]);
    free(correlations);
    freeVariesList(&vbs);
    freeVariesList(&vo);
    freeVariesList(&voac);
    freeVariesList(&voad);
    freeVariesList(&voba);
    freeVariesList(&vobs);
    freeVariesList(&voc);
    freeVariesList(&vod);
    freeVariesList(&vof);

    QL_LOGMESSAGE("end options test");

    printf("end options test\n");

    return 0;

fail:

    return 1;
}


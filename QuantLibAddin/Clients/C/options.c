
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
    QL_LOGMESSAGE(s);
    for (i=0; i<vo.count; i++)
        QL_LOGMESSAGE("field = %s, value = %s", vo.varies[i].Label, 
            variesToString(&vo.varies[i]));
}

int main() {
    VariesList vbc;                 // black constant vols
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

    QL_LOGFILE("quantlib.log");
    QL_CONSOLE(1);
    QL_LOGMESSAGE("begin options test");

    if (QL_BLACK_CONSTANT_VOL(
            "blackconstantvol", 
            settlementDate, 
            volatility, 
            "ACT360",
            &vbc) != SUCCESS) {
        QL_LOGMESSAGE("Error on call to QL_BLACK_CONSTANT_VOL");
        goto fail;
    }

    if (QL_BLACK_SCHOLES_PROCESS(
            "stoch1", 
            "blackconstantvol", 
            underlying, 
            "ACT360",
            settlementDate, 
            riskFreeRate, 
            dividendYield, 
            &vbs) != SUCCESS) {
        QL_LOGMESSAGE("Error on call to QL_BLACK_SCHOLES_PROCESS");
        goto fail;
    }

    printVariesList("QL_BLACK_SCHOLES_PROCESS", vbs);

    if (QL_VANILLA_OPTION(
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
        QL_LOGMESSAGE("Error on call to QL_VANILLA_OPTION");
        goto fail;
    }

    printVariesList("QL_VANILLA_OPTION", vo);

    if (QL_CA_ASIAN_OPTION(
            "opt_asian_cont",               // option handle
            "stoch1",                       // stochastic process handle
            "G",                            // average type ("A"verage/"G"eometric)
            "PUT",                          // option type
            "VAN",                          // payoff type
            strike,                         // strike price
            "EU",                           // exercise type
            exerciseDate,                   // exercise date
            0,                              // settlement date ignored when exercise = European
            "ACGAPA",                       // engine type
            timeSteps,                      // time steps
            &voac) != SUCCESS) {
        QL_LOGMESSAGE("Error on call to QL_CA_ASIAN_OPTION");
        goto fail;
    }

    printVariesList("QL_CA_ASIAN_OPTION", voac);

    fixingDatesCount = exerciseDate - todaysDate + 1;
    fixingDates = (long *) malloc(sizeof(long) * fixingDatesCount);
    for (i = 0; i < fixingDatesCount; i++)
        fixingDates[i] = todaysDate + i;
    if (QL_DA_ASIAN_OPTION(
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
            0,                              // settlement date ignored when exercise = European
            "ADGAPA",                       // engine type (AnalyticDiscreteGeometricAveragePriceAsianEngine)
            timeSteps,                      // time steps
            &voad) != SUCCESS) {
        QL_LOGMESSAGE("Error on call to QL_DA_ASIAN_OPTION");
        goto fail;
    }

    printVariesList("QL_DA_ASIAN_OPTION", voad);

    if (QL_BARRIER_OPTION(
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
            0,                              // settlement date ignored when exercise = European
            "AB",                           // engine type
            timeSteps,                      // time steps
            &voba) != SUCCESS) {
        QL_LOGMESSAGE("Error on call to QL_BARRIER_OPTION");
        goto fail;
    }

    printVariesList("QL_BARRIER_OPTION", voba);

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
            "MIN",                          // basket type
            2,                              // #/rows in correlations matrix
            2,                              // #/cols in correlations matrix
            correlations,                   // correlations matrix
            "CALL",                         // option type
            40.0,                           // strike price
            "EU",                           // exercise type
            exerciseDate,                   // exercise date
            0,                              // settlement date ignored when exercise = European
            "SE",                           // engine type
            timeSteps,                      // time steps
            &vobs) != SUCCESS) {
        QL_LOGMESSAGE("Error on call to QL_BASKET_OPTION");
        goto fail;
    }

    printVariesList("QL_BASKET_OPTION", vobs);

    if (QL_CLIQUET_OPTION(
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
        QL_LOGMESSAGE("Error on call to QL_CLIQUET_OPTION");
        goto fail;
    }

    printVariesList("QL_CLIQUET_OPTION", voc);

    if (QL_DIVIDEND_VANILLA_OPTION(
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
            0,                              // settlement date ignored when exercise = European
            "ADE",                          // engine type
            timeSteps,                      // time steps
            &vod) != SUCCESS) {
        QL_LOGMESSAGE("Error on call to QL_DIVIDEND_VANILLA_OPTION");
        goto fail;
    }

    printVariesList("QL_DIVIDEND_VANILLA_OPTION", vod);

    if (QL_FORWARD_VANILLA_OPTION(
            "opt_fwdvan",                   // option handle
            "stoch1",                       // stochastic process handle
            12.,                            // moneyness
            resetDate,                      // reset date
            "PUT",                          // option type
            "VAN",                          // payoff type
            strike,                         // strike price
            "EU",                           // exercise type
            exerciseDate,                   // exercise date
            0,                              // settlement date ignored when exercise = European
            "FE",                           // engine type
            timeSteps,                      // time steps
            &vof) != SUCCESS) {
        QL_LOGMESSAGE("Error on call to QL_FORWARD_VANILLA_OPTION");
        goto fail;
    }

    printVariesList("QL_FORWARD_VANILLA_OPTION", vof);

    free(fixingDates);
    for (i=0;i<2;i++)
        free(correlations[i]);
    free(correlations);
    freeVariesList(&vbc);
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

    return 0;

fail:

    return 1;
}


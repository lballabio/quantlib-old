
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

#include <Addins/C/qladdin.h>
#include <stdio.h>
#include <malloc.h>

int main() {
    double dividendYield = 0.00;
    double riskFreeRate = 0.06;
    double volatility = 0.20;
    double underlying = 36;
    double strike = 40;
    long timeSteps = 801;
    long exerciseDate = 36297;       // (17, May, 1999);
    long settlementDate = 35932;     // (17, May, 1998);
    long todaysDate = 35930;         // (15, May, 1998);
    VariesList vbs;                  // attributes of black scholes object
    VariesList vo;                   // attributes of vanilla option object
    VariesList va;                   // attributes of asian option object
    int i;                           // iterator
    long fixingDatesCount;           // #/fixing dates
    long *fixingDates;               // array of fixing dates

    printf("hi\n");

    QL_LOGFILE("quantlib.log");
    QL_LOGMESSAGE("begin example program");

    if (QL_STOCHASTIC_PROCESS(
            "my_stochastic", 
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

    if (QL_OPTION_VANILLA(
            "my_option",                    // option handle
            "my_stochastic",                // stochastic process handle
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

    printf("\nhigh-level interrogation - after QL_OPTION_VANILLA\n");
    for (i=0; i<vo.count; i++)
        printf("field = %s, value = %s\n", vo.varies[i].Label, 
            variesToString(&vo.varies[i]));

    if (QL_OPTION_SETENGINE(
            "my_option", 
            "AEQPB",   // AdditiveEQPBinomialTree
            801, 
            &vo) != SUCCESS) {
        printf("Error on call to QL_OPTION_SETENGINE\n");
        goto fail;
    }

    printf("\nhigh-level interrogation - after QL_OPTION_SETENGINE\n");
    for (i=0; i<vo.count; i++)
        printf("field = %s, value = %s\n", vo.varies[i].Label, 
            variesToString(&vo.varies[i]));

        // example that takes a vector as input
    fixingDatesCount = exerciseDate - todaysDate + 1;
    fixingDates = (long *) malloc(sizeof(long) * fixingDatesCount);
    for (i = 0; i < fixingDatesCount; i++)
        fixingDates[i] = todaysDate + i;
    if (QL_OPTION_ASIAN_D(
            "my_asian_discrete",            // option handle
            "my_stochastic",                // stochastic process handle
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
            &va) != SUCCESS) {
        printf("Error on call to QL_OPTION_ASIAN_D\n");
        goto fail;
    }

    printf("\nhigh-level interrogation - after QL_OPTION_ASIAN_D\n");
    for (i=0; i<va.count; i++)
        printf("field = %s, value = %s\n", va.varies[i].Label, 
            variesToString(&va.varies[i]));

    freeVariesList(&vbs);
    freeVariesList(&vo);
    freeVariesList(&va);

    QL_LOGMESSAGE("end example program");

    printf("\nbye\n");

    return 0;

fail:

    return 1;
}


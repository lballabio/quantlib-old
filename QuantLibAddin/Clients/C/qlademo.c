
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
    long exerciseDate = 43903;      // (13, March, 2020);
    long settlementDate = 43537;    // (13, March, 2019);
    VariesList vbs;                 // attributes of black scholes object
    VariesList vo;                  // attributes of vanilla option object
    int i;                          // iterator

    QL_LOGFILE("quantlib.log");     // specify log file
    QL_CONSOLE(1);                  // log messages to stdout
    QL_LOGMESSAGE("begin example program");

    QL_LOGMESSAGE(QL_VER());
    QL_LOGMESSAGE(QL_OH_VER());

    if (QL_STOCHASTIC_PROCESS(
            "my_stochastic", 
            underlying, 
            "ACT360",
            settlementDate, 
            riskFreeRate, 
            dividendYield, 
            volatility, 
            &vbs) != SUCCESS) {
        QL_LOGMESSAGE("Error on call to QL_STOCHASTIC_PROCESS");
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
        QL_LOGMESSAGE("Error on call to QL_OPTION_VANILLA");
        goto fail;
    }

    QL_LOGMESSAGE("high-level interrogation - after QL_OPTION_VANILLA");
    for (i=0; i<vo.count; i++)
        QL_LOGMESSAGE("field = %s, value = %s", vo.varies[i].Label, 
            variesToString(&vo.varies[i]));

    if (QL_OPTION_SETENGINE(
            "my_option", 
            "AEQPB",   // AdditiveEQPBinomialTree
            801, 
            &vo) != SUCCESS) {
        QL_LOGMESSAGE("Error on call to QL_OPTION_SETENGINE");
        goto fail;
    }

    QL_LOGMESSAGE("high-level interrogation - after QL_OPTION_SETENGINE");
    for (i=0; i<vo.count; i++)
        QL_LOGMESSAGE("field = %s, value = %s", vo.varies[i].Label, 
            variesToString(&vo.varies[i]));

    freeVariesList(&vbs);
    freeVariesList(&vo);

    QL_LOGMESSAGE("end example program");

    return 0;

fail:

    return 1;
}


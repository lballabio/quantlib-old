
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
    long exerciseDate = 43903;                  // (13, March, 2020);
    long settlementDate = 43537;                // (13, March, 2019);
    char returnString[100];
    Boolean returnCode;
    Varies logLevel;
	logLevel.type = LONG;
	logLevel.data.AsLong = 4;

    ohSetLogFile("quantlib.log", logLevel, &returnCode);     // specify log file
    ohSetConsole(1, logLevel, &returnCode);                  // log messages to stdout
    ohLogMessage("begin example program", logLevel, &returnCode);

    if (qlBlackConstantVol(
            "my_blackconstantvol", 
            settlementDate, 
            volatility, 
            "Actual360",
            returnString) != SUCCESS) {
        ohLogMessage("Error on call to qlBlackConstantVol", logLevel, &returnCode);
        goto fail;
    }

    if (qlBlackScholesProcess(
            "my_stochastic", 
            "my_blackconstantvol", 
            underlying, 
            "Actual360",
            settlementDate, 
            riskFreeRate, 
            dividendYield, 
            returnString) != SUCCESS) {
        ohLogMessage("Error on call to qlBlackScholesProcess", logLevel, &returnCode);
        goto fail;
    }

    if (qlVanillaOption(
            "my_option",                    // option handle
            "my_stochastic",                // stochastic process handle
            "Put",                          // option type
            "Vanilla",                      // payoff type
            strike,                         // strike price
            "American",                     // exercise type
            exerciseDate,                   // exercise date
            settlementDate,                 // settlement date
            "JR",                           // engine type (jarrow rudd)
            timeSteps,                      // time steps
            returnString) != SUCCESS) {
        ohLogMessage("Error on call to qlVanillaOption", logLevel, &returnCode);
        goto fail;
    }

    ohLogMessage("high-level interrogation - after qlVanillaOption", logLevel, &returnCode);
    ohLogObject("my_option", &returnCode);

    if (qlOptionSetEngine(
            "my_option", 
            "AEQPB",   // AdditiveEQPBinomialTree
            801, 
            returnString) != SUCCESS) {
        ohLogMessage("Error on call to qlOptionSetEngine", logLevel, &returnCode);
        goto fail;
    }

    ohLogMessage("high-level interrogation - after qlOptionSetEngine", logLevel, &returnCode);
    ohLogObject("my_option", &returnCode);

    ohLogMessage("end example program", logLevel, &returnCode);

    return 0;

fail:

    return 1;
}


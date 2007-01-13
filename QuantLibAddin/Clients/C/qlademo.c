
/*
 Copyright (C) 2004, 2005, 2006 Eric Ehlers

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
    double npv = 0;
    long evaluationDate = 35930;    // 15 May 1998
    long settlementDate = 35932;    // 17 May 1998
    long exerciseDate = 36297;      // 17 May 1999
    char returnString[100];
    long logLevel = 4;
    Boolean result;
    long dummy2;
    // dummy value for trigger parameter, which we are ignoring
    Varies dummy;
	dummy.type = LONG;
	dummy.data.AsLong = 0;

    initialize();
    ohSetLogFile("quantlib.log", logLevel, dummy, returnString);   // specify log file
    ohSetConsole(1, logLevel, dummy, &result);                     // log messages to stdout
    ohLogMessage("begin example program", logLevel, dummy, &result);

    if (qlSetEvaluationDate(
            evaluationDate, 
            dummy,
            &dummy2) != SUCCESS) {
        ohLogMessage("Error on call to qlSetEvaluationDate", logLevel, dummy, &result);
        goto fail;
    }

    if (qlBlackConstantVol(
            "my_blackconstantvol", 
            settlementDate, 
            volatility, 
            "Actual/365 (Fixed)",
            FALSE,
            dummy,
            returnString) != SUCCESS) {
        ohLogMessage("Error on call to qlBlackConstantVol", logLevel, dummy, &result);
        goto fail;
    }

    if (qlGeneralizedBlackScholesProcess(
            "my_stochastic", 
            "my_blackconstantvol", 
            underlying, 
            "Actual/365 (Fixed)",
            settlementDate, 
            riskFreeRate, 
            dividendYield, 
            FALSE,
            dummy,
            returnString) != SUCCESS) {
        ohLogMessage("Error on call to qlGeneralizedBlackScholesProcess", logLevel, dummy, &result);
        goto fail;
    }

    if (qlEuropeanExercise(
            "my_exercise", 
            exerciseDate, 
            FALSE, 
            dummy,
            returnString) != SUCCESS) {
        ohLogMessage("Error on call to qlEuropeanExercise", logLevel, dummy, &result);
        goto fail;
    }

    if (qlStrikedTypePayoff(
            "my_payoff", 
            "vanilla", 
            "put",
            strike,
            FALSE, 
            dummy,
            returnString) != SUCCESS) {
        ohLogMessage("Error on call to qlStrikedTypePayoff", logLevel, dummy, &result);
        goto fail;
    }

    if (qlPricingEngine(
            "my_engine", 
            "AE",       // analytic european
            FALSE, 
            dummy,
            returnString) != SUCCESS) {
        ohLogMessage("Error on call to qlPricingEngine", logLevel, dummy, &result);
        goto fail;
    }

    if (qlVanillaOption(
            "my_option",                    // option object ID
            "my_stochastic",                // stochastic process object ID
            "my_payoff",                    // payoff object ID
            "my_exercise",                  // exercise object ID
            "my_engine",                    // engine object ID
            FALSE,                          // time steps
            dummy,
            returnString) != SUCCESS) {
        ohLogMessage("Error on call to qlVanillaOption", logLevel, dummy, &result);
        goto fail;
    }

    if (qlNPV(
            "my_option",
            dummy,
            &npv) != SUCCESS) {
        ohLogMessage("Error on call to qlNPV", logLevel, dummy, &result);
        goto fail;
    }

    // log the NPV
    sprintf(returnString, "the option npv is %f", npv);
    ohLogMessage(returnString, logLevel, dummy, &result);

    // dump the object to the log file
    ohLogObject("my_option", dummy, &result);

    ohLogMessage("end example program", logLevel, dummy, &result);

    return 0;

fail:

    return 1;
}


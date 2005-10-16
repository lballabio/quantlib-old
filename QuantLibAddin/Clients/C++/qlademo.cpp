
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

#include <qla/qladdin.hpp>
#include <sstream>
#include <iostream>

using namespace std;
using namespace QuantLib;
using namespace ObjHandler;
using namespace QuantLibAddin;

int main() {
    try {
        setLogFile("quantlib.log");         // specify log file
        setConsole(1);                      // log messages to stdout
        logMessage("begin example program");
    } catch (const exception &e) {
        cout << "Unable to initialize logging: " << e.what() << endl;
        return 1;
    } catch (...) {
        cout << "Unable to initialize logging." << endl;
        return 1;
    }

    try {
        logMessage(qlVersion());
        logMessage(OBJHANDLER_VERSION);

        double dividendYield = 0.00;
        double riskFreeRate = 0.06;
        double volatility = 0.20;
        double underlying = 36;
        double strike = 40;
        long timeSteps = 801;
        Date exerciseDate(13, March, 2020);
        Date settlementDate(13, March, 2019);

        obj_ptr blackConstantVol(new QuantLibAddin::BlackConstantVol(
            settlementDate.serialNumber(),  // settlement date as long
            volatility,                     // volatility
            "Actual360"));                  // daycount convention
        storeObject("my_blackconstantvol", blackConstantVol);

        obj_ptr blackScholesProcess(new QuantLibAddin::BlackScholesProcess(
            "my_blackconstantvol",          // black constant vol handle
            underlying,                     // underlying
            "Actual360",                    // daycount convention
            settlementDate.serialNumber(),  // settlement date as long
            riskFreeRate,                   // risk free rate
            dividendYield));                // dividend yield
        storeObject("my_blackscholes", blackScholesProcess);

        obj_ptr vanillaOption(new QuantLibAddin::VanillaOption(
            "my_blackscholes",              // stochastic process handle
            "Put",                          // option type
            "Vanilla",                      // payoff type
            strike,                         // strike price
            "American",                     // exercise type
            exerciseDate.serialNumber(),    // exercise date
            settlementDate.serialNumber(),  // settlement date
            "JR",                           // engine type (jarrow rudd)
            timeSteps));                    // time steps
        storeObject("my_option", vanillaOption);
    
        logMessage("High-level interrogation of VanillaOption");
        logObject("my_option");

        boost::shared_ptr<QuantLibAddin::VanillaOption> vanillaOptionQLA =
            OH_GET_OBJECT(QuantLibAddin::VanillaOption, "my_option");
        if (!vanillaOptionQLA)
            QL_FAIL("Error retrieving object my_option");

        vanillaOptionQLA->setEngine(
            "AEQPB",    // AdditiveEQPBinomialTree
            timeSteps);

        logMessage("High-level interrogation: after setting engine");
        logObject("my_option");

        logMessage("Low-level interrogation: NPV of underlying option object");
        boost::shared_ptr<QuantLib::VanillaOption> vanillaOptionQL = 
            OH_GET_REFERENCE(QuantLib::VanillaOption, vanillaOptionQLA);
        ostringstream s;
        s << "underlying option NPV() = " << vanillaOptionQL->NPV();
        logMessage(s.str());

        logMessage("end example program");

        return 0;
    } catch (const exception &e) {
        ostringstream s;
        s << "Error: " << e.what();
        logMessage(s.str(), 1);
        return 1;
    } catch (...) {
        logMessage("unknown error", 1);
        return 1;
    }
}


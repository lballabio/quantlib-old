
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
        OH_LOGFILE("quantlib.log");         // specify log file
        OH_CONSOLE(1);                      // log messages to stdout
        OH_LOG_MESSAGE("begin example program");
    } catch (const exception &e) {
        cout << "Unable to initialize logging: " << e.what() << endl;
        return 1;
    } catch (...) {
        cout << "Unable to initialize logging." << endl;
        return 1;
    }

    try {
        OH_LOG_MESSAGE(QL_VER());
        OH_LOG_MESSAGE(QL_OH_VER());

        double dividendYield = 0.00;
        double riskFreeRate = 0.06;
        double volatility = 0.20;
        double underlying = 36;
        double strike = 40;
        long timeSteps = 801;
        Date exerciseDate(13, March, 2020);
        Date settlementDate(13, March, 2019);

        ArgumentStack bcArgs;
        bcArgs.push(settlementDate.serialNumber()); // settlement date as long
        bcArgs.push(volatility);            // volatility
        bcArgs.push(string("Actual360"));   // daycount convention
        Properties bcProperties =
            OH_MAKE_OBJECT(QuantLibAddin::BlackConstantVol, "my_blackconstantvol", bcArgs);

        ArgumentStack bsArgs;
        bsArgs.push(string("my_blackconstantvol")); // black constant vol handle
        bsArgs.push(underlying);            // underlying
        bsArgs.push(string("Actual360"));   // daycount convention
        bsArgs.push(settlementDate.serialNumber()); // settlement date as long
        bsArgs.push(riskFreeRate);          // risk free rate
        bsArgs.push(dividendYield);         // dividend yield
        Properties bsProperties =
            OH_MAKE_OBJECT(QuantLibAddin::BlackScholesProcess, "my_stochastic", bsArgs);

        ArgumentStack opArgs;
        opArgs.push(string("my_stochastic")); // stochastic process handle
        opArgs.push(string("Put"));         // option type
        opArgs.push(string("Vanilla"));     // payoff type
        opArgs.push(strike);                // strike price
        opArgs.push(string("American"));    // exercise type
        opArgs.push(exerciseDate.serialNumber()); // exercise date
        opArgs.push(settlementDate.serialNumber()); // settlement date
        opArgs.push(string("JR"));          // engine type (jarrow rudd)
        opArgs.push(timeSteps);             // time steps
        OH_MAKE_OBJECT(QuantLibAddin::VanillaOption, "my_option", opArgs);
    
        OH_LOG_MESSAGE("High-level interrogation: after QL_OPTION_VANILLA");
        OH_LOG_OBJECT("my_option");

        QL_OPTION_SETENGINE(
            "my_option", 
            "AEQPB",    // AdditiveEQPBinomialTree
            timeSteps);

        OH_LOG_MESSAGE("High-level interrogation: after QL_OPTION_SETENGINE");
        OH_LOG_OBJECT("my_option");

        OH_LOG_MESSAGE("Low-level interrogation: NPV of underlying option object");
        boost::shared_ptr<QuantLibAddin::VanillaOption> vanillaOptionQLA = 
            boost::dynamic_pointer_cast<QuantLibAddin::VanillaOption> 
            (ObjectHandler::instance().retrieveObject("my_option"));
        boost::shared_ptr<QuantLib::VanillaOption> const vanillaOptionQL =
            boost::static_pointer_cast<QuantLib::VanillaOption>
            (vanillaOptionQLA->getReference());
        ostringstream s;
        s << "underlying option NPV() = " << vanillaOptionQL->NPV();
        OH_LOG_MESSAGE(s.str());

        OH_LOG_MESSAGE("end example program");
        return 0;
    } catch (const exception &e) {
        ostringstream s;
        s << "Error: " << e.what();
        OH_LOG_MESSAGE(s.str(), 1);
        return 1;
    } catch (...) {
        OH_LOG_MESSAGE("unknown error", 1);
        return 1;
    }
}


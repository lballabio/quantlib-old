
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

using namespace std;
using namespace QuantLib;
using namespace ObjHandler;
using namespace QuantLibAddin;

int main() {
    try {
        QL_LOGFILE("quantlib.log");         // specify log file
        QL_CONSOLE(1);                      // log messages to stdout
        QL_LOGMESSAGE("begin example program");

        QL_LOGMESSAGE(QL_VER());
        QL_LOGMESSAGE(QL_OH_VER());

        double dividendYield = 0.00;
        double riskFreeRate = 0.06;
        double volatility = 0.20;
        double underlying = 36;
        double strike = 40;
        long timeSteps = 801;
        Date exerciseDate(13, March, 2020);
        Date settlementDate(13, March, 2019);

        ArgStack bsArgs;
        bsArgs.push(underlying);            // underlying
        bsArgs.push(string("ACT360"));      // daycount convention
        bsArgs.push(settlementDate.serialNumber()); // settlement date as long
        bsArgs.push(riskFreeRate);          // risk free rate
        bsArgs.push(dividendYield);         // dividend yield
        bsArgs.push(volatility);            // volatility
        Properties bsProperties =
            QL_OBJECT_MAKE(StochasticProcess)("my_stochastic", bsArgs);

        ArgStack opArgs;
        opArgs.push(string("my_stochastic")); // stochastic process handle
        opArgs.push(string("PUT"));         // option type
        opArgs.push(string("VAN"));         // payoff type (plain vanilla)
        opArgs.push(strike);                // strike price
        opArgs.push(string("AM"));          // exercise type (american)
        opArgs.push(exerciseDate.serialNumber()); // exercise date
        opArgs.push(settlementDate.serialNumber()); // settlement date
        opArgs.push(string("JR"));          // engine type (jarrow rudd)
        opArgs.push(timeSteps);             // time steps
        Properties opProperties =
            QL_OBJECT_MAKE(VanillaOption)("my_option", opArgs);
    
        QL_LOGMESSAGE("High-level interrogation: after QL_OPTION_VANILLA");
        Properties::const_iterator it;
        for (it = opProperties.begin();
            it != opProperties.end(); it++) {
            ObjectProperty property = *it;
            ostringstream s;
            s << "property = " << property.name() 
                << "\tvalue = " << property();
            QL_LOGMESSAGE(s.str());
        } 

        QL_OPTION_SETENGINE(
            "my_option", 
            "AEQPB",    // AdditiveEQPBinomialTree
            timeSteps);

        QL_LOGMESSAGE("High-level interrogation: after QL_OPTION_SETENGINE");
        for (it = opProperties.begin();
            it != opProperties.end(); it++) {
            ObjectProperty property = *it;
            ostringstream s;
            s << "property = " << property.name() 
                << "\tvalue = " << property();
            QL_LOGMESSAGE(s.str());
        } 

        QL_LOGMESSAGE("Low-level interrogation: NPV of underlying option object");
        boost::shared_ptr<QuantLibAddin::VanillaOption> vanillaOptionQLA = 
            boost::dynamic_pointer_cast<QuantLibAddin::VanillaOption> 
            (ObjectHandler::instance().retrieveObject("my_option"));
        boost::shared_ptr<QuantLib::VanillaOption> const vanillaOptionQL =
            boost::static_pointer_cast<QuantLib::VanillaOption>
            (vanillaOptionQLA->getReference());
        ostringstream s;
        s << "underlying option NPV() = " << vanillaOptionQL->NPV();
        QL_LOGMESSAGE(s.str());

        QL_LOGMESSAGE("end example program");
        return 0;
    } catch (const exception &e) {
        ostringstream s;
        s << "Error: " << e.what();
        QL_LOGMESSAGE(s.str(), 1);
        return 1;
    } catch (...) {
        QL_LOGMESSAGE("unknown error", 1);
        return 1;
    }
}


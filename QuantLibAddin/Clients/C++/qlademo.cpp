
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
#include <qla/objects/vanillaoption.hpp>
#include <iostream>

using namespace std;
using namespace QuantLib;
using namespace ObjHandler;
using namespace QuantLibAddin;

int main() {
    try {
        cout << "hi" << endl;

        QL_LOGFILE("quantlib.log");
        QL_LOGMESSAGE("begin example program");

        cout << QL_VER() << endl;
        cout << QL_OH_VER() << endl;

        double dividendYield = 0.00;
        double riskFreeRate = 0.06;
        double volatility = 0.20;
        double underlying = 36;
        double strike = 40;
        long timeSteps = 801;
        Date exerciseDate(17, May, 1999);
        Date settlementDate(17, May, 1998);
        Date todaysDate(15, May, 1998);

        Properties bsProperties = QL_STOCHASTIC_PROCESS(
            "my_stochastic", 
            underlying,
            "ACT360",
            settlementDate.serialNumber(),
            riskFreeRate,
            dividendYield, 
            volatility);

        Properties opProperties = QL_OPTION_VANILLA(
            "my_option",                    // option handle
            "my_stochastic",                // stochastic process handle
            "PUT",                          // option type
            "VAN",                          // payoff type (plain vanilla)
            strike,                         // strike price
            "AM",                           // exercise type (american)
            exerciseDate.serialNumber(),    // exercise date
            settlementDate.serialNumber(),  // settlement date
            "JR",                           // engine type (jarrow rudd)
            timeSteps);                     // time steps
    
        cout << endl << "High-level interrogation: after QL_OPTION_VANILLA" << endl;
        Properties::const_iterator it;
        for (it = opProperties.begin();
            it != opProperties.end(); it++) {
            ObjectProperty property = *it;
            cout << "property = " << property.name() 
                << "\tvalue = " << property() << endl;
        } 

        QL_OPTION_SETENGINE(
            "my_option", 
            "AEQPB",    // AdditiveEQPBinomialTree
            timeSteps);

        cout << endl << "High-level interrogation: after QL_OPTION_SETENGINE" << endl;
        for (it = opProperties.begin();
            it != opProperties.end(); it++) {
            ObjectProperty property = *it;
            cout << "property = " << property.name() 
                << "\tvalue = " << property() << endl;
        } 

        cout << endl << "Low-level interrogation: NPV of underlying option object" << endl;
        boost::shared_ptr<QuantLibAddin::VanillaOption> vanillaOptionQLA = 
            boost::dynamic_pointer_cast<QuantLibAddin::VanillaOption> 
            (ObjectHandler::instance().retrieveObject("my_option"));
        boost::shared_ptr<QuantLib::VanillaOption> const vanillaOptionQL =
            boost::static_pointer_cast<QuantLib::VanillaOption>
            (vanillaOptionQLA->getReference());
        cout << "underlying option NPV() = " << vanillaOptionQL->NPV() << endl;

        // example that takes a vector as input
        std::vector<long> fixingDates(exerciseDate - todaysDate + 1);
        for (Size i=0; i<fixingDates.size(); i++)
            fixingDates[i] = todaysDate.serialNumber() + i;
        Properties asProperties = QL_OPTION_ASIAN_D(
            "my_asian_discrete",            // option handle
            "my_stochastic",                // stochastic process handle
            "G",                            // average type ("A"verage/"G"eometric)
            1.0,                            // running accumulator
            0,                              // past fixings
            fixingDates,                    // fixingDates
            "PUT",                          // option type
            "VAN",                          // payoff type (plain vanilla)
            strike,                         // strike price
            "EU",                           // exercise type (american)
            exerciseDate.serialNumber(),    // exercise date
            0,                              // settlement date ignored when exercise = European
            "ADGAPA",                       // engine type (AnalyticDiscreteGeometricAveragePriceAsianEngine)
            timeSteps);                     // time steps

        cout << endl << "High-level interrogation: after QL_OPTION_ASIAN_D" << endl;
        for (it = asProperties.begin();
            it != asProperties.end(); it++) {
            ObjectProperty property = *it;
            cout << "property = " << property.name() 
                << "\tvalue = " << property() << endl;
        } 

        QL_LOGMESSAGE("end example program");
        cout << endl << "bye" << endl;
        return 0;
    } catch (const exception &e) {
        cout << "Error: " << e.what() << endl;
        QL_LOGMESSAGE(e.what(), 2);
        return 1;
    } catch (...) {
        cout << "unknown error" << endl;
        QL_LOGMESSAGE("unknown error", 2);
        return 1;
    }
}



/*
 Copyright (C) 2004, 2005, 2006, 2007 Eric Ehlers

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it
 under the terms of the QuantLib license.  You should have received a
 copy of the license along with this program; if not, please email
 <quantlib-dev@lists.sf.net>. The license is also available online at
 <http://quantlib.org/license.shtml>.

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
*/

#include <oh/objecthandler.hpp>
#include <ql/quantlib.hpp>
#include <qlo/qladdin.hpp>
#include <sstream>
#include <iostream>

#ifdef BOOST_MSVC
#  define BOOST_LIB_DIAGNOSTIC
#  include <oh/auto_link.hpp>
#  include <qlo/qladdindefines.hpp>
#  include <qlo/auto_link.hpp>
#  undef BOOST_LIB_DIAGNOSTIC
#endif

using namespace std;
using namespace QuantLib;
using namespace ObjectHandler;
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
        // Instantiate the ObjectHandler Repository
        ObjectHandler::Repository oh;

		// Initialize the Enumeration Registry
		registerEnumerations();

        logMessage(qlVersion());
        logMessage(OBJHANDLER_VERSION);

        double dividendYield = 0.00;
        double riskFreeRate = 0.06;
        double volatility = 0.20;
        double underlying = 36;
        double strike = 40;
        Date evaluationDate(15, May, 1998);
        Date settlementDate(17, May, 1998);
        Date exerciseDate(17, May, 1999);

        QuantLibAddin::qlSettingsSetEvaluationDate(evaluationDate);

        boost::shared_ptr<Object> blackConstantVol(
            new QuantLibAddin::BlackConstantVol(
                settlementDate,                 // settlement date as long
                volatility,                     // volatility
                Actual365Fixed()));             // daycount convention
        ObjectHandler::Repository::instance().storeObject(
            "my_blackconstantvol", blackConstantVol);

        OH_GET_REFERENCE(blackvolRef, "my_blackconstantvol",
            QuantLibAddin::BlackVolTermStructure, QuantLib::BlackVolTermStructure)

        boost::shared_ptr<Object> blackScholesProcess(
            new QuantLibAddin::GeneralizedBlackScholesProcess(
                blackvolRef,                    // black constant vol object ID
                underlying,                     // underlying
                Actual365Fixed(),               // daycount convention
                settlementDate,                 // settlement date as long
                riskFreeRate,                   // risk free rate
                dividendYield));                // dividend yield
        ObjectHandler::Repository::instance().storeObject(
            "my_blackscholes", blackScholesProcess);

        boost::shared_ptr<Object> exercise(
            new QuantLibAddin::EuropeanExercise(
                exerciseDate));                 // exercise date
        ObjectHandler::Repository::instance().storeObject(
            "my_exercise", exercise);

        boost::shared_ptr<ObjectHandler::Object> payoff(
            new QuantLibAddin::StrikedTypePayoff(
                "vanilla",
                Option::Put,
                strike, // "thirdParameter" ?
                strike));
        ObjectHandler::Repository::instance().storeObject(
            "my_payoff", payoff);

        boost::shared_ptr<Object> engine(
            new QuantLibAddin::PricingEngine(
                "AE"));                         // engine ID (Analytic European)
        ObjectHandler::Repository::instance().storeObject(
            "my_engine", engine);

        OH_GET_REFERENCE(blackscholesRef, "my_blackscholes",
            QuantLibAddin::GeneralizedBlackScholesProcess, 
            QuantLib::GeneralizedBlackScholesProcess)

        OH_GET_REFERENCE(payoffRef, "my_payoff",
            QuantLibAddin::StrikedTypePayoff, QuantLib::StrikedTypePayoff)

        OH_GET_REFERENCE(exerciseRef, "my_exercise",
            QuantLibAddin::EuropeanExercise, QuantLib::Exercise)

        OH_GET_REFERENCE(engineRef, "my_engine",
            QuantLibAddin::PricingEngine, QuantLib::PricingEngine)

        boost::shared_ptr<Object> vanillaOption(new QuantLibAddin::VanillaOption(
            blackscholesRef,                    // black scholes object
            payoffRef,                          // payoff object
            exerciseRef,                        // exercise object
            engineRef));                        // engine object
        ObjectHandler::Repository::instance().storeObject("my_option", vanillaOption);

        vanillaOption->setProperties(boost::shared_ptr<ObjectHandler::ValueObject>(new QuantLibAddin::ValueObjects::qlVanillaOption(
            "my_option",                        // object ID
            "my_blackscholes",                  // stochastic process object ID
            "my_payoff",                        // option type
            "my_exercise",                      // payoff type
            "my_engine")));                     // time steps

        logMessage("High-level interrogation of VanillaOption");
        logObject("my_option");

        logMessage("Low-level interrogation: NPV of underlying option object");
        OH_GET_REFERENCE(optionRef, "my_option",
            QuantLibAddin::VanillaOption, QuantLib::VanillaOption)
        ostringstream s;
        s << "underlying option NPV() = " << optionRef->NPV();
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


#ifdef QL_ENABLE_SESSIONS

QuantLib::Integer QuantLib::sessionId() {
    return 0;
}

#endif

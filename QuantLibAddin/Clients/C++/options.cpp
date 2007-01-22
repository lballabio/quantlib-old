
/*
 Copyright (C) 2005, 2006 Eric Ehlers

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

// test program for options

#include <qla/qladdin.hpp>
#include <sstream>
#include <iomanip>

using namespace std;
using namespace QuantLib;
using namespace ObjHandler;
using namespace QuantLibAddin;

int main() {
    try {
        setLogFile("quantlib.log");
        setConsole(1);
        logMessage("begin options test");

        double dividendYield = 0.00;
        double riskFreeRate = 0.06;
        double volatility = 0.20;
        double underlying = 36;
        double strike = 50;
        long timeSteps = 801;
        Date exerciseDate(17, May, 1999);
        Date settlementDate(17, May, 1998);
        Date todaysDate(15, May, 1998);
        QuantLib::setEvaluationDate(todaysDate);

        obj_ptr blackConstantVol(new QuantLibAddin::BlackConstantVol(
            settlementDate.serialNumber(),      // settlement date as long
            volatility,                         // volatility
            "Actual365Fixed"));                 // daycount convention
        storeObject("my_blackconstantvol", blackConstantVol);

        obj_ptr blackScholesProcess(new QuantLibAddin::GeneralizedBlackScholesProcess(
            "my_blackconstantvol",              // black constant vol object ID
            underlying,                         // underlying
            "Actual365Fixed",                   // daycount convention
            settlementDate.serialNumber(),      // settlement date as long
            riskFreeRate,                       // risk free rate
            dividendYield));                    // dividend yield
        storeObject("my_blackscholesprocess", blackScholesProcess);
        logObject("my_blackscholesprocess");

        obj_ptr exercise(new QuantLibAddin::EuropeanExercise(
            settlementDate.serialNumber()));    // settlement date
        storeObject("my_exercise", exercise);

        obj_ptr vanillaOption(new QuantLibAddin::VanillaOption(
            "my_blackscholesprocess",           // stochastic process object ID
            "Put",                              // option type
            "Vanilla",                          // payoff type
            strike,                             // strike price
            "my_exercise",                      // exercise object ID
            "AE",                               // engine type (analytic european)
            timeSteps));                        // time steps
        storeObject("my_vanillaOption", vanillaOption);
        vanillaOption->setProperties(boost::shared_ptr<ObjHandler::ValueObject>(new QuantLibAddin::ValueObjects::qlVanillaOption(
            "my_vanillaOption",                 // object ID
            "my_blackscholesprocess",           // stochastic process object ID
            "Put",                              // option type
            "Vanilla",                          // payoff type
            strike,                             // strike price
            "my_exercise",                      // exercise object ID
            "AE",                               // engine type (analytic european)
            timeSteps)));                       // time steps
        logObject("my_vanillaOption");

        double x = boost::dynamic_pointer_cast<QuantLibAddin::VanillaOption>
                (vanillaOption)->getObject().NPV();
        std::ostringstream msg;
        msg << "NPV: " << std::setprecision(12) << std::fixed << x << endl;
        ObjHandler::logMessage(msg.str());

        obj_ptr continuousAveragingAsianOption(new QuantLibAddin::ContinuousAveragingAsianOption(
            "Geometric",                        // average type
            "my_blackscholesprocess",           // stochastic process object ID
            "Put",                              // option type
            "Vanilla",                          // payoff type
            strike,                             // strike price
            "my_exercise",                      // exercise object ID
            "ACGAPA",                           // engine type (AnalyticContinuousGeometricAveragePriceAsianEngine)
            timeSteps));                        // time steps
        storeObject("my_continuous", continuousAveragingAsianOption);
        logObject("my_continuous");

        vector < long > fixingDates;
        for (int i = 0; i < exerciseDate - todaysDate + 1; ++i)
            fixingDates.push_back(todaysDate.serialNumber() + i);
        obj_ptr discreteAveragingAsianOption(new QuantLibAddin::DiscreteAveragingAsianOption(
            "Geometric",                        // average type
            1.0,                                // running accumulator
            0,                                  // past fixings
            fixingDates,                        // fixingDates
            "my_blackscholesprocess",           // stochastic process object ID
            "Put",                              // option type
            "Vanilla",                          // payoff type
            strike,                             // strike price
            "my_exercise",                      // exercise object ID
            "ADGAPA",                           // engine type (AnalyticDiscreteGeometricAveragePriceAsianEngine)
            timeSteps));                        // time steps
        storeObject("my_discrete", discreteAveragingAsianOption);
        logObject("my_discrete");

        obj_ptr barrierOption(new QuantLibAddin::BarrierOption(
            "DownIn",                           // barrier type
            35.0,                               // barrier
            3.0,                                // rebate
            "my_blackscholesprocess",           // stochastic process object ID
            "Put",                              // option type
            "Vanilla",                          // payoff type
            strike,                             // strike price
            "my_exercise",                      // exercise object ID
            "AB",                               // engine type (AnalyticBarrierEngine)
            timeSteps));                        // time steps
        storeObject("my_barrierOption", barrierOption);
        logObject("my_barrierOption");

        vector < long > resetDates;
        resetDates.push_back(Date(12, March, 2020).serialNumber());
        obj_ptr cliquetOption(new QuantLibAddin::CliquetOption(
            "my_blackscholesprocess",           // stochastic process object ID
            "Put",                              // option type
            strike,                             // strike price
            exerciseDate.serialNumber(),        // exercise date
            resetDates,                         // reset dates
            "AC",                               // engine type (AnalyticCliquetEngine)
            timeSteps));                        // time steps
        storeObject("my_cliquetOption", cliquetOption);
        logObject("my_cliquetOption");

        vector < long > dividendDates;
        dividendDates.push_back(Date(13, September, 2019).serialNumber());
        dividendDates.push_back(Date(13, March, 2020).serialNumber());
        vector < double >dividends;
        dividends.push_back(5.);
        dividends.push_back(5.);

        obj_ptr dividendVanillaOption(new QuantLibAddin::DividendVanillaOption(
            "my_blackscholesprocess",           // stochastic process object ID
            "Call",                             // option type
            "Vanilla",                          // payoff type
            10.0,                               // strike price
            "my_exercise",                      // exercise object ID
            dividendDates,                      // dividend dates
            dividends,                          // dividends
            "ADE",                              // engine type (AnalyticDividendEuropeanEngine)
            timeSteps));                        // time steps
        storeObject("my_dividendVanillaOption", dividendVanillaOption);
        logObject("my_dividendVanillaOption");

        long resetDate = exerciseDate.serialNumber() - 90;

        obj_ptr forwardVanillaOption(new QuantLibAddin::ForwardVanillaOption(
            12,                                 // moneyness
            resetDate,                          // reset date
            "my_blackscholesprocess",           // stochastic process object ID
            "Put",                              // option type
            "Vanilla",                          // payoff type (plain vanilla)
            strike,                             // strike price
            "my_exercise",                      // exercise object ID
            "FE",                               // engine type (ForwardEngine)
            timeSteps));                        // time steps
        storeObject("my_forwardVanillaOption", forwardVanillaOption);
        logObject("my_forwardVanillaOption");

        logMessage("end options test");
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


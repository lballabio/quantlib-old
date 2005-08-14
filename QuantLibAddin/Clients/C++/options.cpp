
/*
 Copyright (C) 2005 Eric Ehlers

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
        double strike = 40;
        long timeSteps = 801;
        Date exerciseDate(13, March, 2020);
        Date settlementDate(13, March, 2019);
        Date todaysDate(13, March, 2005);

        obj_ptr blackConstantVol(new QuantLibAddin::BlackConstantVol(
            settlementDate.serialNumber(),      // settlement date as long
            volatility,                         // volatility
            "Actual360"));                      // daycount convention
        storeObject("my_blackconstantvol", blackConstantVol);

        obj_ptr blackScholesProcess(new QuantLibAddin::BlackScholesProcess(
            "my_blackconstantvol",              // black constant vol handle
            underlying,                         // underlying
            "Actual360",                        // daycount convention
            settlementDate.serialNumber(),      // settlement date as long
            riskFreeRate,                       // risk free rate
            dividendYield));                    // dividend yield
        storeObject("my_blackscholesprocess", blackScholesProcess);
        logObject("my_blackscholesprocess");

        obj_ptr vanillaOption(new QuantLibAddin::VanillaOption(
            "my_blackscholesprocess",           // stochastic process handle
            "Put",                              // option type
            "Vanilla",                          // payoff type
            strike,                             // strike price
            "American",                         // exercise type
            exerciseDate.serialNumber(),        // exercise date
            settlementDate.serialNumber(),      // settlement date
            "JR",                               // engine type (jarrow rudd)
            timeSteps));                        // time steps
        storeObject("my_vanillaOption", vanillaOption);
        logObject("my_vanillaOption");

        obj_ptr continuousAveragingAsianOption(new QuantLibAddin::ContinuousAveragingAsianOption(
            "my_blackscholesprocess",           // stochastic process handle
            "Geometric",                        // average type
            "Put",                              // option type
            "Vanilla",                          // payoff type
            strike,                             // strike price
            "European",                         // exercise type
            exerciseDate.serialNumber(),        // exercise date
            0,                                  // settlement date ignored when exercise = European
            "ACGAPA",                           // engine type (AnalyticContinuousGeometricAveragePriceAsianEngine)
            timeSteps));                        // time steps
        storeObject("my_continuous", continuousAveragingAsianOption);
        logObject("my_continuous");

        vector < long > fixingDates;
        for (int i = 0; i < exerciseDate - todaysDate + 1; i++)
            fixingDates.push_back(todaysDate.serialNumber() + i);
        obj_ptr discreteAveragingAsianOption(new QuantLibAddin::DiscreteAveragingAsianOption(
            "my_blackscholesprocess",           // stochastic process handle
            "Geometric",                        // average type
            1.0,                                // running accumulator
            0,                                  // past fixings
            fixingDates,                        // fixingDates
            "Put",                              // option type
            "Vanilla",                          // payoff type
            strike,                             // strike price
            "European",                         // exercise type
            exerciseDate.serialNumber(),        // exercise date
            0,                                  // settlement date ignored when exercise = European
            "ADGAPA",                           // engine type (AnalyticDiscreteGeometricAveragePriceAsianEngine)
            timeSteps));                        // time steps
        storeObject("my_discrete", discreteAveragingAsianOption);
        logObject("my_discrete");

        obj_ptr barrierOption(new QuantLibAddin::BarrierOption(
            "my_blackscholesprocess",           // stochastic process handle
            "DownIn",                           // barrier type
            35.0,                               // barrier
            3.0,                                // rebate
            "Put",                              // option type
            "Vanilla",                          // payoff type
            strike,                             // strike price
            "European",                         // exercise type
            exerciseDate.serialNumber(),        // exercise date
            0,                                  // settlement date ignored when exercise = European
            "AB",                               // engine type (AnalyticBarrierEngine)
            timeSteps));                        // time steps
        storeObject("my_barrierOption", barrierOption);
        logObject("my_barrierOption");

        vector < string > stochHandles;
        stochHandles.push_back("my_blackscholesprocess");
        stochHandles.push_back("my_blackscholesprocess");
        vector < vector < double > >correlations;
        vector < double > row1, row2;
        row1.push_back(1.0);
        row1.push_back(0.9);
        row2.push_back(0.9);
        row2.push_back(1.0);
        correlations.push_back(row1);
        correlations.push_back(row2);
        obj_ptr basketOption(new QuantLibAddin::BasketOption(
            stochHandles,                       // vector of stochastic process handles
            "Min",                              // basket type
            correlations,                       // correlations matrix
            "Call",                             // option type
            strike,                             // strike price
            "European",                         // exercise type
            exerciseDate.serialNumber(),        // exercise date
            0,                                  // settlement date ignored when exercise = European
            "SE",                               // engine type (StulzEngine)
            timeSteps));                        // time steps
        storeObject("my_basketOption", basketOption);
        logObject("my_basketOption");

        vector < long > resetDates;
        resetDates.push_back(Date(12, March, 2020).serialNumber());
        obj_ptr cliquetOption(new QuantLibAddin::CliquetOption(
            "my_blackscholesprocess",           // stochastic process handle
            resetDates,                         // reset dates
            "Put",                              // option type
            strike,                             // strike price
            exerciseDate.serialNumber(),        // exercise date
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
            "my_blackscholesprocess",           // stochastic process handle
            dividendDates,                      // dividend dates
            dividends,                          // dividends
            "Call",                             // option type
            "Vanilla",                          // payoff type
            10.0,                               // strike price
            "European",                         // exercise type
            exerciseDate.serialNumber(),        // exercise date
            0,                                  // settlement date ignored when exercise = European
            "ADE",                              // engine type (AnalyticDividendEuropeanEngine)
            timeSteps));                        // time steps
        storeObject("my_dividendVanillaOption", dividendVanillaOption);
        logObject("my_dividendVanillaOption");

        long resetDate = exerciseDate.serialNumber() - 90;

        obj_ptr forwardVanillaOption(new QuantLibAddin::ForwardVanillaOption(
            "my_blackscholesprocess",           // stochastic process handle
            12,                                 // moneyness
            resetDate,                          // reset date
            "Put",                              // option type
            "Vanilla",                          // payoff type (plain vanilla)
            strike,                             // strike price
            "European",                         // exercise type
            exerciseDate.serialNumber(),        // exercise date
            0,                                  // settlement date ignored when exercise = European
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


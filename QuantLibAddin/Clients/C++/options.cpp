
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
        OH_LOGFILE("quantlib.log");
        OH_CONSOLE(1);
        OH_LOG_MESSAGE("begin options test");

        double dividendYield = 0.00;
        double riskFreeRate = 0.06;
        double volatility = 0.20;
        double underlying = 36;
        double strike = 40;
        long timeSteps = 801;
        Date exerciseDate(13, March, 2020);
        Date settlementDate(13, March, 2019);
        Date todaysDate(13, March, 2005);

        ArgumentStack a0;
        a0.push(settlementDate.serialNumber()); // settlement date as long
        a0.push(volatility);                    // volatility
        a0.push(string("Actual360"));           // daycount convention
        OH_MAKE_OBJECT(QuantLibAddin::BlackConstantVol, "blackConstantVol", a0);

        ArgumentStack a1;
        a1.push(string("blackConstantVol"));// black constant vol handle
        a1.push(underlying);                // underlying
        a1.push(string("Actual360"));       // daycount convention
        a1.push(settlementDate.serialNumber()); // settlement date as long
        a1.push(riskFreeRate);              // risk free rate
        a1.push(dividendYield);             // dividend yield
        OH_MAKE_OBJECT(QuantLibAddin::BlackScholesProcess, "stochasticProcess", a1);
        OH_LOG_OBJECT("stochasticProcess");

        ArgumentStack a2;
        a2.push(string("stochasticProcess"));// stochastic process handle
        a2.push(string("Put"));             // option type
        a2.push(string("Vanilla"));         // payoff type
        a2.push(strike);                    // strike price
        a2.push(string("American"));        // exercise type
        a2.push(exerciseDate.serialNumber()); // exercise date
        a2.push(settlementDate.serialNumber()); // settlement date
        a2.push(string("JR"));              // engine type (jarrow rudd)
        a2.push(timeSteps);                 // time steps
        OH_MAKE_OBJECT(QuantLibAddin::VanillaOption, "vanillaOption", a2);
        OH_LOG_OBJECT("vanillaOption");

        ArgumentStack a3;
        a3.push(string("stochasticProcess"));// stochastic process handle
        a3.push(string("Geometric"));       // average type
        a3.push(string("Put"));             // option type
        a3.push(string("Vanilla"));         // payoff type
        a3.push(strike);                    // strike price
        a3.push(string("European"));        // exercise type
        a3.push(exerciseDate.serialNumber()); // exercise date
        a3.push(0l);                        // settlement date ignored when exercise = European
        a3.push(string("ACGAPA"));          // engine type (AnalyticContinuousGeometricAveragePriceAsianEngine)
        a3.push(timeSteps);                 // time steps
        OH_MAKE_OBJECT(QuantLibAddin::ContinuousAveragingAsianOption, "continuous", a3);
        OH_LOG_OBJECT("continuous");

        vector < long > fixingDates;
        for (int i = 0; i < exerciseDate - todaysDate + 1; i++)
            fixingDates.push_back(todaysDate.serialNumber() + i);
        ArgumentStack a4;
        a4.push(string("stochasticProcess"));// stochastic process handle
        a4.push(string("Geometric"));       // average type
        a4.push(1.0);                       // running accumulator
        a4.push(0l);                        // past fixings
        a4.push(fixingDates);               // fixingDates
        a4.push(string("Put"));             // option type
        a4.push(string("Vanilla"));         // payoff type
        a4.push(strike);                    // strike price
        a4.push(string("European"));        // exercise type
        a4.push(exerciseDate.serialNumber()); // exercise date
        a4.push(0l);                        // settlement date ignored when exercise = European
        a4.push(string("ADGAPA"));          // engine type (AnalyticDiscreteGeometricAveragePriceAsianEngine)
        a4.push(timeSteps);                 // time steps
        OH_MAKE_OBJECT(QuantLibAddin::DiscreteAveragingAsianOption, "discrete", a4);
        OH_LOG_OBJECT("discrete");

        ArgumentStack a5;
        a5.push(string("stochasticProcess"));// stochastic process handle
        a5.push(string("DownIn"));          // barrier type
        a5.push(35.0);                      // barrier
        a5.push(3.0);                       // rebate
        a5.push(string("Put"));             // option type
        a5.push(string("Vanilla"));         // payoff type
        a5.push(strike);                    // strike price
        a5.push(string("European"));        // exercise type
        a5.push(exerciseDate.serialNumber()); // exercise date
        a5.push(0l);                        // settlement date ignored when exercise = European
        a5.push(string("AB"));              // engine type (AnalyticBarrierEngine)
        a5.push(timeSteps);                 // time steps
        OH_MAKE_OBJECT(QuantLibAddin::BarrierOption, "barrierOption", a5);
        OH_LOG_OBJECT("barrierOption");

        vector < string > stochHandles;
        stochHandles.push_back("stochasticProcess");
        stochHandles.push_back("stochasticProcess");
        vector < vector < double > >correlations;
        vector < double > row1, row2;
        row1.push_back(1.0);
        row1.push_back(0.9);
        row2.push_back(0.9);
        row2.push_back(1.0);
        correlations.push_back(row1);
        correlations.push_back(row2);

        ArgumentStack a6;
        a6.push(stochHandles);              // vector of stochastic process handles
        a6.push(string("Min"));             // basket type
        a6.push(correlations);              // correlations matrix
        a6.push(string("Call"));            // option type
        a6.push(strike);                    // strike price
        a6.push(string("European"));        // exercise type
        a6.push(exerciseDate.serialNumber()); // exercise date
        a6.push(0l);                        // settlement date ignored when exercise = European
        a6.push(string("SE"));              // engine type (StulzEngine)
        a6.push(timeSteps);                 // time steps
        OH_MAKE_OBJECT(QuantLibAddin::BasketOption, "basketOption", a6);
        OH_LOG_OBJECT("basketOption");

        vector < long > resetDates;
        resetDates.push_back(Date(12, March, 2020).serialNumber());
        ArgumentStack a7;
        a7.push(string("stochasticProcess"));// stochastic process handle
        a7.push(resetDates);                // reset dates
        a7.push(string("Put"));             // option type
        a7.push(strike);                    // strike price
        a7.push(exerciseDate.serialNumber()); // exercise date
        a7.push(string("AC"));              // engine type (AnalyticCliquetEngine)
        a7.push(timeSteps);                 // time steps
        OH_MAKE_OBJECT(QuantLibAddin::CliquetOption, "cliquetOption", a7);
        OH_LOG_OBJECT("cliquetOption");

        vector < long > dividendDates;
        dividendDates.push_back(Date(13, September, 2019).serialNumber());
        dividendDates.push_back(Date(13, March, 2020).serialNumber());
        vector < double >dividends;
        dividends.push_back(5.);
        dividends.push_back(5.);

        ArgumentStack a8;
        a8.push(string("stochasticProcess"));          // stochastic process handle
        a8.push(dividendDates);             // dividend dates
        a8.push(dividends);                 // dividends
        a8.push(string("Call"));            // option type
        a8.push(string("Vanilla"));         // payoff type
        a8.push(10.0);                      // strike price
        a8.push(string("European"));        // exercise type
        a8.push(exerciseDate.serialNumber()); // exercise date
        a8.push(0l);                        // settlement date ignored when exercise = European
        a8.push(string("ADE"));             // engine type (AnalyticDividendEuropeanEngine)
        a8.push(timeSteps);                 // time steps
        OH_MAKE_OBJECT(QuantLibAddin::DividendVanillaOption, "dividendVanillaOption", a8);
        OH_LOG_OBJECT("dividendVanillaOption");

        long resetDate = exerciseDate.serialNumber() - 90;

        ArgumentStack a9;
        a9.push(string("stochasticProcess"));          // stochastic process handle
        a9.push(12.);                       // moneyness
        a9.push(resetDate);                 // reset date
        a9.push(string("Put"));             // option type
        a9.push(string("Vanilla"));         // payoff type (plain vanilla)
        a9.push(strike);                    // strike price
        a9.push(string("European"));        // exercise type
        a9.push(exerciseDate.serialNumber()); // exercise date
        a9.push(0l);                        // settlement date ignored when exercise = European
        a9.push(string("FE"));              // engine type (ForwardEngine)
        a9.push(timeSteps);                 // time steps
        OH_MAKE_OBJECT(QuantLibAddin::ForwardVanillaOption, "forwardVanillaOption", a9);
        OH_LOG_OBJECT("forwardVanillaOption");

        OH_LOG_MESSAGE("end options test");
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



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
    
void printObject(const string &s, const Properties &p) {
    QL_LOGMESSAGE("Object properties:");
    Properties::const_iterator it;
    for (it = p.begin(); it != p.end(); it++) {
        ObjectProperty property = *it;
        ostringstream s;
        s << left << "property = " << setw(10) << property.name() <<
            "value = " << property();
        QL_LOGMESSAGE(s.str());
    } 
}

int main() {
    try {
        QL_LOGFILE("quantlib.log");
        QL_CONSOLE(1);
        QL_LOGMESSAGE("begin options test");

        double dividendYield = 0.00;
        double riskFreeRate = 0.06;
        double volatility = 0.20;
        double underlying = 36;
        double strike = 40;
        long timeSteps = 801;
        Date exerciseDate(17, May, 1999);
        Date settlementDate(17, May, 1998);
        Date todaysDate(15, May, 1998);

        Properties p1 = QL_MAKE_OBJECT(StochasticProcess)(
            "stoch1", 
            underlying,
            "ACT360",
            settlementDate.serialNumber(),
            riskFreeRate,
            dividendYield, 
            volatility);

        printObject("QL_STOCHASTIC_PROCESS", p1);

        Properties p2 = QL_MAKE_OBJECT(VanillaOption)(
            "opt_van",                      // option handle
            "stoch1",                       // stochastic process handle
            "PUT",                          // option type
            "VAN",                          // payoff type
            strike,                         // strike price
            "AM",                           // exercise type
            exerciseDate.serialNumber(),    // exercise date
            settlementDate.serialNumber(),  // settlement date
            "JR",                           // engine type
            timeSteps);                     // time steps

        printObject("QL_OPTION_VANILLA", p2);

        Properties p3 = QL_MAKE_OBJECT(ContinuousAveragingAsianOption)(
            "opt_asian_cont",               // option handle
            "stoch1",                       // stochastic process handle
            "G",                            // average type ("A"verage/"G"eometric)
            "PUT",                          // option type
            "VAN",                          // payoff type
            strike,                         // strike price
            "EU",                           // exercise type
            exerciseDate.serialNumber(),    // exercise date
            0,                              // settlement date ignored when exercise = European
            "ACGAPA",                       // engine type
            timeSteps);                     // time steps

        printObject("QL_OPTION_ASIAN_C", p3);


        long fixingDatesCount = exerciseDate - todaysDate + 1;
        long *fixingDates = new long[fixingDatesCount];
        for (int i = 0; i < fixingDatesCount; i++)
            fixingDates[i] = todaysDate.serialNumber() + i;
        Properties p4 = QL_MAKE_OBJECT(DiscreteAveragingAsianOption)(
            "opt_asian_disc",               // option handle
            "stoch1",                       // stochastic process handle
            "G",                            // average type
            1.0,                            // running accumulator
            0,                              // past fixings
            fixingDatesCount,               // fixingDates
            fixingDates,                    // fixingDates
            "PUT",                          // option type
            "VAN",                          // payoff type
            strike,                         // strike price
            "EU",                           // exercise type
            exerciseDate.serialNumber(),    // exercise date
            0,                              // settlement date ignored when exercise = European
            "ADGAPA",                       // engine type
            timeSteps);                     // time steps
        delete [] fixingDates;

        printObject("QL_OPTION_ASIAN_D", p4);

        Properties p5 = QL_MAKE_OBJECT(BarrierOption)(
            "opt_barrier",                  // option handle
            "stoch1",                       // stochastic process handle
            "DOWNIN",                       // barrier type
            35.0,                           // barrier
            3.0,                            // rebate
            "PUT",                          // option type
            "VAN",                          // payoff type
            strike,                         // strike price
            "EU",                           // exercise type
            exerciseDate.serialNumber(),    // exercise date
            0,                              // settlement date ignored when exercise = European
            "AB",                           // engine type
            timeSteps);                     // time steps

        printObject("QL_OPTION_BARRIER", p5);

        char *stochHandles[] = { "stoch1", "stoch1" };
        double **correlations = new double*[2];
        correlations[0] = new double[2];
        correlations[1] = new double[2];
        correlations[0][0] = 1.0;
        correlations[0][1] = 0.9;
        correlations[1][0] = 0.9;
        correlations[1][1] = 1.0;

        Properties p6 = QL_MAKE_OBJECT(BasketOption)(
            "opt_basket",                   // option handle
            2,
            stochHandles,                   // vector of stochastic process handles
            "MIN",                          // basket type
            2,
            2,
            correlations,                   // correlations
            "CALL",                         // option type
            40.0,                           // strike price
            "EU",                           // exercise type
            exerciseDate.serialNumber(),    // exercise date
            0,                              // settlement date ignored when exercise = European
            "SE",                           // engine type
            timeSteps);                     // time steps

        for (int j=0; j<2; j++)
            delete correlations[j];
        delete [] correlations;

        printObject("QL_OPTION_BASKET", p6);

        long resetDates[] = { 36020 };
        Properties p7 = QL_MAKE_OBJECT(CliquetOption)(
            "opt_cliquet",                  // option handle
            "stoch1",                       // stochastic process handle
            1,
            resetDates,                     // reset dates
            "PUT",                          // option type
            strike,                         // strike price
            exerciseDate.serialNumber(),    // exercise date
            "AC",                           // engine type
            timeSteps);                     // time steps

        printObject("QL_OPTION_CLIQUET", p7);

        long dividendDates[] = { 36022, 36206 };
        double dividends[] = { 5., 5. };
        Properties p8 = QL_MAKE_OBJECT(DividendVanillaOption)(
            "opt_divvan",                   // option handle
            "stoch1",                       // stochastic process handle
            2,                              // dividend dates
            dividendDates,                  // dividend dates
            2,                              // dividends
            dividends,                      // dividends
            "CALL",                         // option type
            "VAN",                          // payoff type
            10.0,                           // strike price
            "EU",                           // exercise type
            exerciseDate.serialNumber(),    // exercise date
            0,                              // settlement date ignored when exercise = European
            "ADE",                          // engine type
            timeSteps);                     // time steps

        printObject("QL_OPTION_DIVIDENDVANILLA", p8);

        long resetDate = todaysDate.serialNumber() + 90;
        Properties p9 = QL_MAKE_OBJECT(ForwardVanillaOption)(
            "opt_fwdvan",                   // option handle
            "stoch1",                       // stochastic process handle
            12.,                            // moneyness
            resetDate,                      // reset date
            "PUT",                          // option type
            "VAN",                          // payoff type
            strike,                         // strike price
            "EU",                           // exercise type
            exerciseDate.serialNumber(),    // exercise date
            0,                              // settlement date ignored when exercise = European
            "FE",                           // engine type
            timeSteps);                     // time steps

        printObject("QL_OPTION_FORWARDVANILLA", p9);

        QL_LOGMESSAGE("end options test");
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



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
#include <iostream>

using namespace std;
using namespace QuantLib;
using namespace ObjHandler;
using namespace QuantLibAddin;
    
void printObject(const string &s, const Properties &p) {
    Properties::const_iterator it;
    cout << endl << s << endl;
    for (it = p.begin(); it != p.end(); it++) {
        ObjectProperty property = *it;
        cout << "property = " << property.name() 
            << "\tvalue = " << property() << endl;
    } 
}

int main() {
    try {
        cout << "begin options test" << endl;

        QL_LOGFILE("quantlib.log");
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

        Properties p1 = QL_STOCHASTIC_PROCESS(
            "stoch1", 
            underlying,
            "ACT360",
            settlementDate.serialNumber(),
            riskFreeRate,
            dividendYield, 
            volatility);

        printObject("QL_STOCHASTIC_PROCESS", p1);

        Properties p2 = QL_OPTION_VANILLA(
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

        Properties p3 = QL_OPTION_ASIAN_C(
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

        std::vector<long> fixingDates(exerciseDate - todaysDate + 1);
        for (Size i=0; i<fixingDates.size(); i++)
            fixingDates[i] = todaysDate.serialNumber() + i;
        Properties p4 = QL_OPTION_ASIAN_D(
            "opt_asian_disc",               // option handle
            "stoch1",                       // stochastic process handle
            "G",                            // average type
            1.0,                            // running accumulator
            0,                              // past fixings
            fixingDates,                    // fixingDates
            "PUT",                          // option type
            "VAN",                          // payoff type
            strike,                         // strike price
            "EU",                           // exercise type
            exerciseDate.serialNumber(),    // exercise date
            0,                              // settlement date ignored when exercise = European
            "ADGAPA",                       // engine type
            timeSteps);                     // time steps

        printObject("QL_OPTION_ASIAN_D", p4);

        Properties p5 = QL_OPTION_BARRIER(
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

        std::vector< std::string > v;
        v.push_back("stoch1");
        v.push_back("stoch1");
        std::vector < std::vector < double > > correlations;
        for (int i2=0;i2<2;i2++) {
            std::vector < double > v;
            for (int j2=0;j2<2;j2++)
                v.push_back(0.9);
            correlations.push_back(v);
        }
        for (int i3=0;i3<2;i3++)
            correlations[i3][i3] = 1.0;
        Properties p6 = QL_OPTION_BASKET(
            "opt_basket",                   // option handle
            v,                              // vector of stochastic process handles
            "MIN",                          // basket type
            correlations,                   // correlations
            "CALL",                         // option type
            40.0,                           // strike price
            "EU",                           // exercise type
            exerciseDate.serialNumber(),    // exercise date
            0,                              // settlement date ignored when exercise = European
            "SE",                           // engine type
            timeSteps);                     // time steps

        printObject("QL_OPTION_BASKET", p6);

        std::vector< long > resetDates;
        resetDates.push_back(todaysDate.serialNumber() + 90);
        Properties p7 = QL_OPTION_CLIQUET(
            "opt_cliquet",                  // option handle
            "stoch1",                       // stochastic process handle
            resetDates,                     // reset dates
            "PUT",                          // option type
            strike,                         // strike price
            exerciseDate.serialNumber(),    // exercise date
            "AC",                           // engine type
            timeSteps);                     // time steps

        printObject("QL_OPTION_CLIQUET", p7);

        std::vector< long > dividendDates;
        std::vector< double > dividends;
        for (Date d = todaysDate + 3*Months; d < exerciseDate; d+= 6*Months) {
            dividendDates.push_back(d.serialNumber());
            dividends.push_back(5.0);
        }
        Properties p8 = QL_OPTION_DIVIDENDVANILLA(
            "opt_divvan",                   // option handle
            "stoch1",                       // stochastic process handle
            dividendDates,                  // dividend dates
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
        Properties p9 = QL_OPTION_FORWARDVANILLA(
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
        cout << endl << "end options test" << endl;
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


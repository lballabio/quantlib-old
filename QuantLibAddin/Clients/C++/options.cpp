
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
    
void printObject(const string &className, const Properties &p) {
    QL_LOGMESSAGE("Object properties - class " + className);
    Properties::const_iterator it;
    for (it = p.begin(); it != p.end(); it++) {
        ObjectProperty property = *it;
        ostringstream os;
        os << left << "property = " << setw(10) << property.name() <<
            "value = " << property();
        QL_LOGMESSAGE(os.str());
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

        ArgStack a1;
        a1.push(underlying);                // underlying
        a1.push(string("ACT360"));          // daycount convention
        a1.push(settlementDate.serialNumber()); // settlement date as long
        a1.push(riskFreeRate);              // risk free rate
        a1.push(dividendYield);             // dividend yield
        a1.push(volatility);                // volatility
        Properties p1 =
            QL_OBJECT_MAKE(StochasticProcess)("stoch1", a1);
        printObject("StochasticProcess", p1);

        ArgStack a2;
        a2.push(string("stoch1"));          // stochastic process handle
        a2.push(string("PUT"));             // option type
        a2.push(string("VAN"));             // payoff type (plain vanilla)
        a2.push(strike);                    // strike price
        a2.push(string("AM"));              // exercise type (american)
        a2.push(exerciseDate.serialNumber()); // exercise date
        a2.push(settlementDate.serialNumber()); // settlement date
        a2.push(string("JR"));              // engine type (jarrow rudd)
        a2.push(timeSteps);                 // time steps
        Properties p2 =
            QL_OBJECT_MAKE(VanillaOption)("opt_van", a2);
        printObject("QL_OPTION_VANILLA", p2);

        ArgStack a3;
        a3.push(string("stoch1"));          // stochastic process handle
        a3.push(string("G"));               // average type ("A"verage/"G"eometric)
        a3.push(string("PUT"));             // option type
        a3.push(string("VAN"));             // payoff type (plain vanilla)
        a3.push(strike);                    // strike price
        a3.push(string("EU"));              // exercise type (american)
        a3.push(exerciseDate.serialNumber()); // exercise date
        a3.push(0l);                        // settlement date ignored when exercise = European
        a3.push(string("ACGAPA"));          // engine type
        a3.push(timeSteps);                 // time steps
        Properties p3 =
            QL_OBJECT_MAKE(ContinuousAveragingAsianOption)("opt_asian_cont", a3);
        printObject("ContinuousAveragingAsianOption", p3);

        vector < long > fixingDates;
        for (int i = 0; i < exerciseDate - todaysDate + 1; i++)
            fixingDates.push_back(todaysDate.serialNumber() + i);
        ArgStack a4;
        a4.push(string("stoch1"));          // stochastic process handle
        a4.push(string("G"));               // average type ("A"verage/"G"eometric)
        a4.push(1.0);                       // running accumulator
        a4.push(0l);                        // past fixings
        a4.push(fixingDates);               // fixingDates
        a4.push(string("PUT"));             // option type
        a4.push(string("VAN"));             // payoff type (plain vanilla)
        a4.push(strike);                    // strike price
        a4.push(string("EU"));              // exercise type (american)
        a4.push(exerciseDate.serialNumber()); // exercise date
        a4.push(0l);                        // settlement date ignored when exercise = European
        a4.push(string("ADGAPA"));          // engine type
        a4.push(timeSteps);                 // time steps
        Properties p4 =
            QL_OBJECT_MAKE(DiscreteAveragingAsianOption)("opt_asian_disc", a4);
        printObject("DiscreteAveragingAsianOption", p4);

        ArgStack a5;
        a5.push(string("stoch1"));          // stochastic process handle
        a5.push(string("DOWNIN"));          // barrier type
        a5.push(35.0);                      // barrier
        a5.push(3.0);                       // rebate
        a5.push(string("PUT"));             // option type
        a5.push(string("VAN"));             // payoff type (plain vanilla)
        a5.push(strike);                    // strike price
        a5.push(string("EU"));              // exercise type (american)
        a5.push(exerciseDate.serialNumber()); // exercise date
        a5.push(0l);                        // settlement date ignored when exercise = European
        a5.push(string("AB"));              // engine type
        a5.push(timeSteps);                 // time steps
        Properties p5 =
            QL_OBJECT_MAKE(BarrierOption)("opt_barrier", a5);
        printObject("BarrierOption", p5);

        vector < string > stochHandles;
        stochHandles.push_back("stoch1");
        stochHandles.push_back("stoch1");
        vector < vector < double > >correlations;
        vector < double > row1, row2;
        row1.push_back(1.0);
        row1.push_back(0.9);
        row2.push_back(0.9);
        row2.push_back(1.0);
        correlations.push_back(row1);
        correlations.push_back(row2);

        ArgStack a6;
        a6.push(stochHandles);              // vector of stochastic process handles
        a6.push(string("MIN"));             // basket type
        a6.push(correlations);              // correlations matrix
        a6.push(string("CALL"));            // option type
        a6.push(strike);                    // strike price
        a6.push(string("EU"));              // exercise type (american)
        a6.push(exerciseDate.serialNumber()); // exercise date
        a6.push(0l);                        // settlement date ignored when exercise = European
        a6.push(string("SE"));              // engine type
        a6.push(timeSteps);                 // time steps
        Properties p6 =
            QL_OBJECT_MAKE(BasketOption)("opt_basket", a6);
        printObject("BasketOption", p6);

        vector < long > resetDates;
        resetDates.push_back(Date(13, August, 1998).serialNumber());
        ArgStack a7;
        a7.push(string("stoch1"));          // stochastic process handle
        a7.push(resetDates);                // reset dates
        a7.push(string("PUT"));             // option type
        a7.push(strike);                    // strike price
        a7.push(exerciseDate.serialNumber()); // exercise date
        a7.push(string("AC"));              // engine type
        a7.push(timeSteps);                 // time steps
        Properties p7 =
            QL_OBJECT_MAKE(CliquetOption)("opt_cliquet", a7);
        printObject("CliquetOption", p7);

        vector < long > dividendDates;
        dividendDates.push_back(Date(15, August, 1998).serialNumber());
        dividendDates.push_back(Date(15, February, 1999).serialNumber());
        vector < double >dividends;
        dividends.push_back(5.);
        dividends.push_back(5.);

        ArgStack a8;
        a8.push(string("stoch1"));          // stochastic process handle
        a8.push(dividendDates);             // dividend dates
        a8.push(dividends);                 // dividends
        a8.push(string("CALL"));            // option type
        a8.push(string("VAN"));             // payoff type (plain vanilla)
        a8.push(10.0);                      // strike price
        a8.push(string("EU"));              // exercise type
        a8.push(exerciseDate.serialNumber()); // exercise date
        a8.push(0l);                        // settlement date ignored when exercise = European
        a8.push(string("ADE"));             // engine type (jarrow rudd)
        a8.push(timeSteps);                 // time steps
        Properties p8 =
            QL_OBJECT_MAKE(DividendVanillaOption)("opt_divvan", a8);
        printObject("DividendVanillaOption", p8);

        long resetDate = todaysDate.serialNumber() + 90;

        ArgStack a9;
        a9.push(string("stoch1"));          // stochastic process handle
        a9.push(12.);                       // moneyness
        a9.push(resetDate);                 // reset date
        a9.push(string("PUT"));             // option type
        a9.push(string("VAN"));             // payoff type (plain vanilla)
        a9.push(strike);                    // strike price
        a9.push(string("EU"));              // exercise type
        a9.push(exerciseDate.serialNumber()); // exercise date
        a9.push(0l);                        // settlement date ignored when exercise = European
        a9.push(string("FE"));              // engine type (jarrow rudd)
        a9.push(timeSteps);                 // time steps
        Properties p9 =
            QL_OBJECT_MAKE(ForwardVanillaOption)("opt_fwdvan", a9);
        printObject("ForwardVanillaOption", p9);

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


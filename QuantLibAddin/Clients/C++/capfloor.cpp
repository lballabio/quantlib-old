
/*
 Copyright (C) 2005 Aurelien Chanudet

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

#define LENGTH(a) ((sizeof(a))/(sizeof(a[0])))

int main()
{
    try {
        
        QL_LOGFILE("quantlib.log");
        QL_CONSOLE(1);
        QL_LOGMESSAGE("begin capfloor test");
        
        double dQuotes[] = { 0.020800, 0.020960, 0.021500, 0.021700, 0.021860, 0.022120,
                             0.022420, 0.022720, 0.023020, 0.023330, 0.023620, 0.023920 };
        long dMaturities[] = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 };
    
        double sQuotes[] = { 0.027310, 0.030005, 0.032365, 0.034385, 0.036165, 0.037750,
                             0.039100, 0.040230, 0.041170, 0.042670, 0.044365, 0.046145,
                             0.047280 };
        long sMaturities[] = { 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 15, 20, 30 };
        
        std::vector< std::string > rateHelpers;
        
        for (std::size_t i=0 ; i < LENGTH(dQuotes) ; i++)
        {
            ArgStack stack;
            stack.push(dQuotes[i]);
            stack.push(dMaturities[i]);
            stack.push(string("MONTHS"));
            stack.push(2l);
            stack.push(string("TARGET"));
            stack.push(string("MF"));
            stack.push(string("ACT360"));
            std::string handle = "handleDeposit"
                               + IntegerFormatter::toString(dMaturities[i]);
            Properties prop = QL_OBJECT_MAKE(DepositRateHelper)(handle, stack);            
            rateHelpers.push_back(handle);
        }
        
        for (std::size_t i2=0 ; i2 < LENGTH(dMaturities) ; i2++)
        {
            ArgStack stack;
            stack.push(sQuotes[i2]);
            stack.push(sMaturities[i2]);
            stack.push(string("YEARS"));
            stack.push(2l);
            stack.push(string("TARGET"));
            stack.push(string("A"));                    // fixed frequency
            stack.push(string("U"));                    // fixed convention
            stack.push(string("THIRTY360"));            // fixed day counter
            stack.push(string("S"));                    // floating frequency
            stack.push(string("MF"));                   // floating convention
            std::string handle = "handleSwap"
                                 + IntegerFormatter::toString(sMaturities[i2]);
            (void) QL_OBJECT_MAKE(SwapRateHelper)(handle, stack);            
            rateHelpers.push_back(handle);        
        }
        
        Date evaluationDate(23, March, 2005);
        Date settlementDate(25, March, 2005);
        
        ArgStack tsArgs;
        tsArgs.push(evaluationDate.serialNumber());
        tsArgs.push(settlementDate.serialNumber());
        tsArgs.push(rateHelpers);
        tsArgs.push(string("ACT360"));
        (void) QL_OBJECT_MAKE(PiecewiseFlatForward)("my_termStructure", tsArgs);
        
        ArgStack hwArgs;
        hwArgs.push(string("my_termStructure"));
        hwArgs.push(0.1);
        hwArgs.push(0.01);
        (void) QL_OBJECT_MAKE(HullWhite)("my_hullwhite", hwArgs);
        
        ArgStack engineArgs;
        engineArgs.push(string("my_hullwhite"));
        (void) QL_OBJECT_MAKE(AnalyticCapFloorEngine)("my_closedForm", engineArgs);
        
        Date startDate(25, March, 2006);
        
        ArgStack capArgs;
        capArgs.push(startDate.serialNumber());
        capArgs.push(5l);
        capArgs.push(string("YEARS"));
        capArgs.push(string("MF"));
        capArgs.push(string("S"));
        capArgs.push(2l);
        capArgs.push(string("my_termStructure"));
        capArgs.push(100000.0);
        capArgs.push(0.04);
        capArgs.push(string("my_closedForm"));
        capArgs.push(string("Cap"));
        Properties opProperties = QL_OBJECT_MAKE(CapFloor)("my_cap", capArgs);
        
        QL_LOG_OBJECT("my_cap");
        QL_LOGMESSAGE("end capfloor test");
        
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


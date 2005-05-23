
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
        
        OH_LOGFILE("quantlib.log");
        OH_CONSOLE(1);
        OH_LOG_MESSAGE("begin capfloor test");
        
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
            ArgumentStack stack;
            stack.push(dQuotes[i]);
            stack.push(dMaturities[i]);
            stack.push(string("Months"));
            stack.push(2l);
            stack.push(string("Target"));
            stack.push(string("ModifiedFollowing"));
            stack.push(string("Actual360"));
            std::string handle = "handleDeposit"
                               + IntegerFormatter::toString(dMaturities[i]);
            Properties prop = OH_MAKE_OBJECT(QuantLibAddin::DepositRateHelper, handle, stack);            
            rateHelpers.push_back(handle);
        }
        
        for (std::size_t i2=0 ; i2 < LENGTH(dMaturities) ; i2++)
        {
            ArgumentStack stack;
            stack.push(sQuotes[i2]);
            stack.push(sMaturities[i2]);
            stack.push(string("Years"));
            stack.push(2l);
            stack.push(string("Target"));
            stack.push(string("Annual"));               // fixed frequency
            stack.push(string("Unadjusted"));           // fixed convention
            stack.push(string("Thirty360"));            // fixed day counter
            stack.push(string("Semiannual"));           // floating frequency
            stack.push(string("ModifiedFollowing"));    // floating convention
            std::string handle = "handleSwap"
                                 + IntegerFormatter::toString(sMaturities[i2]);
            (void) OH_MAKE_OBJECT(QuantLibAddin::SwapRateHelper, handle, stack);            
            rateHelpers.push_back(handle);        
        }
        
        Date evaluationDate(23, March, 2005);
        Date settlementDate(25, March, 2005);
        
        ArgumentStack tsArgs;
        tsArgs.push(evaluationDate.serialNumber());
        tsArgs.push(settlementDate.serialNumber());
        tsArgs.push(rateHelpers);
        tsArgs.push(string("Actual360"));
        (void) OH_MAKE_OBJECT(QuantLibAddin::PiecewiseFlatForward, "my_termStructure", tsArgs);
        
        ArgumentStack hwArgs;
        hwArgs.push(string("my_termStructure"));
        hwArgs.push(0.1);
        hwArgs.push(0.01);
        (void) OH_MAKE_OBJECT(QuantLibAddin::HullWhite, "my_hullwhite", hwArgs);
        
        ArgumentStack engineArgs;
        engineArgs.push(string("my_hullwhite"));
        (void) OH_MAKE_OBJECT(QuantLibAddin::AnalyticCapFloorEngine, "my_closedForm", engineArgs);
        
        Date startDate(25, March, 2006);

        ArgumentStack capArgs;
        capArgs.push(startDate.serialNumber());     // start of capping period
        capArgs.push(5l);                           // capping period length
        capArgs.push(string("Years"));              // time units
        capArgs.push(string("ModifiedFollowing"));  // business day convention
        capArgs.push(string("Semiannual"));         // capping frequency (semiannual)
        capArgs.push(2l);                           // fixing days
        capArgs.push(string("my_termStructure"));   // term structure
        capArgs.push(100000.0);                     // nominal
        capArgs.push(0.04);                         // cap srike
        capArgs.push(0.02);                         // floor strike
        capArgs.push(string("my_closedForm"));      // pricer
        capArgs.push(string("Cap"));                // option type
        capArgs.push(0l);                           // no amortisation
        Properties opProperties = OH_MAKE_OBJECT(QuantLibAddin::CapFloor, "my_cap", capArgs);
        
        OH_LOG_OBJECT("my_cap");
        OH_LOG_MESSAGE("end capfloor test");
        
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


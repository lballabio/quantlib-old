
/*
 Copyright (C) 2005 Eric Ehlers
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
        
        setLogFile("quantlib.log");
        setConsole(1);
        logMessage("begin capfloor test");
        
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
            obj_ptr depositRateHelper(new QuantLibAddin::DepositRateHelper(
                dQuotes[i],
                dMaturities[i],
                "Months",
                2,
                "Target",
                "ModifiedFollowing",
                "Actual360"));
            std::ostringstream handle;
            handle << "handleDeposit" << dMaturities[i];
            storeObject(handle.str(), depositRateHelper);
        }
        
        for (std::size_t i2=0 ; i2 < LENGTH(dMaturities) ; i2++)
        {
            obj_ptr swapRateHelper(new QuantLibAddin::SwapRateHelper(
                sQuotes[i2],
                sMaturities[i2],
                "Years",
                2,
                "Target",
                "Annual",                 // fixed frequency
                "Unadjusted",             // fixed convention
                "Thirty360",              // fixed day counter
                "Semiannual",             // floating frequency
                "ModifiedFollowing"));    // floating convention

            std::ostringstream handle;
            handle << "handleSwap" << sMaturities[i2];
            storeObject(handle.str(), swapRateHelper);
            rateHelpers.push_back(handle.str());

        }
        
        Date evaluationDate(23, March, 2005);
        Date settlementDate(25, March, 2005);
        
        obj_ptr piecewiseFlatForward(new QuantLibAddin::PiecewiseFlatForward(
            evaluationDate.serialNumber(),
            settlementDate.serialNumber(),
            rateHelpers,
            "Actual360"));
        storeObject("my_termStructure", piecewiseFlatForward);

        obj_ptr hullWhite(new QuantLibAddin::HullWhite(
            "my_termStructure",
            0.1,
            0.01));
        storeObject("my_hullwhite", hullWhite);
        
        obj_ptr analyticCapFloorEngine(new QuantLibAddin::AnalyticCapFloorEngine(
            "my_hullwhite"));
        storeObject("my_closedForm", analyticCapFloorEngine);

        Date startDate(25, March, 2006);

        obj_ptr capFloor(new QuantLibAddin::CapFloor(
            startDate.serialNumber(),   // start of capping period
            5,                          // capping period length
            "Years",                    // time units
            "ModifiedFollowing",        // business day convention
            "Semiannual",               // capping frequency (semiannual)
            2,                          // fixing days
            "my_termStructure",         // term structure
            100000.0,                   // nominal
            0.04,                       // cap srike
            0.02,                       // floor strike
            "my_closedForm",            // pricer
            "Cap",                      // option type
            0));                        // no amortisation
        storeObject("my_cap", capFloor);
        
        logObject("my_cap");
        logMessage("end capfloor test");
        
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



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

// test program for instruments

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
        QL_LOGMESSAGE("begin instruments test");

        Date issueDate(1, January, 2000);           // issue date
        Date datedDate(15, July, 2001);         // dated date
        Date maturityDate(1, January, 2010);        // maturity date
        long settlementDays         = 3;        // settlement days
        double coupon               = 0.04;     // coupon
        double yield                = 0.06;     // yield
        string frequencyID          = "A";      // frequency (annual)
        string dayCounterID         = "ACTACT"; // day count (actual/actual)
        string calendarID           = "DE";     // calendar (Germany)

        ArgStack a1;
        a1.push(issueDate.serialNumber());      // issue date as long
        a1.push(datedDate.serialNumber());      // dated date as long
        a1.push(maturityDate.serialNumber());   // maturity date as long
        a1.push(settlementDays);                // settlement days
        a1.push(coupon);                        // coupon
        a1.push(yield);                         // yield
        a1.push(frequencyID);                   // frequency ID
        a1.push(dayCounterID);                  // day counter ID
        a1.push(calendarID);                    // calendar ID
        Properties p1 =
            QL_OBJECT_MAKE(FixedCouponBond)("bond1", a1);
        printObject("FixedCouponBond", p1);

        QL_LOGMESSAGE("end instruments test");
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


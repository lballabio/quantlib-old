
/*
 Copyright (C) 2005 Eric Ehlers
 Copyright (C) 2005 Walter Penschke

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

using namespace std;
using namespace QuantLib;
using namespace ObjHandler;
using namespace QuantLibAddin;

int main() {
    try {
        OH_LOGFILE("quantlib.log");
        OH_CONSOLE(1);
        OH_LOGMESSAGE("begin instruments test");

        // Fixed Coupon Bond

        Date issueDate(13, March, 2005);        // issue date
        Date datedDate(13, March, 2005);        // dated date
        Date maturityDate(13, March, 2015);     // maturity date
        long settlementDays         = 3;        // settlement days
        vector < double > coupons(1, 0.04);     // coupons
        double yield                = 0.06;     // yield
        string frequencyID          = "A";      // frequency (annual)
        string dayCounterID         = "THIRTY360"; // day count (30/360)
        string calendarID           = "DE";     // calendar (Germany)

        ArgStack a1;
        a1.push(issueDate.serialNumber());      // issue date as long
        a1.push(datedDate.serialNumber());      // dated date as long
        a1.push(maturityDate.serialNumber());   // maturity date as long
        a1.push(settlementDays);                // settlement days
        a1.push(coupons);                       // coupons
        a1.push(yield);                         // yield
        a1.push(frequencyID);                   // frequency ID
        a1.push(dayCounterID);                  // day counter ID
        a1.push(calendarID);                    // calendar ID
        OH_OBJECT_MAKE(QuantLibAddin::FixedCouponBond)("fixedCouponBond", a1);
        OH_LOG_OBJECT("fixedCouponBond");

        // ZeroCurve

        Date todaysDate(15, March, 2005);

        long todayAsLong = todaysDate.serialNumber();
        vector<long> datesAsLong;
        vector<double> yieldsAsDouble;
        DayCounter dayCounter = ActualActual();

        datesAsLong.push_back(todayAsLong);        yieldsAsDouble.push_back(0.04);
        datesAsLong.push_back(todayAsLong + 30);   yieldsAsDouble.push_back(0.041);
        datesAsLong.push_back(todayAsLong + 60);   yieldsAsDouble.push_back(0.042);
        datesAsLong.push_back(todayAsLong + 90);   yieldsAsDouble.push_back(0.043);
        datesAsLong.push_back(todayAsLong + 182);  yieldsAsDouble.push_back(0.044);
        datesAsLong.push_back(todayAsLong + 365);  yieldsAsDouble.push_back(0.045);
        datesAsLong.push_back(todayAsLong + 730);  yieldsAsDouble.push_back(0.046);
        datesAsLong.push_back(todayAsLong + 1826); yieldsAsDouble.push_back(0.047);
        datesAsLong.push_back(todayAsLong + 3652); yieldsAsDouble.push_back(0.048);

        ArgStack zeroCurveArgs;
        zeroCurveArgs.push(datesAsLong);
        zeroCurveArgs.push(yieldsAsDouble);
        zeroCurveArgs.push(string("ACTACT"));

        OH_OBJECT_MAKE(QuantLibAddin::ZeroCurve)("myZeroCurve", zeroCurveArgs);
        OH_LOG_OBJECT("myZeroCurve");

        // ZeroBond

        long issueDateAsLong = issueDate.serialNumber();
        long maturityDateAsLong = maturityDate.serialNumber();
        // Integer settlementDays = 3;
        std::string dayCounterId = "ACTACT";
        std::string calendarId = "DE";
        std::string conventionId = "F";
        double redemption = 100.0;
        std::string zeroCurveHandle = "myZeroCurve";

        ArgStack zeroCouponBondArgs;
        zeroCouponBondArgs.push(issueDateAsLong);
        zeroCouponBondArgs.push(maturityDateAsLong);
        zeroCouponBondArgs.push((long)settlementDays);
        zeroCouponBondArgs.push(dayCounterId);
        zeroCouponBondArgs.push(calendarId);
        zeroCouponBondArgs.push(conventionId);
        zeroCouponBondArgs.push((double)redemption);
        zeroCouponBondArgs.push(zeroCurveHandle);

        OH_OBJECT_MAKE(QuantLibAddin::ZeroCouponBond)("myZeroCouponBond", zeroCouponBondArgs);
        OH_LOG_OBJECT("myZeroCouponBond");

        OH_LOGMESSAGE("end instruments test");
        return 0;
    } catch (const exception &e) {
        ostringstream s;
        s << "Error: " << e.what();
        OH_LOGMESSAGE(s.str(), 1);
        return 1;
    } catch (...) {
        OH_LOGMESSAGE("unknown error", 1);
        return 1;
    }
}


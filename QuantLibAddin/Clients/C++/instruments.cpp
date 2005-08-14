
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
        setLogFile("quantlib.log");
        setConsole(1);
        logMessage("begin instruments test");

        // Fixed Coupon Bond

        Date issueDate(13, March, 2005);        // issue date
        Date datedDate(13, March, 2005);        // dated date
        Date maturityDate(13, March, 2015);     // maturity date
        long settlementDays         = 3;        // settlement days
        vector < double > coupons(1, 0.04);     // coupons
        vector < double > nominals(1, 100.0);   // nominals
        double redemption           = 100.0;    // redemption
        string frequencyID          = "Annual"; // frequency
        string dayCounterID         = "Thirty360"; // day count
        string businessDayConvention= "Following"; // business day convention
        string calendarID           = "Germany";// calendar

        obj_ptr fixedCouponBond(new QuantLibAddin::FixedCouponBond(
            issueDate.serialNumber(),           // issue date as long
            datedDate.serialNumber(),           // dated date as long
            maturityDate.serialNumber(),        // maturity date as long
            settlementDays,                     // settlement days
            coupons,                            // coupons
            nominals,                           // nominals
            redemption,                         // redemption
            frequencyID,                        // frequency ID
            dayCounterID,                       // day counter ID
            businessDayConvention,              // business day convention
            calendarID,                         // calendar ID
            true,                               // startFromEnd
            true,                               // longFinal
            ""));                               // discCurveId
        storeObject("myFixedCouponBond", fixedCouponBond);
        logObject("myFixedCouponBond");

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

        obj_ptr zeroCurve(new QuantLibAddin::ZeroCurve(
            datesAsLong,
            yieldsAsDouble,
            "ActualActual"));
        storeObject("myZeroCurve", zeroCurve);
        logObject("myZeroCurve");

        // ZeroBond

        long issueDateAsLong = issueDate.serialNumber();
        long maturityDateAsLong = maturityDate.serialNumber();
        // Integer settlementDays = 3;
        std::string dayCounterId = "ActualActual";
        std::string calendarId = "Germany";
        std::string conventionId = "Following";
        double redemption2 = 100.0;
        std::string zeroCurveHandle = "myZeroCurve";

        obj_ptr zeroCouponBond(new QuantLibAddin::ZeroCouponBond(
            issueDateAsLong,
            maturityDateAsLong,
            settlementDays,
            dayCounterId,
            calendarId,
            conventionId,
            redemption2,
            zeroCurveHandle));
        storeObject("myZeroCouponBond", zeroCouponBond);
        logObject("myZeroCouponBond");

        logMessage("end instruments test");
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


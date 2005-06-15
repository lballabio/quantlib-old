
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
        OH_LOG_MESSAGE("begin instruments test");

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

        ArgumentStack a1;
        a1.push(issueDate.serialNumber());      // issue date as long
        a1.push(datedDate.serialNumber());      // dated date as long
        a1.push(maturityDate.serialNumber());   // maturity date as long
        a1.push(settlementDays);                // settlement days
        a1.push(coupons);                       // coupons
        a1.push(nominals);                      // nominals
        a1.push(redemption);                    // redemption
        a1.push(frequencyID);                   // frequency ID
        a1.push(dayCounterID);                  // day counter ID
        a1.push(businessDayConvention);         // business day convention
        a1.push(calendarID);                    // calendar ID
        a1.push(true);                          // startFromEnd
        a1.push(true);                          // longFinal
        a1.push(std::string());                 // discCurveId
        OH_MAKE_OBJECT(QuantLibAddin::FixedCouponBond, "fixedCouponBond", a1);
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

        ArgumentStack zeroCurveArgs;
        zeroCurveArgs.push(datesAsLong);
        zeroCurveArgs.push(yieldsAsDouble);
        zeroCurveArgs.push(string("ActualActual"));

        OH_MAKE_OBJECT(QuantLibAddin::ZeroCurve, "myZeroCurve", zeroCurveArgs);
        OH_LOG_OBJECT("myZeroCurve");

        // ZeroBond

        long issueDateAsLong = issueDate.serialNumber();
        long maturityDateAsLong = maturityDate.serialNumber();
        // Integer settlementDays = 3;
        std::string dayCounterId = "ActualActual";
        std::string calendarId = "Germany";
        std::string conventionId = "Following";
        double redemption2 = 100.0;
        std::string zeroCurveHandle = "myZeroCurve";

        ArgumentStack zeroCouponBondArgs;
        zeroCouponBondArgs.push(issueDateAsLong);
        zeroCouponBondArgs.push(maturityDateAsLong);
        zeroCouponBondArgs.push((long)settlementDays);
        zeroCouponBondArgs.push(dayCounterId);
        zeroCouponBondArgs.push(calendarId);
        zeroCouponBondArgs.push(conventionId);
        zeroCouponBondArgs.push((double)redemption2);
        zeroCouponBondArgs.push(zeroCurveHandle);

        OH_MAKE_OBJECT(QuantLibAddin::ZeroCouponBond, "myZeroCouponBond", zeroCouponBondArgs);
        OH_LOG_OBJECT("myZeroCouponBond");

        OH_LOG_MESSAGE("end instruments test");
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


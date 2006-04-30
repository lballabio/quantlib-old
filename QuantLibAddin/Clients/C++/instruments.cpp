
/*
 Copyright (C) 2005, 2006 Eric Ehlers
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

#define LENGTH(a) ((sizeof(a))/(sizeof(a[0])))

struct RateHelperDatum {
    Integer n;
    char* units;
    Rate rate;
};

struct RateHelperDatum depositData [] = {
    { 1, "Months", 0.0210 },
    { 3, "Months", 0.0212 },
    { 6, "Months", 0.0214 }
};

struct RateHelperDatum swapData [] = {
    {  1, "Years", 0.02199 },
    {  2, "Years", 0.02320 },
    {  3, "Years", 0.02468 },
    {  4, "Years", 0.02618 },
    {  5, "Years", 0.02759 },
    {  6, "Years", 0.02892 },
    {  7, "Years", 0.03015 },
    {  8, "Years", 0.03129 },
    {  9, "Years", 0.03229 },
    { 10, "Years", 0.03317 },
    { 11, "Years", 0.03393 },
    { 12, "Years", 0.03459 },
    { 13, "Years", 0.03516 },
    { 14, "Years", 0.03566 },
    { 15, "Years", 0.03611 },
    { 16, "Years", 0.03652 },
    { 17, "Years", 0.03688 },
    { 18, "Years", 0.03719 },
    { 19, "Years", 0.03745 },
    { 20, "Years", 0.03767 }
};

int main() {
    try {
        setLogFile("quantlib.log");
        setConsole(1);
        logMessage("begin instruments test");

        // Discount Curve

        std::vector<std::string> rateHelpers;

        for (std::size_t i=0 ; i < LENGTH(depositData) ; i++) {
            const struct RateHelperDatum& datum = depositData[i];

            ObjHandler::obj_ptr depositRateHelper(
                new QuantLibAddin::DepositRateHelper(datum.rate,
                                                     datum.n,
                                                     datum.units,
                                                     2,
                                                     "NullCalendar",
                                                     "Unadjusted",
                                                     "Simple"));
            std::ostringstream handle;
            handle << datum.n << "M";
            ObjHandler::storeObject(handle.str(), depositRateHelper);

            rateHelpers.push_back(handle.str());
        }

        for (std::size_t j=0 ; j < LENGTH(swapData) ; j++) {
            const struct RateHelperDatum& datum = swapData[j];

            ObjHandler::obj_ptr swapRateHelper(
                new QuantLibAddin::SwapRateHelper(datum.rate,
                                                  datum.n,
                                                  datum.units,
                                                  2,
                                                  "NullCalendar",
                                                  "Annual",          // fixed frequency
                                                  "Unadjusted",      // fixed convention
                                                  "Simple",          // fixed day counter
                                                  "Annual",          // floating frequency
                                                  "Unadjusted",      // floating convention
                                                  "Simple"));        // floating day counter

            std::ostringstream handle;
            handle << datum.n << "Y"; 
            ObjHandler::storeObject(handle.str(), swapRateHelper);

            rateHelpers.push_back(handle.str());
        }

        QuantLib::setEvaluationDate(QuantLib::Date(23, March, 2005));
        Date settlementDate(25, March, 2005);

        ObjHandler::obj_ptr piecewiseFlatForward(
            new QuantLibAddin::PiecewiseFlatForward(settlementDate.serialNumber(),
                                                    rateHelpers,
                                                    "Simple"));
        ObjHandler::storeObject("YC", piecewiseFlatForward);

        // Fixed Coupon Bond

        Date issueDate(13, March, 2005);        // issue date
        Date datedDate(13, March, 2005);        // dated date
        Date maturityDate(13, March, 2015);     // maturity date
        long settlementDays = 3;                // settlement days
        vector < double > coupons(1, 0.04);     // coupons
        vector < double > nominals(1, 100.0);   // nominals
        double redemption = 100.0;              // redemption
        string frequencyID = "Annual";          // frequency
        string dayCounterID = "Thirty360";      // day count
        string businessDayConvention= "Following"; // business day convention
        string calendarID = "Germany";          // calendar

        obj_ptr fixedCouponBond(new QuantLibAddin::FixedCouponBond(
            issueDate.serialNumber(),           // issue date as serial number
            datedDate.serialNumber(),           // dated date as serial number
            maturityDate.serialNumber(),        // maturity date as serial number
            settlementDays,                     // settlement days
            coupons,                            // coupons
            nominals,                           // nominals
            redemption,                         // redemption
            frequencyID,                        // frequency ID
            dayCounterID,                       // day counter ID
            businessDayConvention,              // accrual convention ID
            businessDayConvention,              // payment convention ID
            calendarID,                         // calendar ID
            true,                               // startFromEnd
            true,                               // longFinal
            "YC"));                             // discCurveId
        storeObject("myFixedCouponBond", fixedCouponBond);

        std::ostringstream msg1;
        msg1 << "fixed coupon bond NPV: "
            << boost::dynamic_pointer_cast<QuantLibAddin::FixedCouponBond>
                (fixedCouponBond)->getObject().NPV();
        ObjHandler::logMessage(msg1.str());

        // ZeroCurve

        std::string zeroCurveHandle = "myZeroCurve";
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
        storeObject(zeroCurveHandle, zeroCurve);
        zeroCurve->setProperties(boost::shared_ptr<ObjHandler::ValueObject>
            (new QuantLibAddin::ValueObjects::qlZeroCurve(
            zeroCurveHandle,
            datesAsLong,
            yieldsAsDouble,
            "ActualActual")));
        logObject(zeroCurveHandle);

        // ZeroBond

        long issueDateAsLong = issueDate.serialNumber();
        long maturityDateAsLong = maturityDate.serialNumber();
        std::string dayCounterId = "ActualActual";
        std::string calendarId = "Germany";
        std::string conventionId = "Following";
        double redemption2 = 100.0;

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

        std::ostringstream msg2;
        msg2 << "zero coupon bond NPV: "
            << boost::dynamic_pointer_cast<QuantLibAddin::ZeroCouponBond>
                (zeroCouponBond)->getObject().NPV();
        ObjHandler::logMessage(msg2.str());

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


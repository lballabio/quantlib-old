
/*
 Copyright (C) 2005 Eric Ehlers
 Copyright (C) 2005 Aurelien Chanudet

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it
 under the terms of the QuantLib license.  You should have received a
 copy of the license along with this program; if not, please email
 <quantlib-dev@lists.sf.net>. The license is also available online at
 <http://quantlib.org/license.shtml>.

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
*/

#include <qla/qladdin.hpp>
#include <sstream>

using namespace QuantLib;
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

int main()
{
    try {
        
        ObjHandler::setLogFile("quantlib.log");
        ObjHandler::setConsole(1);
        ObjHandler::logMessage("begin capfloor test");
        
        // -- Bootstrap term structure --
        
        std::vector<std::string> rateHelpers;
        
        for (std::size_t i=0 ; i < LENGTH(depositData) ; ++i) {
            const struct RateHelperDatum& datum = depositData[i];
            
            ObjHandler::obj_ptr depositRateHelper(
                new QuantLibAddin::DepositRateHelper(datum.rate,
                                                     datum.n,
                                                     datum.units,
                                                     2,
                                                     "NullCalendar",
                                                     "Unadjusted",
                                                     "Simple"));
            std::ostringstream objectID;
            objectID << datum.n << "M";
            ObjHandler::storeObject(objectID.str(), depositRateHelper);
            
            rateHelpers.push_back(objectID.str());
        }
        
        for (std::size_t j=0 ; j < LENGTH(swapData) ; ++j) {
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

            std::ostringstream objectID;
            objectID << datum.n << "Y"; 
            ObjHandler::storeObject(objectID.str(), swapRateHelper);
            
            rateHelpers.push_back(objectID.str());
        }
        
        QuantLib::setEvaluationDate(QuantLib::Date(23, March, 2005));
        Date settlementDate(25, March, 2005);
                
        ObjHandler::obj_ptr piecewiseFlatForward(
            new QuantLibAddin::PiecewiseFlatForward(settlementDate.serialNumber(),
                                                    rateHelpers,
                                                    "Simple"));
        ObjHandler::storeObject("YC", piecewiseFlatForward);
        
        // -- Index --
        
        std::vector<long> dates(0);
        std::vector<double> fixings(0);
        long tenor = 12;
        long fixingDays = 2;
        
        ObjHandler::obj_ptr index(
            new QuantLibAddin::Xibor("Euribor",
                                     "EUR",
                                     tenor,
                                     "Months",
                                     "NullCalendar",
                                     "Unadjusted",
                                     "Simple",
                                     fixingDays,
                                     "YC",
                                     dates,
                                     fixings));
        ObjHandler::storeObject("IDX", index);
                                                
        
        // -- Make cap/floor --
        
        ObjHandler::obj_ptr hullWhite(
            new QuantLibAddin::HullWhite("YC", 0.1, 0.0079));
        ObjHandler::storeObject("HW", hullWhite);
        
        ObjHandler::obj_ptr analyticCapFloorEngine(
            new QuantLibAddin::AnalyticCapFloorEngine("HW"));
        ObjHandler::storeObject("ENGINE", analyticCapFloorEngine);
        
        Date startDate (29, July, 2005);
        Date endDate   (29, July, 2011);
                
        std::vector<double> nominals (6, 1.0);
        std::vector<double> spreads  (6, 0.0);
        std::vector<double> cStrikes (6, 0.04);
        std::vector<double> fStrikes (6, 0.04);
        
        ObjHandler::obj_ptr schedule(
            new QuantLibAddin::Schedule("NullCalendar",
                                        startDate.serialNumber(),
                                        endDate.serialNumber(),
                                        "Annual",
                                        "Unadjusted",
                                        false,
                                        false));
        ObjHandler::storeObject("SCHEDULE", schedule);
        
        ObjHandler::obj_ptr leg(
            new QuantLibAddin::FloatingRateCouponVector("SCHEDULE",
                                                        nominals,
                                                        "IDX",
                                                        spreads));
        ObjHandler::storeObject("LEG", leg);
        
        ObjHandler::obj_ptr capFloor(
            new QuantLibAddin::CapFloor("LEG",
                                        "YC",
                                        cStrikes,
                                        fStrikes,
                                        "ENGINE",
                                        "CAP"));
        ObjHandler::storeObject("CAP", capFloor);

        std::ostringstream msg;
        msg << "NPV: "
            << boost::dynamic_pointer_cast<QuantLibAddin::CapFloor>(capFloor)->getObject().NPV();
        ObjHandler::logMessage(msg.str());
        
        ObjHandler::logMessage("end capfloor test");
        
        return 0;
    
    } catch (const std::exception &e) {
        std::ostringstream msg; msg << "Error: " << e.what();
        ObjHandler::logMessage(msg.str(), 1);
        return 1;
    } catch (...) {
        ObjHandler::logMessage("unknown error", 1);
        return 1;
    }
}


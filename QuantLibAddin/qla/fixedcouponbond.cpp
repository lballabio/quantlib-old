
/*
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

#if defined(HAVE_CONFIG_H)
    #include <qla/config.hpp>
#endif

#include <qla/fixedcouponbond.hpp>
#include <qla/generalutils.hpp>

// indexes to the Property vector
// FIXME - need a cleaner way to achieve this
#define CLEAN_PRICE                     "CLEAN_PRICE"
#define DIRTY_PRICE                     "DIRTY_PRICE"
#define IDX_CLEAN_PRICE                 0
#define IDX_DIRTY_PRICE                 1

namespace QuantLibAddin {

    FixedCouponBond::FixedCouponBond(ObjHandler::ArgStack& args) {
        std::string calendarID    = ObjHandler::Args<std::string>::popArg(args);
        std::string dayCounterID  = ObjHandler::Args<std::string>::popArg(args);
        std::string frequencyID   = ObjHandler::Args<std::string>::popArg(args);
        double yield              = ObjHandler::Args<double>::popArg(args);
        std::vector < double > coupons 
            = ObjHandler::Args< std::vector < double > >::popArg(args);
        long settlementDays       = ObjHandler::Args<long>::popArg(args);
        long maturityDate         = ObjHandler::Args<long>::popArg(args);
        long datedDate            = ObjHandler::Args<long>::popArg(args);
        long issueDate            = ObjHandler::Args<long>::popArg(args);

        // BusinessDayConvention convention = Following;
        // Real redemption = 100.0;
        // Handle<YieldTermStructure> discountCurve
        //                       = Handle<YieldTermStructure>();
        // Date stub = Date();
        // bool fromEnd = true;

        QuantLib::Frequency couponFrequency = IDtoFrequency(frequencyID);
        QuantLib::DayCounter dayCounter     = IDtoDayCounter(dayCounterID);
        QuantLib::Calendar calendar         = IDtoCalendar(calendarID);
        const std::vector<QuantLib::Rate> couponsQL = 
            doubleVectorToRateVector(coupons);

        myFixedCouponBond = 
            boost::shared_ptr<QuantLib::FixedCouponBond>(
                new QuantLib::FixedCouponBond(QuantLib::Date(issueDate),
                                              QuantLib::Date(datedDate),
                                              QuantLib::Date(maturityDate),
                                              settlementDays,
                                              couponsQL,
                                              couponFrequency,
                                              dayCounter,
                                              calendar));

        // Perform pricing
        double cleanPrice = myFixedCouponBond->cleanPrice(yield);
        double dirtyPrice = myFixedCouponBond->dirtyPrice(yield);

        // Setup object properties
        ObjHandler::any_ptr anyCleanPrice(new boost::any(cleanPrice));
        ObjHandler::any_ptr anyDirtyPrice(new boost::any(dirtyPrice));

        ObjHandler::ObjectProperty propCleanPrice(CLEAN_PRICE, anyCleanPrice);
        ObjHandler::ObjectProperty propDirtyPrice(DIRTY_PRICE, anyDirtyPrice);

        properties_.push_back(propCleanPrice);
        properties_.push_back(propDirtyPrice);
    }
}

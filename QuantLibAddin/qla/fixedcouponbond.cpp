
/*
 Copyright (C) 2005 Plamen Neykov
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
#include <qla/termstructures.hpp>
#include <qla/enumfactory.hpp>

namespace QuantLibAddin {

    FixedCouponBond::FixedCouponBond(ObjHandler::ArgumentStack& arguments) {
        std::string discCurveId      = OH_POP_ARGUMENT(std::string, arguments);
        bool longFinal               = OH_POP_ARGUMENT(bool, arguments);
        bool startFromEnd            = OH_POP_ARGUMENT(bool, arguments);
        std::string calendarID       = OH_POP_ARGUMENT(std::string, arguments);
        std::string bDayConvID       = OH_POP_ARGUMENT(std::string, arguments);
        std::string dayCounterID     = OH_POP_ARGUMENT(std::string, arguments);
        std::string frequencyID      = OH_POP_ARGUMENT(std::string, arguments);
        double redemption            = OH_POP_ARGUMENT(double, arguments);
        std::vector<double> nominals = OH_POP_ARGUMENT(std::vector<double>, arguments);
        std::vector<double> coupons  = OH_POP_ARGUMENT(std::vector<double>, arguments);
        long settlementDays          = OH_POP_ARGUMENT(long, arguments);
        long maturityDate            = OH_POP_ARGUMENT(long, arguments);
        long datedDate               = OH_POP_ARGUMENT(long, arguments);
        long issueDate               = OH_POP_ARGUMENT(long, arguments);

        QuantLib::Frequency couponFrequency = CREATE_ENUM(QuantLib::Frequency, frequencyID);
        QuantLib::DayCounter dayCounter     = CREATE_ENUM(QuantLib::DayCounter, dayCounterID);
        QuantLib::Calendar calendar         = CREATE_ENUM(QuantLib::Calendar, calendarID);
        QuantLib::BusinessDayConvention bDayConv = 
            CREATE_ENUM(QuantLib::BusinessDayConvention, bDayConvID);

        boost::shared_ptr<QuantLibAddin::YieldTermStructure> tmpDiscYC =
            OH_GET_OBJECT(QuantLibAddin::YieldTermStructure, discCurveId);
        QuantLib::Handle<QuantLib::YieldTermStructure> discountingTermStructure;
        if(tmpDiscYC) {
            boost::shared_ptr<QuantLib::YieldTermStructure> discYC = 
                OH_GET_REFERENCE(QuantLib::YieldTermStructure, tmpDiscYC);
            discountingTermStructure.linkTo(discYC);
        }

        myFixedCouponBond = boost::shared_ptr<QuantLib::FixedCouponBond>(
            new QuantLib::FixedCouponBond(QuantLib::Date(issueDate),
                                            QuantLib::Date(datedDate),
                                            QuantLib::Date(maturityDate),
                                            settlementDays,
                                            coupons,
                                            couponFrequency,
                                            dayCounter,
                                            calendar,
                                            bDayConv,
                                            redemption,
                                            discountingTermStructure,
                                            QuantLib::Date(),
                                            startFromEnd
#ifdef LOCAL_QL_PATCH
                                            ,
                                            longFinal, // Not yet implemented in QL
                                            nominals // Not yet implemented in QL
#endif
                                            ));
    }

    double 
    FixedCouponBond::cleanPrice(double yield, const std::string &compounding, long settlementDate) const {
        QuantLib::Compounding comp = CREATE_ENUM(QuantLib::Compounding, compounding);
        QuantLib::Date sett;
        if(settlementDate) sett = QuantLib::Date(settlementDate);
        return myFixedCouponBond->cleanPrice(yield, comp, sett);
    }
    double 
    FixedCouponBond::dirtyPrice(double yield, const std::string &compounding, long settlementDate) const {
        QuantLib::Compounding comp = CREATE_ENUM(QuantLib::Compounding, compounding);
        QuantLib::Date sett;
        if(settlementDate) sett = QuantLib::Date(settlementDate);
        return myFixedCouponBond->dirtyPrice(yield, comp, sett);
    }
    double 
    FixedCouponBond::yield(double cleanPrice, const std::string &compounding, long settlementDate) const {
        QuantLib::Compounding comp = CREATE_ENUM(QuantLib::Compounding, compounding);
        QuantLib::Date sett;
        if(settlementDate) sett = QuantLib::Date(settlementDate);
        return myFixedCouponBond->yield(cleanPrice, comp, sett);
    }
    double 
    FixedCouponBond::accruedAmount(long  d) const {
        QuantLib::Date sett;
        if(d) sett = QuantLib::Date(d);
        return myFixedCouponBond->accruedAmount(sett);
    }
}




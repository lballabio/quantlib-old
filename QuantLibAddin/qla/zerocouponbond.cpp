
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

#include <qla/zerocouponbond.hpp>
#include <qla/zerocurve.hpp>
#include <qla/generalutils.hpp>

namespace QuantLibAddin {

    ZeroCouponBond::ZeroCouponBond(ObjHandler::ArgStack& args) {
        
        std::string handleZeroCurve = ObjHandler::Args<std::string>::popArg(args);        

        boost::shared_ptr<ZeroCurve> zeroCurve =
            boost::dynamic_pointer_cast<ZeroCurve>(QL_OBJECT_GET(handleZeroCurve));
        
        if (!zeroCurve)
            QL_FAIL("ZeroCouponBond: error retrieving object " + handleZeroCurve);

        boost::shared_ptr<QuantLib::YieldTermStructure> zeroCurveQl
            = boost::static_pointer_cast<QuantLib::YieldTermStructure>
            (zeroCurve->getReference());

        double redemption = ObjHandler::Args<double>::popArg(args);
        std::string conventionID  = ObjHandler::Args<std::string>::popArg(args);
        std::string calendarID  = ObjHandler::Args<std::string>::popArg(args);
        std::string dayCounterID  = ObjHandler::Args<std::string>::popArg(args);
        long settlementDays = ObjHandler::Args<long>::popArg(args);
        long maturityDate = ObjHandler::Args<long>::popArg(args);
        long issueDate = ObjHandler::Args<long>::popArg(args);

        QuantLib::BusinessDayConvention convention = IDtoConvention(conventionID);
        QuantLib::Calendar calendar = IDtoCalendar(calendarID);
        QuantLib::DayCounter dayCounter = IDtoDayCounter(dayCounterID);
        
        zeroCouponBond_ = 
            boost::shared_ptr<QuantLib::ZeroCouponBond>(
                new QuantLib::ZeroCouponBond(QuantLib::Date(issueDate),
                                             QuantLib::Date(maturityDate),
                                             settlementDays,
                                             dayCounter,
                                             calendar,
                                             convention,
                                             redemption,
                                             QuantLib::Handle<QuantLib::YieldTermStructure>(zeroCurveQl)));

        // Perform pricing
        double npv = zeroCouponBond_->NPV();

        // Setup object properties
        ObjHandler::any_ptr anyNpv(new boost::any(npv));
        ObjHandler::ObjectProperty propNpv(FIELD_NPV, anyNpv);
        properties_.push_back(propNpv);
    }
}


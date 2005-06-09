
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

#include <qla/zerocouponbond.hpp>
#include <qla/zerocurve.hpp>
#include <qla/generalutils.hpp>
#include <qla/enumfactory.hpp>

// indexes to the Property vector
// FIXME - need a cleaner way to achieve this
#define FIELD_NPV "NPV"
#define IDX_NPV   0

namespace QuantLibAddin {

    ZeroCouponBond::ZeroCouponBond(ObjHandler::ArgumentStack& arguments) {
        
        std::string handleZeroCurve = OH_POP_ARGUMENT(std::string, arguments);

        boost::shared_ptr<ZeroCurve> zeroCurve =
            OH_GET_OBJECT(ZeroCurve, handleZeroCurve);
        
        if (!zeroCurve)
            QL_FAIL("ZeroCouponBond: error retrieving object " + handleZeroCurve);

        boost::shared_ptr<QuantLib::YieldTermStructure> zeroCurveQl =
            OH_GET_REFERENCE(QuantLib::YieldTermStructure, zeroCurve);

        double redemption           = OH_POP_ARGUMENT(double, arguments);
        std::string conventionID    = OH_POP_ARGUMENT(std::string, arguments);
        std::string calendarID      = OH_POP_ARGUMENT(std::string, arguments);
        std::string dayCounterID    = OH_POP_ARGUMENT(std::string, arguments);
        long settlementDays         = OH_POP_ARGUMENT(long, arguments);
        long maturityDate           = OH_POP_ARGUMENT(long, arguments);
        long issueDate              = OH_POP_ARGUMENT(long, arguments);

		QuantLib::BusinessDayConvention convention = 
			CreateEnum<QuantLib::BusinessDayConvention>::create(conventionID);
		QuantLib::Calendar calendar = CreateEnum<QuantLib::Calendar>::create(calendarID);
		QuantLib::DayCounter dayCounter = CreateEnum<QuantLib::DayCounter>::create(dayCounterID);
        
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



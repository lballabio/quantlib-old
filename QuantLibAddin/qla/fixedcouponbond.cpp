
/*
 Copyright (C) 2005 Eric Ehlers
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
#include <ql/Instruments/fixedcouponbond.hpp>
#include <qla/typefactory.hpp>

namespace QuantLibAddin {

    FixedCouponBond::FixedCouponBond(
            const long &issueDate,
            const long &datedDate,
            const long &maturityDate,
            const long &settlementDays,
            const std::vector<double> &coupons,
            const std::vector<double> &nominals,
            const double &redemption,
            const std::string &frequencyID,
            const std::string &dayCounterID,
            const std::string &bDayConvID,
            const std::string &calendarID,
            const bool &startFromEnd,
            const bool &longFinal,
            const std::string &discCurveId) {

        QuantLib::Frequency couponFrequency = Create<QuantLib::Frequency>()(frequencyID);
        QuantLib::DayCounter dayCounter     = Create<QuantLib::DayCounter>()(dayCounterID);
        QuantLib::Calendar calendar         = Create<QuantLib::Calendar>()(calendarID);
        QuantLib::BusinessDayConvention bDayConv = 
            Create<QuantLib::BusinessDayConvention>()(bDayConvID);

        boost::shared_ptr<QuantLibAddin::YieldTermStructure> tmpDiscYC;
        QuantLib::Handle<QuantLib::YieldTermStructure> discountingTermStructure;
        if (!discCurveId.empty()) {
            boost::shared_ptr<QuantLibAddin::YieldTermStructure> tmpDiscYC =
                OH_GET_OBJECT(QuantLibAddin::YieldTermStructure, discCurveId);
            boost::shared_ptr<QuantLib::YieldTermStructure> discYC = 
                OH_GET_REFERENCE(QuantLib::YieldTermStructure, tmpDiscYC);
            discountingTermStructure.linkTo(discYC);
        }

        mInstrument = boost::shared_ptr<QuantLib::Instrument>(
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
                                            startFromEnd,
                                            longFinal
#ifdef LOCAL_QL_PATCH
                                            ,
                                            nominals // Not yet implemented in QL
#endif
                                            ));
    }

}


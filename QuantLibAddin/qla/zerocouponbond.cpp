
/*
 Copyright (C) 2005, 2006 Eric Ehlers
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
#include <qla/typefactory.hpp>
#include <ql/Instruments/zerocouponbond.hpp>

namespace QuantLibAddin {

    ZeroCouponBond::ZeroCouponBond(
            const long &issueDate,
            const long &maturityDate,
            const long &settlementDays,
            const std::string &dayCounterID,
            const std::string &calendarID,
            const std::string &conventionID,
            const double &redemption,
            const std::string &handleZeroCurve) {

        OH_GET_REFERENCE(zeroCurve, handleZeroCurve, 
            ZeroCurve, QuantLib::YieldTermStructure)
        QuantLib::Handle<QuantLib::YieldTermStructure> zeroCurveH(zeroCurve);

        QuantLib::BusinessDayConvention convention = 
            Create<QuantLib::BusinessDayConvention>()(conventionID);
        QuantLib::Calendar calendar =
            Create<QuantLib::Calendar>()(calendarID);
        QuantLib::DayCounter dayCounter =
            Create<QuantLib::DayCounter>()(dayCounterID);

        mInstrument = 
            boost::shared_ptr<QuantLib::Instrument>(
                new QuantLib::ZeroCouponBond(QuantLib::Date(issueDate),
                                             QuantLib::Date(maturityDate),
                                             settlementDays,
                                             dayCounter,
                                             calendar,
                                             convention,
                                             redemption,
                                             zeroCurveH));

    }
}


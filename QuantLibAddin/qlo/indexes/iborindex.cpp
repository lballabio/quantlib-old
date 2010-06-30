/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2006, 2007, 2009 Ferdinando Ametrano
 Copyright (C) 2006 Katiuscia Manzoni
 Copyright (C) 2005 Eric Ehlers
 Copyright (C) 2005 Plamen Neykov

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

#if defined(HAVE_CONFIG_H)
    #include <qlo/config.hpp>
#endif

#include <qlo/indexes/iborindex.hpp>
#include <ql/experimental/coupons/proxyibor.hpp>

namespace QuantLibAddin {

    IborIndex::IborIndex(
                 const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
                 const std::string& familyName,
                 const QuantLib::Period& p,
                 const QuantLib::Natural fixingDays,
                 const QuantLib::Currency& crr,
                 const QuantLib::Calendar& calendar,
                 QuantLib::BusinessDayConvention fltBDC,
                 bool endOfMonth,
                 const QuantLib::DayCounter& fltDayCounter,
                 const QuantLib::Handle<QuantLib::YieldTermStructure>& hYTS,
                 bool permanent)
    : InterestRateIndex(properties, permanent)
    {
        libraryObject_ = boost::shared_ptr<QuantLib::IborIndex>(new
            QuantLib::IborIndex(familyName, 
                                p,
                                fixingDays, crr, calendar, 
                                fltBDC, endOfMonth, fltDayCounter,
                                hYTS));
    }

    OvernightIndex::OvernightIndex(
                 const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
                 const std::string& familyName,
                 const QuantLib::Natural fixingDays,
                 const QuantLib::Currency& crr,
                 const QuantLib::Calendar& calendar,
                 const QuantLib::DayCounter& fltDayCounter,
                 const QuantLib::Handle<QuantLib::YieldTermStructure>& hYTS,
                 bool permanent)
    : IborIndex(properties, permanent)
    {
        libraryObject_ = boost::shared_ptr<QuantLib::OvernightIndex>(new
            QuantLib::OvernightIndex(familyName,
                                     fixingDays, crr, calendar, 
                                     fltDayCounter,
                                     hYTS));
    }

    ProxyIbor::ProxyIbor(
                 const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
                 const std::string& familyName,
                 const QuantLib::Period& tenor,
                 QuantLib::Natural settlementDays,
                 const QuantLib::Currency& currency,
                 const QuantLib::Calendar& fixingCalendar,
                 QuantLib::BusinessDayConvention convention,
                 bool endOfMonth,
                 const QuantLib::DayCounter& dayCounter,
                 QuantLib::Real gearing,
                 const boost::shared_ptr<QuantLib::IborIndex>& iborIndex,
                 QuantLib::Spread spread,
                 bool permanent)
    : IborIndex(properties, permanent)
    {
        libraryObject_ = boost::shared_ptr<QuantLib::ProxyIbor>(new
            QuantLib::ProxyIbor(familyName,
                                tenor,
                                settlementDays,
                                currency,
                                fixingCalendar,
                                convention,
                                endOfMonth,
                                dayCounter,
                                gearing,
                                iborIndex,
                                spread));
    }

}

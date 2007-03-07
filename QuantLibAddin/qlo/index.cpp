
/*
 Copyright (C) 2006 Ferdinando Ametrano
 Copyright (C) 2006 Katiuscia Manzoni
 Copyright (C) 2005 Eric Ehlers
 Copyright (C) 2005 Plamen Neykov

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
    #include <qlo/config.hpp>
#endif

#include <qlo/index.hpp>
#include <ql/Indexes/swapindex.hpp>

namespace QuantLibAddin {

    void Index::addFixings(const std::vector<QuantLib::Date>& dates,
                           const std::vector<QuantLib::Real>& values) {
        QL_REQUIRE(dates.size()==values.size(),
                   "size mismatch between dates (" << dates.size() <<
                   ") and values (" << values.size() << ")");
        libraryObject_->addFixings(dates.begin(), dates.end(),
                                   values.begin());
    }

    IborIndex::IborIndex(const std::string& indexName,
                 const QuantLib::Period& p,
                 const QuantLib::Natural fixingDays,
                 const QuantLib::Currency& crr,
                 const QuantLib::Calendar& calendar,
                 QuantLib::BusinessDayConvention fltBDC,
                 bool endOfMonth,
                 const QuantLib::DayCounter& fltDayCounter,
                 const QuantLib::Handle<QuantLib::YieldTermStructure>& hYTS)
    {
        libraryObject_ = boost::shared_ptr<QuantLib::Index>(new
            QuantLib::IborIndex(indexName, 
                                p,
                                fixingDays, crr, calendar, 
                                fltBDC, endOfMonth, fltDayCounter,
                                hYTS));
    }

    SwapIndex::SwapIndex(const std::string& familyName,
                         const QuantLib::Period& p,
                         QuantLib::Natural fixingDays,
                         QuantLib::Currency& crr,
                         const QuantLib::Calendar& calendar,
                         const QuantLib::Period& fixedLegTenor,
                         QuantLib::BusinessDayConvention fixedLegBDC,
                         const QuantLib::DayCounter& fixedLegDayCounter,
                         const boost::shared_ptr<QuantLib::IborIndex>& index)
    {
        libraryObject_ = boost::shared_ptr<QuantLib::Index>(new
            QuantLib::SwapIndex(familyName, p,
                                fixingDays, crr, calendar, 
                                fixedLegTenor, fixedLegBDC,
                                fixedLegDayCounter, index));
    }

}

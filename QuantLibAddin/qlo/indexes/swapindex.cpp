/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2006, 2007, 2008 Ferdinando Ametrano
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

#include <qlo/indexes/swapindex.hpp>
#include <ql/indexes/swapindex.hpp>

namespace QuantLibAddin {

    SwapIndex::SwapIndex(const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
                         const std::string& familyName,
                         const QuantLib::Period& p,
                         QuantLib::Natural fixingDays,
                         QuantLib::Currency& crr,
                         const QuantLib::Calendar& calendar,
                         const QuantLib::Period& fixedLegTenor,
                         QuantLib::BusinessDayConvention fixedLegBDC,
                         const QuantLib::DayCounter& fixedLegDayCounter,
                         const boost::shared_ptr<QuantLib::IborIndex>& index,
                         bool permanent) : InterestRateIndex(properties, permanent)
    {
        libraryObject_ = boost::shared_ptr<QuantLib::SwapIndex>(new
            QuantLib::SwapIndex(familyName, p,
                                fixingDays, crr, calendar, 
                                fixedLegTenor, fixedLegBDC,
                                fixedLegDayCounter, index));
    }

    std::ostream& operator<<(std::ostream& out,
                             SwapIndex::FixingType t) {
        switch (t) {
          case SwapIndex::IsdaFixA:
            return out << "IsdaFixA";
          case SwapIndex::IsdaFixB:
            return out << "IsdaFixB";
          case SwapIndex::IfrFix:
            return out << "IfrFix";
          case SwapIndex::IsdaFixAm:
            return out << "IsdaFixAm";
          case SwapIndex::IsdaFixPm:
            return out << "IsdaFixPm";
          default:
            QL_FAIL("unknown SwapIndex::FixingType(" << QuantLib::Integer(t) << ")");
        }
    }

}

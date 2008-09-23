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

#ifndef qla_swapindex_hpp
#define qla_swapindex_hpp

#include <qlo/indexes/interestrateindex.hpp>

#include <ql/time/businessdayconvention.hpp>

namespace QuantLib {
    class Period;
    class Currency;
    class Calendar;
    class DayCounter;
    class IborIndex;
    class YieldTermStructure;

    template <class T>
    class Handle;
}

namespace QuantLibAddin {

    class SwapIndex : public InterestRateIndex {
      public:
        enum FixingType {Isda,
                         IsdaFixA,
                         IsdaFixB,
                         IsdaFixAm,
                         IsdaFixPm,
                         IfrFix
        };
        SwapIndex(const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
                  const std::string& familyName,
                  const QuantLib::Period& p,
                  QuantLib::Natural fixingDays,
                  QuantLib::Currency& crr,
                  const QuantLib::Calendar& calendar,
                  const QuantLib::Period& fixedLegTenor,
                  QuantLib::BusinessDayConvention fixedLegBDC,
                  const QuantLib::DayCounter& fixedLegDayCounter,
                  const boost::shared_ptr<QuantLib::IborIndex>& index,
                  bool permanent);
      protected:
        OH_OBJ_CTOR(SwapIndex, InterestRateIndex);
    };

    std::ostream& operator<<(std::ostream& out,
                             SwapIndex::FixingType t);
}

#endif

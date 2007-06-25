
/*
 Copyright (C) 2006 Ferdinando Ametrano
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

#ifndef qla_xibor_hpp
#define qla_xibor_hpp

#include <oh/libraryobject.hpp>

#include <ql/time/businessdayconvention.hpp>
#include <ql/handle.hpp>
#include <ql/types.hpp>

#include <string>

namespace QuantLib {
    class Date;
    class Period;
    class Index;
    class Currency;
    class Calendar;
    class DayCounter;
    class IborIndex;
    class YieldTermStructure;
}

namespace QuantLibAddin {

    class Index : public ObjectHandler::LibraryObject<QuantLib::Index> {
      public:
          void addFixings(const std::vector<QuantLib::Date>& dates,
			              const std::vector<QuantLib::Real>& values);
	};

    class InterestRateIndex : public Index {};

    class IborIndex : public InterestRateIndex {
      public:
        IborIndex(const std::string& indexName,
                  const QuantLib::Period& p,
                  const QuantLib::Natural fixingDays,
                  const QuantLib::Currency& crr,
                  const QuantLib::Calendar& calendar,
                  QuantLib::BusinessDayConvention fltBDC,
                  bool endOfMonth,
                  const QuantLib::DayCounter& fltDayCounter,
                  const QuantLib::Handle<QuantLib::YieldTermStructure>& hYTS);
    };

    class SwapIndex : public InterestRateIndex {
      public:
        SwapIndex(const std::string& familyName,
                  const QuantLib::Period& p,
                  QuantLib::Natural fixingDays,
                  QuantLib::Currency& crr,
                  const QuantLib::Calendar& calendar,
                  const QuantLib::Period& fixedLegTenor,
                  QuantLib::BusinessDayConvention fixedLegBDC,
                  const QuantLib::DayCounter& fixedLegDayCounter,
                  const boost::shared_ptr<QuantLib::IborIndex>& index);
    };

}

#endif

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

#ifndef qla_iborindex_hpp
#define qla_iborindex_hpp

#include <qlo/indexes/interestrateindex.hpp>

#include <ql/time/businessdayconvention.hpp>

namespace QuantLib {
    class Period;
    class Currency;
    class Calendar;
    class DayCounter;
    class YieldTermStructure;
    class IborIndex;
    class Quote;

    template <class T>
    class Handle;
}

namespace QuantLibAddin {

    class IborIndex : public InterestRateIndex {
      public:
        IborIndex(const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
                  const std::string& familyName,
                  const QuantLib::Period& p,
                  const QuantLib::Natural fixingDays,
                  const QuantLib::Currency& crr,
                  const QuantLib::Calendar& calendar,
                  QuantLib::BusinessDayConvention fltBDC,
                  bool endOfMonth,
                  const QuantLib::DayCounter& fltDayCounter,
                  const QuantLib::Handle<QuantLib::YieldTermStructure>& hYTS,
                  bool permanent);
      protected:
        OH_OBJ_CTOR(IborIndex, InterestRateIndex);
    };

    class OvernightIndex : public IborIndex {
      public:
        OvernightIndex(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const std::string& familyName,
            const QuantLib::Natural fixingDays,
            const QuantLib::Currency& crr,
            const QuantLib::Calendar& calendar,
            const QuantLib::DayCounter& fltDayCounter,
            const QuantLib::Handle<QuantLib::YieldTermStructure>& hYTS,
            bool permanent);
      protected:
        OH_OBJ_CTOR(OvernightIndex, IborIndex);
    };

    class ProxyIbor : public IborIndex {
      public:
        ProxyIbor(const boost::shared_ptr<ObjectHandler::ValueObject>& prop,
                  const std::string& familyName,
                  const QuantLib::Period& tenor,
                  QuantLib::Natural settlementDays,
                  const QuantLib::Currency& currency,
                  const QuantLib::Calendar& fixingCalendar,
                  QuantLib::BusinessDayConvention convention,
                  bool endOfMonth,
                  const QuantLib::DayCounter& dayCounter,
                  const QuantLib::Handle<QuantLib::Quote>& gearing,
                  const boost::shared_ptr<QuantLib::IborIndex>& iborIndex,
                  const QuantLib::Handle<QuantLib::Quote>& spread,
                  bool permanent);
      protected:
        OH_OBJ_CTOR(ProxyIbor, IborIndex);
    };

}

#endif

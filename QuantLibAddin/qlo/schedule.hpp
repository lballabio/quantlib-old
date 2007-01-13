
/*
 Copyright (C) 2005 Aurelien Chanudet

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

#ifndef qla_schedule_hpp
#define qla_schedule_hpp

#include <oh/objhandler.hpp>
#include <ql/schedule.hpp>

namespace QuantLibAddin {

    class Schedule : public ObjHandler::LibraryObject<QuantLib::Schedule> {
      public:
        Schedule(const QuantLib::Date& effectiveDate,
                 const QuantLib::Date& terminationDate,
                 const QuantLib::Period& tenor,
                 const QuantLib::Calendar& calendar,
                 QuantLib::BusinessDayConvention convention,
                 QuantLib::BusinessDayConvention terminationDateConvention,
                 bool backward,
                 bool endOfMonth,
                 const QuantLib::Date& firstDate,
                 const QuantLib::Date& nextToLastDate);
    };

}

#endif


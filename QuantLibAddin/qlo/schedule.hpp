/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2011 Ferdinando Ametrano
 Copyright (C) 2005 Aurelien Chanudet

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

#ifndef qla_schedule_hpp
#define qla_schedule_hpp

#include <oh/libraryobject.hpp>

#include <ql/time/businessdayconvention.hpp>
#include <ql/time/dategenerationrule.hpp>

namespace QuantLib {
    class Schedule;
    class Date;
    class Period;
    class Calendar;
}

namespace QuantLibAddin {

    class Schedule : public ObjectHandler::LibraryObject<QuantLib::Schedule> {
      public:
        Schedule(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const QuantLib::Date& effectiveDate,
            const QuantLib::Date& terminationDate,
            const QuantLib::Period& tenor,
            const QuantLib::Calendar& calendar,
            QuantLib::BusinessDayConvention convention,
            QuantLib::BusinessDayConvention terminationDateConvention,
            QuantLib::DateGeneration::Rule rule,
            bool endOfMonth,
            const QuantLib::Date& firstDate,
            const QuantLib::Date& nextToLastDate,
            bool permanent);
        Schedule(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const std::vector<QuantLib::Date>& dates,
            bool permanent);
        Schedule(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const boost::shared_ptr<QuantLib::Schedule>& originalSchedule,
            const QuantLib::Date& truncationDate,
            bool permanent);
    };

}

#endif

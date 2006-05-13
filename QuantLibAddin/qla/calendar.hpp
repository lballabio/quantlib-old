
/*
 Copyright (C) 2006 Eric Ehlers

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

#ifndef qla_calendar_hpp
#define qla_calendar_hpp

#include <oh/object.hpp>
#include <ql/Calendars/jointcalendar.hpp>
#include <ql/Calendars/nullcalendar.hpp>

namespace QuantLibAddin {

    class JointCalendar : public ObjHandler::Object {
    public:
        JointCalendar(
            const QuantLib::JointCalendarRule &jointCalendarRule,
            const std::vector < std::string > &calendars);
        virtual boost::shared_ptr<void> getReference() const {
            return boost::static_pointer_cast<void>(jointCalendar_);
        }
    protected:
        boost::shared_ptr<QuantLib::JointCalendar> jointCalendar_;
    };

}

#endif


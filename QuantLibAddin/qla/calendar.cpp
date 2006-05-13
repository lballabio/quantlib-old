
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

#if defined(HAVE_CONFIG_H)     // Dynamically created by configure
    #include <qla/config.hpp>
#endif
#include <oh/objhandlerdefines.hpp>
#include <qla/calendar.hpp>
#include <qla/typefactory.hpp>
#include <oh/exception.hpp>

namespace QuantLibAddin {

    JointCalendar::JointCalendar(
        const QuantLib::JointCalendarRule &jointCalendarRule,
        const std::vector < std::string > &calendars) {
            if (calendars.size() == 2) {
                jointCalendar_ = boost::shared_ptr<QuantLib::JointCalendar>(
                    new QuantLib::JointCalendar(
                        Create<QuantLib::Calendar>()(calendars[0]),
                        Create<QuantLib::Calendar>()(calendars[1]),
                        jointCalendarRule));
            } else if (calendars.size() == 3) {
                jointCalendar_ = boost::shared_ptr<QuantLib::JointCalendar>(
                    new QuantLib::JointCalendar(
                        Create<QuantLib::Calendar>()(calendars[0]),
                        Create<QuantLib::Calendar>()(calendars[1]),
                        Create<QuantLib::Calendar>()(calendars[2]),
                        jointCalendarRule));
            } else if (calendars.size() == 4) {
                jointCalendar_ = boost::shared_ptr<QuantLib::JointCalendar>(
                    new QuantLib::JointCalendar(
                        Create<QuantLib::Calendar>()(calendars[0]),
                        Create<QuantLib::Calendar>()(calendars[1]),
                        Create<QuantLib::Calendar>()(calendars[2]),
                        Create<QuantLib::Calendar>()(calendars[3]),
                        jointCalendarRule));
            } else {
                std::ostringstream err;
                err << "Error constructing JointCalendar object - list of calendars "
                    "to be joined may contain two, three or four items - "
                    << calendars.size() << " item(s) provided.";
                throw ObjHandler::Exception(err.str());
            }
    }

}

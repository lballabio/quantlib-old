/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2006 The QuantLib Group

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

#ifndef qla_calendarfactory_hpp
#define qla_calendarfactory_hpp

#include <oh/Enumerations/typefactory.hpp>
#include <ql/time/calendars/jointcalendar.hpp>

namespace ObjectHandler {

    template<>
    class Create<QuantLib::Calendar> :
        private RegistryManager<QuantLib::Calendar, EnumTypeRegistry> {
    public:
        QuantLib::Calendar operator()(const std::string& id);
        using RegistryManager<QuantLib::Calendar, EnumTypeRegistry>::checkType;
        using RegistryManager<QuantLib::Calendar, EnumTypeRegistry>::registerType;
    private:
        std::vector<std::string> calendarIDs;
        QuantLib::JointCalendarRule jointCalendarRule;
        std::string idOriginal, idUpper, idFull;
        bool testID();
        void parseID();
        QuantLib::Calendar *makeJointCalendar(const unsigned int&);
        QuantLib::Calendar *makeJointCalendar2();
        QuantLib::Calendar *makeJointCalendar3();
        QuantLib::Calendar *makeJointCalendar4();
    };

 }

#endif


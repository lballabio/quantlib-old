
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

#if defined(HAVE_CONFIG_H)     // Dynamically created by configure
    #include <qla/config.hpp>
#endif
#include <qla/schedule.hpp>
#include <qla/typefactory.hpp>

namespace QuantLibAddin {

    Schedule::Schedule(
        const boost::shared_ptr < InstanceName > &instanceName,
        const std::string   &calendarID,
        const long          &lStartDate,
        const long          &lEndDate,
        const std::string   &frequencyID,
        const std::string   &conventionID,
     // const long          &lStubDate,
        const bool          &startFromEnd,
        const bool          &longFinal)  : ObjHandler::Object(instanceName) {
        
        QuantLib::Date startDate (lStartDate);
        QuantLib::Date endDate   (lEndDate);
        QuantLib::Date stubDate/*(lStubDate)*/;
        
        QuantLib::Calendar calendar =
            Create<QuantLib::Calendar>()(calendarID);
        QuantLib::Frequency frequency =
            Create<QuantLib::Frequency>()(frequencyID);
        QuantLib::BusinessDayConvention convention =
            Create<QuantLib::BusinessDayConvention>()(conventionID);
        
        schedule_ = boost::shared_ptr<QuantLib::Schedule>(
            new QuantLib::Schedule(calendar,
                                   startDate,
                                   endDate,
                                   frequency,
                                   convention,
                                   stubDate,
                                   startFromEnd,
                                   longFinal));
    }
    
    std::vector<long> Schedule::scheduleDates() const {
        
        std::vector<long> dates;
        
        for (std::size_t i=0 ; i < schedule_->size() ; i++) {
            dates.push_back(schedule_->date(i).serialNumber());
        }
        
        return dates;
    }
    
}


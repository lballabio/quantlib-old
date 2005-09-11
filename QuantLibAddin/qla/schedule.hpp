
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

    class Schedule : public ObjHandler::Object {
      public:
        Schedule(
            const std::string   &calendarID,
            const long          &lStartDate,
            const long          &lEndDate,
            const std::string   &frequencyID,
            const std::string   &conventionID,
         // const long          &lStubDate,
            const bool          &startFromEnd,
            const bool          &longFinal);
        
        virtual boost::shared_ptr<void> getReference() const {
            return boost::static_pointer_cast<void>(schedule_);
        }
        
        const QuantLib::Schedule& getObject() const {
            return *schedule_;
        }
        
      private:
        boost::shared_ptr<QuantLib::Schedule> schedule_;
    };

}

#endif


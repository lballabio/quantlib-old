
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

#ifndef qla_capfloor_hpp
#define qla_capfloor_hpp

#include <qla/baseinstruments.hpp>
#include <ql/Instruments/capfloor.hpp>
#include <ql/PricingEngines/CapFloor/analyticcapfloorengine.hpp>

namespace QuantLibAddin {

    class CapFloor : public Instrument {
      public:
        CapFloor(
            const boost::shared_ptr < InstanceName > &instanceName,
            const std::string&         couponVectorID,
            const std::string&         termStructureID,
            const std::vector<double>& capStrikes,
            const std::vector<double>& floorStrikes,
            const std::string&         engineID,
            const std::string&         optionID);
        
        EXPORT_QL_OBJECT(QuantLib::CapFloor);
        
    };
    
    class AnalyticCapFloorEngine : public ObjHandler::Object {
      public:
        AnalyticCapFloorEngine(
            const boost::shared_ptr < InstanceName > &instanceName,
            const std::string& handleModel);
        virtual boost::shared_ptr<void> getReference() const {
            return boost::static_pointer_cast<void>(engine_);
        }
      private:
        boost::shared_ptr<QuantLib::PricingEngine> engine_;
    };

}

#endif


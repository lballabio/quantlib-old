
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

#include <ql/pricingengine.hpp>
#include <ql/Instruments/capfloor.hpp>
#include <ql/PricingEngines/CapFloor/analyticcapfloorengine.hpp>

#include <oh/objhandler.hpp>

namespace QuantLibAddin {

    class CapFloor : public ObjHandler::Object {
      public:
        CapFloor(ObjHandler::ArgumentStack& args);
        virtual boost::shared_ptr<void> getReference() const {
            return boost::static_pointer_cast<void>(capfloor_);
        }
      private:
        boost::shared_ptr<QuantLib::CapFloor> capfloor_;
    };
    
    class AnalyticCapFloorEngine : public ObjHandler::Object {
      public:
        AnalyticCapFloorEngine(ObjHandler::ArgumentStack& args);
        virtual boost::shared_ptr<void> getReference() const {
            return boost::static_pointer_cast<void>(engine_);
        }
      private:
        boost::shared_ptr<QuantLib::PricingEngine> engine_;
    };
    
}

#endif


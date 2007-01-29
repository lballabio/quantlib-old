
/*
 Copyright (C) 2006 Ferdinando Ametrano
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

#include <qlo/baseinstruments.hpp>
#include <qlo/couponvectors.hpp>
#include <ql/Instruments/capfloor.hpp>

namespace QuantLibAddin {

    class CapFloor : public Instrument {
      public:
        CapFloor(QuantLib::CapFloor::Type type,
                 const boost::shared_ptr<Leg>& floatingLegWrapper,
                 const std::vector<QuantLib::Rate>& strikes,
                 const QuantLib::Handle<QuantLib::YieldTermStructure>& termStructure,
                 const boost::shared_ptr<QuantLib::PricingEngine>& engine);
        CapFloor(QuantLib::CapFloor::Type capFloorType,
                 const QuantLib::Period& capFloorTenor, 
                 const boost::shared_ptr<QuantLib::IborIndex>& index,
                 QuantLib::Rate strike,
                 const QuantLib::Period& forwardStart,
                 const boost::shared_ptr<QuantLib::PricingEngine>& engine);
        std::vector<std::vector<boost::any> > legAnalysis();
    };
    
}

#endif


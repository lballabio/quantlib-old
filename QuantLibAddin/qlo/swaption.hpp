/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2006 Ferdinando Ametrano
 Copyright (C) 2006 Cristina Duminuco

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

#ifndef qla_swaption_hpp
#define qla_swaption_hpp

#include <qlo/baseinstruments.hpp>
#include <ql/instruments/swaption.hpp>
#include <ql/pricingengines/swaption/blackswaptionengine.hpp>

namespace QuantLib {
    class SwapIndex;
}

namespace QuantLibAddin {
    
    class Swaption : public Instrument {
      public:
        Swaption(const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
                 const boost::shared_ptr<QuantLib::VanillaSwap>& vanillaSwap,
                 const boost::shared_ptr<QuantLib::Exercise>& exercise,
                 QuantLib::Settlement::Type settlementType,
                 bool permanent);
        // MakeSwaption
        Swaption(const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
                 const boost::shared_ptr<QuantLib::SwapIndex>& swapIndex,
                 const QuantLib::Period& optionTenor,
                 QuantLib::Rate strike,
                 const boost::shared_ptr<QuantLib::PricingEngine>& engine,
                 bool permanent);

        std::string underlyingSwap() {
            return boost::any_cast<std::string>(propertyValue("VANILLASWAP"));
        }

    };
    
}

#endif

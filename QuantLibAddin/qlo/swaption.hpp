
/*
 Copyright (C) 2006 Ferdinando Ametrano
 Copyright (C) 2006 Cristina Duminuco

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

#ifndef qla_swaption_hpp
#define qla_swaption_hpp

#include <qlo/baseinstruments.hpp>
#include <ql/pricingengines/swaption/blackswaptionengine.hpp>
#include <ql/instruments/swaption.hpp>

namespace QuantLibAddin {
    
    class Swaption : public Instrument {
      public:
        Swaption(const boost::shared_ptr<QuantLib::VanillaSwap>& swap,
                 const boost::shared_ptr<QuantLib::Exercise>& exercise,
                 const QuantLib::Handle<QuantLib::YieldTermStructure>& hYTS,
                 const boost::shared_ptr<QuantLib::BlackSwaptionEngine>& engine,
                 QuantLib::Settlement::Type settlementType);
        std::string underlyingSwap()
        {
            return boost::any_cast<std::string>(propertyValue("vanillaSwap"));
        }

    };
    
}

#endif


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

#if defined(HAVE_CONFIG_H)
    #include <qlo/config.hpp>
#endif

#include <qlo/swaption.hpp>
#include <ql/Instruments/swaption.hpp>

namespace QuantLibAddin {

    Swaption::Swaption(
        const boost::shared_ptr<QuantLib::VanillaSwap>& swap,
        const boost::shared_ptr<QuantLib::Exercise>& exercise,
        const QuantLib::Handle<QuantLib::YieldTermStructure>& hYTS,
        const boost::shared_ptr<QuantLib::BlackSwaptionEngine>& engine,
        QuantLib::Settlement::Type settlementType)
    {
        libraryObject_ = boost::shared_ptr<QuantLib::Instrument>(
            new QuantLib::Swaption(swap, exercise, hYTS, engine,
                                   settlementType));
    }

}

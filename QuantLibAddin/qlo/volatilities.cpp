
/*
 Copyright (C) 2005 Plamen Neykov
 Copyright (C) 2005 Eric Ehlers

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

#if defined(HAVE_CONFIG_H)     // Dynamically created by configure
    #include <qlo/config.hpp>
#endif

#include <qlo/volatilities.hpp>

#include <ql/voltermstructures/equityfx/blackconstantvol.hpp>
#include <ql/voltermstructures/equityfx/blackvariancesurface.hpp>

namespace QuantLibAddin {

    BlackConstantVol::BlackConstantVol(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const QuantLib::Date& settlementDate,
            QuantLib::Volatility volatility,
            const QuantLib::DayCounter& dayCounter,
            bool permanent) : BlackVolTermStructure(properties, permanent)
    {
        libraryObject_ =
            boost::shared_ptr<QuantLib::BlackVolTermStructure>(new
                QuantLib::BlackConstantVol(settlementDate,
                                           volatility,
                                           dayCounter));
    }

    BlackVarianceSurface::BlackVarianceSurface(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const QuantLib::Date& settlementDate,
            const std::vector<QuantLib::Date>& dates,
            const std::vector<QuantLib::Rate>& strikes,
            const QuantLib::Matrix& vols,
            const QuantLib::DayCounter& dayCounter,
            bool permanent) : BlackVolTermStructure(properties, permanent)
    {
        libraryObject_ =
            boost::shared_ptr<QuantLib::BlackVolTermStructure>(new
                QuantLib::BlackVarianceSurface(settlementDate,
                                               dates,
                                               strikes,
                                               vols,
                                               dayCounter));
    }

}


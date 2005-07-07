
/*
 Copyright (C) 2005 Plamen Neykov
 Copyright (C) 2005 Eric Ehlers

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
#include <qla/volatilities.hpp>
#include <qla/generalutils.hpp>
#include <qla/typefactory.hpp>

namespace QuantLibAddin {

    BlackConstantVol::BlackConstantVol(ObjHandler::ArgumentStack &arguments) {
        std::string dayCounterID    = OH_POP_ARGUMENT(std::string, arguments);
        double volatility           = OH_POP_ARGUMENT(double, arguments);
        long settlementDateLong     = OH_POP_ARGUMENT(long, arguments);

        QuantLib::Date settlementDate(settlementDateLong);
		QuantLib::DayCounter dayCounter =
            Create<QuantLib::DayCounter>()(dayCounterID);
        blackVolTermStructure_ = boost::shared_ptr<QuantLib::BlackVolTermStructure> (
            new QuantLib::BlackConstantVol(
                settlementDate,
                volatility,
                dayCounter));
    }

    BlackVarianceSurface::BlackVarianceSurface(ObjHandler::ArgumentStack &arguments) {
        std::string dayCounterID    = OH_POP_ARGUMENT(std::string, arguments);
        std::vector < std::vector < double > > vols 
            = OH_POP_ARGUMENT(std::vector < std::vector < double > >, arguments);
        std::vector < double > strikes 
            = OH_POP_ARGUMENT(std::vector < double >, arguments);
        std::vector < long > dates 
            = OH_POP_ARGUMENT(std::vector < long >, arguments);
        long settlementDate         = OH_POP_ARGUMENT(long, arguments);

        QuantLib::Date settlementDateQL(settlementDate);
        const std::vector<QuantLib::Date> datesQL = 
            longVectorToDateVector(dates);
        QuantLib::Matrix volsQL =
            vectorVectorToMatrix(vols);
		QuantLib::DayCounter dayCounter =
            Create<QuantLib::DayCounter>()(dayCounterID);

        blackVolTermStructure_ = boost::shared_ptr<QuantLib::BlackVolTermStructure> (
            new QuantLib::BlackVarianceSurface(
                settlementDateQL,
                datesQL,
                strikes,
                volsQL,
                dayCounter));
    }


}

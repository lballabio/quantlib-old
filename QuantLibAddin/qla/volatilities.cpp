
/*
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

namespace QuantLibAddin {

    BlackConstantVol::BlackConstantVol(ObjHandler::ArgStack &args) {
        std::string dayCounterID    = ObjHandler::Args<std::string>::popArg(args);
        double volatility           = ObjHandler::Args<double>::popArg(args);
        long settlementDateLong     = ObjHandler::Args<long>::popArg(args);

        QuantLib::Date settlementDate(settlementDateLong);
        QuantLib::DayCounter dayCounter = IDtoDayCounter(dayCounterID);
        blackVolTermStructure_ = boost::shared_ptr<QuantLib::BlackVolTermStructure> (
            new QuantLib::BlackConstantVol(
                settlementDate,
                volatility,
                dayCounter));
    }

    BlackVarianceSurface::BlackVarianceSurface(ObjHandler::ArgStack &args) {
        std::string dayCounterID    = ObjHandler::Args<std::string>::popArg(args);
        std::vector < std::vector < double > > vols 
            = ObjHandler::Args< std::vector < std::vector < double > > >::popArg(args);
        std::vector < double > strikes 
            = ObjHandler::Args< std::vector < double > >::popArg(args);
        std::vector < long > dates 
            = ObjHandler::Args< std::vector < long > >::popArg(args);
        long settlementDate         = ObjHandler::Args<long>::popArg(args);

        QuantLib::Date settlementDateQL(settlementDate);
        const std::vector<QuantLib::Date> datesQL = 
            longVectorToDateVector(dates);
        QuantLib::Matrix volsQL =
            vectorVectorToMatrix(vols);
        QuantLib::DayCounter dayCounter = IDtoDayCounter(dayCounterID);

        blackVolTermStructure_ = boost::shared_ptr<QuantLib::BlackVolTermStructure> (
            new QuantLib::BlackVarianceSurface(
                settlementDateQL,
                datesQL,
                strikes,
                volsQL,
                dayCounter));
    }


}


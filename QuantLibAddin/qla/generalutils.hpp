
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

/*! \file generalutils.hpp
    \brief miscellaneous utility functions intended for use by files
    within the qla directory
*/

#ifndef qla_generalutils_hpp
#define qla_generalutils_hpp

#include <ql/DayCounters/all.hpp>
#include <ql/currency.hpp>
#include <ql/calendar.hpp>
#include <ql/Math/matrix.hpp>

#include <vector>

namespace QuantLibAddin {

    //std::vector<QuantLib::Date> longVectorToDateVector(
    //    const std::vector < long > &v);

    QuantLib::Matrix vectorVectorToMatrix(
        const std::vector < std::vector < double > > &vv);

    QuantLib::Date FutIDtoExpiryDate(
        const std::string& immID,
        const QuantLib::Calendar& calendar, 
        QuantLib::BusinessDayConvention bdc,
        QuantLib::Integer decade);
}

#endif



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

#ifndef qla_instrumentutils_hpp
#define qla_instrumentutils_hpp

//#include <ql/option.hpp>
//#include <ql/Instruments/payoffs.hpp>
//#include <ql/Math/matrix.hpp>
//#include <ql/basicdataformatters.hpp>
#include <qla/generalutils.hpp>
#include <ql/calendar.hpp>

// indexes to the Property vector
// FIXME - need a cleaner way to achieve this
#define CLEAN_PRICE                     "CLEAN_PRICE"
#define DIRTY_PRICE                     "DIRTY_PRICE"
#define IDX_CLEAN_PRICE                 0
#define IDX_DIRTY_PRICE                 1


namespace QuantLibAddin {

    QuantLib::Frequency IDtoFrequency(
        const std::string &frequencyID);

    QuantLib::DayCounter IDtoDayCounter(
        const std::string &dayCounterID);

    QuantLib::Calendar IDtoCalendar(
        const std::string &calendarID);

}

#endif



/*
 Copyright (C) 2005 Eric Ehlers
 Copyright (C) 2005 Walter Penschke

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
#include <oh/objhandler.hpp>
#include <qla/instrumentutils.hpp>
#include <ql/date.hpp>
#include <ql/Calendars/all.hpp>

namespace QuantLibAddin {

    QuantLib::Frequency IDtoFrequency(const std::string &frequencyID) {
        std::string idUpper = QuantLib::StringFormatter::toUppercase(frequencyID);
        if (idUpper.compare("NULL") ==0)
            return QuantLib::NoFrequency;
        else if (idUpper.compare("ONCE") == 0)
            return QuantLib::Once;
        else if (idUpper.compare("A") == 0)
            return QuantLib::Annual;
        else if (idUpper.compare("S") == 0)
            return QuantLib::Semiannual;
        else if (idUpper.compare("EVERY4M") == 0)
            return QuantLib::EveryFourthMonth;
        else if (idUpper.compare("Q") == 0)
            return QuantLib::Quarterly;
        else if (idUpper.compare("B") == 0)
            return QuantLib::Bimonthly;
        else if (idUpper.compare("M") == 0)
            return QuantLib::Monthly;
        else
            QL_FAIL("IDtoFrequency: unrecognized frequencyID: " + frequencyID);
    }

    QuantLib::Calendar IDtoCalendar(const std::string &calendarID) {
        std::string idUpper = QuantLib::StringFormatter::toUppercase(calendarID);
        if (idUpper.compare("DE") ==0)
            return QuantLib::Germany();
        else if (idUpper.compare("IT") == 0)
            return QuantLib::Italy();
        else if (idUpper.compare("TARGET") == 0)
            return QuantLib::TARGET();
        else if (idUpper.compare("TOKYO") == 0)
            return QuantLib::Tokyo();
        else if (idUpper.compare("UK") == 0)
            return QuantLib::UnitedKingdom();
        else if (idUpper.compare("US") == 0)
            return QuantLib::UnitedStates();
        else
            QL_FAIL("IDtoCalendar: unrecognized calendarID: " + calendarID);
    }

    QuantLib::BusinessDayConvention IDtoConvention(const std::string &conventionID) {
        std::string idUpper = QuantLib::StringFormatter::toUppercase(conventionID);
        if (idUpper.compare("F") ==0)
            return QuantLib::Following;
        else if (idUpper.compare("MF") == 0)
            return QuantLib::ModifiedFollowing;
        else if (idUpper.compare("MP") == 0)
            return QuantLib::ModifiedPreceding;
        else if (idUpper.compare("MER") == 0)
            return QuantLib::MonthEndReference;
        else if (idUpper.compare("P") == 0)
            return QuantLib::Preceding;
        else if (idUpper.compare("U") == 0)
            return QuantLib::Unadjusted;
        else
            QL_FAIL("IDtoConvention: unrecognized conventionID: " + conventionID);
    }

}



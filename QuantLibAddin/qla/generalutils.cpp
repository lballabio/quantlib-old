
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
#include <oh/objhandler.hpp>
#include <qla/generalutils.hpp>

namespace QuantLibAddin {

    QuantLib::DayCounter IDtoDayCounter(const std::string &dayCounterID) {
        std::string idUpper = QuantLib::StringFormatter::toUppercase(dayCounterID);
        if (idUpper.compare("ACT365FIX") ==0)
            return QuantLib::Actual365Fixed();
        else if (idUpper.compare("ACT360") == 0)
            return QuantLib::Actual360();
        else if (idUpper.compare("ACTACT") == 0)
            return QuantLib::ActualActual();
        else
            QL_FAIL("IDtoDayCounter: unrecognized dayCounterID: " + dayCounterID);
    }

}


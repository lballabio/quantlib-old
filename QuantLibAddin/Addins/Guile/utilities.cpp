
/*
 Copyright (C) 2005 Eric Ehlers
 Copyright (C) 2005 Aurelien Chanudet

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

#include <ql/date.hpp>
#include <qla/qladdin.hpp>
#include <Addins/Guile/guileutils.hpp>
extern "C" {
#include <Addins/Guile/extra.h>
#include <Addins/Guile/utilities.h>
}

SCM qlVersion(SCM x) {
    try {
        std::string ver =  QuantLibAddin::qlVersion();
        return gh_str02scm(ver.c_str());
    } catch (const std::exception &e) {
        ObjHandler::logMessage("qlVersion Error: " + std::string(e.what()), 2);
        return SCM_UNSPECIFIED;
    }
}

SCM qlConsole(SCM x) {
    try {
        int enabled = GetChop<int>::scalar(x);
        int logLevel = GetChop<int>::scalar(x);
        ObjHandler::setConsole(enabled, logLevel);
        return SCM_UNSPECIFIED;
    } catch (const std::exception &e) {
        ObjHandler::logMessage("qlConsole Error: " + std::string(e.what()), 2);
        return SCM_UNSPECIFIED;
    }
}

SCM qlDate(SCM x) {
    try {
        int day                 = GetChop<int>::scalar(x);
        std::string month       = GetChop<std::string>::scalar(x);
        int year                = GetChop<int>::scalar(x);
        std::string monthUpper  = QuantLib::uppercase(month);
        QuantLib::Month monthQL;
        if (monthUpper.compare(0, 3, "JAN") == 0) {
            monthQL = QuantLib::January;
        } else if (monthUpper.compare(0, 3, "FEB") == 0) {
            monthQL = QuantLib::February;
        } else if (monthUpper.compare(0, 3, "MAR") == 0) {
            monthQL = QuantLib::March;
        } else if (monthUpper.compare(0, 3, "APR") == 0) {
            monthQL = QuantLib::April;
        } else if (monthUpper.compare(0, 3, "MAY") == 0) {
            monthQL = QuantLib::May;
        } else if (monthUpper.compare(0, 3, "JUN") == 0) {
            monthQL = QuantLib::June;
        } else if (monthUpper.compare(0, 3, "JUL") == 0) {
            monthQL = QuantLib::July;
        } else if (monthUpper.compare(0, 3, "AUG") == 0) {
            monthQL = QuantLib::August;
        } else if (monthUpper.compare(0, 3, "SEP") == 0) {
            monthQL = QuantLib::September;
        } else if (monthUpper.compare(0, 3, "OCT") == 0) {
            monthQL = QuantLib::October;
        } else if (monthUpper.compare(0, 3, "NOV") == 0) {
            monthQL = QuantLib::November;
        } else if (monthUpper.compare(0, 3, "DEC") == 0) {
            monthQL = QuantLib::December;
        } else {
            QL_FAIL("unrecognised month");
        }
        QuantLib::Date date(day, monthQL, year);
        return gh_long2scm(date.serialNumber());
    } catch (const std::exception &e) {
        ObjHandler::logMessage("qlDate Error: " + std::string(e.what()), 2);
        return SCM_UNSPECIFIED;
    }
}

SCM qlListRegisteredEnums(SCM x) {
    try {
        std::vector < std::string > returnValue = QuantLibAddin::getRegisteredEnums();
        return Nat2Scm<std::string>::vector(returnValue);
    } catch (const std::exception &e) {
        ObjHandler::logMessage(std::string("qlListRegisteredEnums Error: ") + e.what(), 2);
        return SCM_EOL;
    }
}

SCM qlListEnum(SCM x) {
    try {
        std::string enumId = GetChop<std::string>::scalar(x);
        std::vector < std::string > returnValue = QuantLibAddin::getEnumMembers(
            enumId);
        return Nat2Scm<std::string>::vector(returnValue);
    } catch (const std::exception &e) {
        ObjHandler::logMessage(std::string("qlListEnum Error: ") + e.what(), 2);
        return SCM_EOL;
    }
}

SCM qlListRegisteredTypes(SCM x) {
    try {
        std::vector < std::string > returnValue = QuantLibAddin::getRegisteredComplexTypes();
        return Nat2Scm<std::string>::vector(returnValue);
    } catch (const std::exception &e) {
        ObjHandler::logMessage(std::string("qlListRegisteredTypes Error: ") + e.what(), 2);
        return SCM_EOL;
    }
}

SCM qlListType(SCM x) {
    try {
        std::string enumId = GetChop<std::string>::scalar(x);
        std::vector < std::string > returnValue = QuantLibAddin::getComplexTypeMembers(
            enumId);
        return Nat2Scm<std::string>::vector(returnValue);
    } catch (const std::exception &e) {
        ObjHandler::logMessage(std::string("qlListType Error: ") + e.what(), 2);
        return SCM_EOL;
    }
}


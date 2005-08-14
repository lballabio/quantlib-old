
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

SCM qlOhVersion(SCM x) {
    try {
        std::string ver = ObjHandler::ohVersion();
        return gh_str02scm(ver.c_str());
    } catch (const std::exception &e) {
        ObjHandler::logMessage("qlOhVersion Error: " + std::string(e.what()), 2);
        return SCM_UNSPECIFIED;
    }
}

SCM qlFieldNames(SCM x) {
    try {
        std::string handle = GetChop<std::string>::scalar(x);
        ObjHandler::obj_ptr objectPointer = ObjHandler::retrieveObject(handle);
        ObjHandler::Properties properties = objectPointer->getProperties();
        SCM rtn = SCM_EOL;
        for (std::size_t i=properties.size() ; --i != std::size_t(-1) ; ) {
            ObjHandler::ObjectProperty property = properties[i];
            ObjHandler::any_ptr a = property();
            SCM label = gh_str02scm(property.name().c_str());
            rtn = gh_cons(label, rtn);
        }
        return rtn;
    } catch (const std::exception &e) {
        ObjHandler::logMessage("qlFieldNames Error: " + std::string(e.what()), 2);
        return SCM_UNSPECIFIED;
    }
}

SCM qlFieldValue(SCM x) {
    try {
        std::string handle = GetChop<std::string>::scalar(x);
        ObjHandler::obj_ptr objectPointer = ObjHandler::retrieveObject(handle);
        ObjHandler::Properties properties = objectPointer->getProperties();
        std::string fieldName = GetChop<std::string>::scalar(x);
        std::string fieldNameUpper = QuantLib::uppercase(fieldName);
        for (std::size_t i=properties.size() ; --i != std::size_t(-1) ; ) {
            ObjHandler::ObjectProperty property = properties[i];
            ObjHandler::any_ptr a = property();
            if (property.name().compare(fieldNameUpper) == 0)
                return anyToPairValue(*property());
        }
        return SCM_UNSPECIFIED;
    } catch (const std::exception &e) {
        ObjHandler::logMessage("qlValue Error: " + std::string(e.what()), 2);
        return SCM_UNSPECIFIED;
    }
}

SCM qlSetLogfile(SCM x) {
    try {
        std::string logFile = GetChop<std::string>::scalar(x);
        int logLevel = GetChop<int>::scalar(x);
        ObjHandler::setLogFile(logFile, logLevel);
        return SCM_UNSPECIFIED;
    } catch (const std::exception &e) {
        ObjHandler::logMessage("qlLogfile Error: " + std::string(e.what()), 2);
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

SCM qlLogMessage(SCM x) {
    try {
        std::string logMessage = GetChop<std::string>::scalar(x);
        int logLevel = GetChop<int>::scalar(x);
        ObjHandler::logMessage(logMessage, logLevel);
        return SCM_UNSPECIFIED;
    } catch (const std::exception &e) {
        ObjHandler::logMessage("qlLogMessage Error: " + std::string(e.what()), 2);
        return SCM_UNSPECIFIED;
    }
}

SCM qlSetLogLevel(SCM x) {
    try {
        long logLevel = GetChop<long>::scalar(x);
        ObjHandler::setLogLevel(logLevel);
        return SCM_UNSPECIFIED;
    } catch (const std::exception &e) {
        ObjHandler::logMessage("qlLogLevel Error: " + std::string(e.what()), 2);
        return SCM_UNSPECIFIED;
    }
}

SCM qlLogObject(SCM x) {
    try {
        std::string handle = GetChop<std::string>::scalar(x);
        ObjHandler::logObject(handle);
        return SCM_UNSPECIFIED;
    } catch (const std::exception &e) {
        ObjHandler::logMessage("qlLogObject Error: " + std::string(e.what()), 2);
        return SCM_UNSPECIFIED;
    }
}

SCM qlLogAllObjects(SCM x) {
    try {
        ObjHandler::logAllObjects();
        return SCM_UNSPECIFIED;
    } catch (const std::exception &e) {
        ObjHandler::logMessage("qlLogAllObjects Error: " + std::string(e.what()), 2);
        return SCM_UNSPECIFIED;
    }
}

SCM qlDeleteObject(SCM x) {
    try {
        std::string handle = GetChop<std::string>::scalar(x);
        ObjHandler::ObjectHandler::instance().deleteObject(handle);
        return SCM_UNSPECIFIED;
    } catch (const std::exception &e) {
        ObjHandler::logMessage("qlDeleteObject Error: " + std::string(e.what()), 2);
        return SCM_UNSPECIFIED;
    }
}

SCM qlDeleteAllObjects(SCM x) {
    try {
        ObjHandler::ObjectHandler::instance().deleteAllObjects();
        return SCM_UNSPECIFIED;
    } catch (const std::exception &e) {
        ObjHandler::logMessage("qlLogAllObjects Error: " + std::string(e.what()), 2);
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


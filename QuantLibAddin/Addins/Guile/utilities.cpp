
/*
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

#include <qla/qladdin.hpp>
#include <Addins/Guile/guileutils.hpp>
extern "C" {
#include <Addins/Guile/extra.h>
#include <Addins/Guile/utilities.h>
}

using namespace ObjHandler;
using namespace QuantLibAddin;

SCM qlVersion(SCM x) {
    try {
        std::string ver = QL_VERSION();
        return gh_str02scm(ver.c_str());
    } catch (const std::exception &e) {
        OH_LOG_MESSAGE("qlVer Error: " + std::string(e.what()), 2);
        return SCM_UNSPECIFIED;
    }
}

SCM qlOhVersion(SCM x) {
    try {
        std::string ver = QL_OH_VERSION();
        return gh_str02scm(ver.c_str());
    } catch (const std::exception &e) {
        OH_LOG_MESSAGE("qlOhVer Error: " + std::string(e.what()), 2);
        return SCM_UNSPECIFIED;
    }
}

SCM qlFieldNames(SCM x) {
    try {
        std::string handle = Convert<std::string>::scalar(gh_car(x));
        Properties properties = OH_QUERY_OBJECT(handle);
        SCM rtn = SCM_EOL;
        for (std::size_t i=properties.size() ; --i != std::size_t(-1) ; ) {
            ObjectProperty property = properties[i];
            any_ptr a = property();
            SCM label = gh_str02scm(property.name().c_str());
            rtn = gh_cons(label, rtn);
        }
        return rtn;
    } catch (const std::exception &e) {
        OH_LOG_MESSAGE("qlFieldNames Error: " + std::string(e.what()), 2);
        return SCM_UNSPECIFIED;
    }
}

SCM qlValue(SCM x) {
    try {
        std::string handle = Convert<std::string>::scalar(gh_car(x));
        Properties properties = OH_QUERY_OBJECT(handle);
        std::string fieldName = Convert<std::string>::scalar(gh_cadr(x));
        std::string fieldNameUpper = QuantLib::StringFormatter::toUppercase(fieldName);
        for (std::size_t i=properties.size() ; --i != std::size_t(-1) ; ) {
            ObjectProperty property = properties[i];
            any_ptr a = property();
            if (property.name().compare(fieldNameUpper) == 0)
                return anyToPairValue(property);
        }
        return SCM_UNSPECIFIED;
    } catch (const std::exception &e) {
        OH_LOG_MESSAGE("qlValue Error: " + std::string(e.what()), 2);
        return SCM_UNSPECIFIED;
    }
}

SCM qlLogfile(SCM x) {
    try {
        std::string logFile = Convert<std::string>::scalar(gh_car(x));
        int logLevel = Convert<int>::scalar(gh_cadr(x));
        OH_LOGFILE(logFile, logLevel);
        return SCM_UNSPECIFIED;
    } catch (const std::exception &e) {
        OH_LOG_MESSAGE("qlLogfile Error: " + std::string(e.what()), 2);
        return SCM_UNSPECIFIED;
    }
}

SCM qlConsole(SCM x) {
    try {
        long consoleEnabled = Convert<long>::scalar(gh_car(x));
        int logLevel = Convert<int>::scalar(gh_cadr(x));
        OH_CONSOLE(consoleEnabled, logLevel);
        return SCM_UNSPECIFIED;
    } catch (const std::exception &e) {
        OH_LOG_MESSAGE("qlConsole Error: " + std::string(e.what()), 2);
        return SCM_UNSPECIFIED;
    }
}

SCM qlLogMessage(SCM x) {
    try {
        std::string logMessage = Convert<std::string>::scalar(gh_car(x));
        int logLevel = Convert<int>::scalar(gh_cadr(x));
        OH_LOG_MESSAGE(logMessage, logLevel);
        return SCM_UNSPECIFIED;
    } catch (const std::exception &e) {
        OH_LOG_MESSAGE("qlLogMessage Error: " + std::string(e.what()), 2);
        return SCM_UNSPECIFIED;
    }
}

SCM qlLogLevel(SCM x) {
    try {
        long logLevel = Convert<long>::scalar(gh_car(x));
        OH_LOG_LEVEL(logLevel);
        return SCM_UNSPECIFIED;
    } catch (const std::exception &e) {
        OH_LOG_MESSAGE("qlLogLevel Error: " + std::string(e.what()), 2);
        return SCM_UNSPECIFIED;
    }
}

SCM qlLogObject(SCM x) {
    try {
        std::string handle = Convert<std::string>::scalar(gh_car(x));
        OH_LOG_OBJECT(handle);
        return SCM_UNSPECIFIED;
    } catch (const std::exception &e) {
        OH_LOG_MESSAGE("qlLogObject Error: " + std::string(e.what()), 2);
        return SCM_UNSPECIFIED;
    }
}

SCM qlLogAllObjects(SCM x) {
    try {
        OH_LOG_ALL_OBJECTS();
        return SCM_UNSPECIFIED;
    } catch (const std::exception &e) {
        OH_LOG_MESSAGE("qlLogAllObjects Error: " + std::string(e.what()), 2);
        return SCM_UNSPECIFIED;
    }
}

SCM qlDeleteObject(SCM x) {
    try {
        std::string handle = Convert<std::string>::scalar(gh_car(x));
        OH_DELETE_OBJECT(handle);
        return SCM_UNSPECIFIED;
    } catch (const std::exception &e) {
        OH_LOG_MESSAGE("qlDeleteObject Error: " + std::string(e.what()), 2);
        return SCM_UNSPECIFIED;
    }
}

SCM qlDeleteAllObjects(SCM x) {
    try {
        OH_DELETE_ALL_OBJECTS();
        return SCM_UNSPECIFIED;
    } catch (const std::exception &e) {
        OH_LOG_MESSAGE("qlLogAllObjects Error: " + std::string(e.what()), 2);
        return SCM_UNSPECIFIED;
    }
}


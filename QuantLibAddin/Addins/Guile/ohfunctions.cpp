
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
#include <Addins/Guile/objecthandler.h>
}


SCM ohVersion(SCM x) {
    try {
        std::string ver = OBJHANDLER_VERSION;
        return gh_str02scm(ver.c_str());
    } catch (const std::exception &e) {
        ObjHandler::logMessage("ohVersion Error: " + std::string(e.what()), 2);
        return SCM_UNSPECIFIED;
    }
}

SCM ohFieldNames(SCM x) {
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
        ObjHandler::logMessage("ohFieldNames Error: " + std::string(e.what()), 2);
        return SCM_UNSPECIFIED;
    }
}

SCM ohFieldValue(SCM x) {
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
        ObjHandler::logMessage("ohFieldValue Error: " + std::string(e.what()), 2);
        return SCM_UNSPECIFIED;
    }
}

SCM ohSetLogFile(SCM x) {
    try {
        std::string logFile = GetChop<std::string>::scalar(x);
        int logLevel = GetChop<int>::scalar(x);
        ObjHandler::setLogFile(logFile, logLevel);
        return SCM_UNSPECIFIED;
    } catch (const std::exception &e) {
        ObjHandler::logMessage("ohSetLogFile Error: " + std::string(e.what()), 2);
        return SCM_UNSPECIFIED;
    }
}

SCM ohLogMessage(SCM x) {
    try {
        std::string logMessage = GetChop<std::string>::scalar(x);
        int logLevel = GetChop<int>::scalar(x);
        ObjHandler::logMessage(logMessage, logLevel);
        return SCM_UNSPECIFIED;
    } catch (const std::exception &e) {
        return SCM_UNSPECIFIED;
    }
}

SCM ohSetLogLevel(SCM x) {
    try {
        long logLevel = GetChop<long>::scalar(x);
        ObjHandler::setLogLevel(logLevel);
        return SCM_UNSPECIFIED;
    } catch (const std::exception &e) {
        ObjHandler::logMessage("ohSetLogLevel Error: " + std::string(e.what()), 2);
        return SCM_UNSPECIFIED;
    }
}

SCM ohLogObject(SCM x) {
    try {
        std::string handle = GetChop<std::string>::scalar(x);
        ObjHandler::logObject(handle);
        return SCM_UNSPECIFIED;
    } catch (const std::exception &e) {
        ObjHandler::logMessage("ohLogObject Error: " + std::string(e.what()), 2);
        return SCM_UNSPECIFIED;
    }
}

SCM ohLogAllObjects(SCM x) {
    try {
        ObjHandler::logAllObjects();
        return SCM_UNSPECIFIED;
    } catch (const std::exception &e) {
        ObjHandler::logMessage("ohLogAllObjects Error: " + std::string(e.what()), 2);
        return SCM_UNSPECIFIED;
    }
}

SCM ohDeleteObject(SCM x) {
    try {
        std::string handle = GetChop<std::string>::scalar(x);
        ObjHandler::ObjectHandler::instance().deleteObject(handle);
        return SCM_UNSPECIFIED;
    } catch (const std::exception &e) {
        ObjHandler::logMessage("ohDeleteObject Error: " + std::string(e.what()), 2);
        return SCM_UNSPECIFIED;
    }
}

SCM ohDeleteAllObjects(SCM x) {
    try {
        ObjHandler::ObjectHandler::instance().deleteAllObjects();
        return SCM_UNSPECIFIED;
    } catch (const std::exception &e) {
        ObjHandler::logMessage("ohDeleteAllObjects Error: " + std::string(e.what()), 2);
        return SCM_UNSPECIFIED;
    }
}

SCM ohObjectCount(SCM x) {
    try {
        return SCM_UNSPECIFIED;
    } catch (const std::exception &e) {
        ObjHandler::logMessage("ohObjectCount Error: " + std::string(e.what()), 2);
        return SCM_UNSPECIFIED;
    }
}

SCM ohHandleList(SCM x) {
    try {
        return SCM_UNSPECIFIED;
    } catch (const std::exception &e) {
        ObjHandler::logMessage("ohHandleList Error: " + std::string(e.what()), 2);
        return SCM_UNSPECIFIED;
    }
}



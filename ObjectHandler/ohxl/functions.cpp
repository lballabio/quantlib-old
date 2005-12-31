
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

#include <oh/utilities.hpp>
#include <oh/objecthandler.hpp>
#include <ohxl/conversions.hpp>
#include <sstream>

using namespace ObjHandler;

extern "C" char* ohVersion() {
    try {
        static char ret[XL_MAX_STR_LEN];
		stringToChar(ret, OBJHANDLER_VERSION);
        return ret;
    } catch (const std::exception &e) {
        logMessage(std::string("ERROR: ohVersion: ") + e.what(), 2);
        return 0;
    }
}

extern "C" long* ohObjectCount() {
    try {
        static long ret;
        ret = ObjectHandler::instance().objectCount();
        return &ret;
    } catch (const std::exception &e) {
        logMessage(std::string("ERROR: ohObjectCount: ") + e.what(), 2);
        return 0;
    }
}

extern "C" XLOPER* ohHandleList() {
    try {
        static XLOPER ret;
        std::vector < std::string > handleList = ObjectHandler::instance().handleList();
        vectorToXloper(ret, handleList);
        return &ret;
    } catch (const std::exception &e) {
        logMessage(std::string("ERROR: ohHandleList: ") + e.what(), 2);
        return 0;
    }
}

extern "C" XLOPER *ohPropertyNames(char *handleObject) {
    try {
        static XLOPER ret;
        std::vector < std::string > propertyNames 
            = ObjectHandler::instance().propertyNames(handleObject);
        vectorToXloper(ret, propertyNames);
        return &ret;
    } catch (const std::exception &e) {
        logMessage(std::string("ERROR: ohPropertyNames: ") + e.what(), 2);
        return 0;
    }
}

extern "C" XLOPER *ohPropertyValue(char *handleObject,
        char *propertyName,
        OPER *trigger) {
    try {
        static XLOPER ret;
        boost::any propertyValue 
            = ObjectHandler::instance().propertyValue(handleObject, propertyName);
        scalarToXloper(ret, propertyValue);
        return &ret;
    } catch (const std::exception &e) {
        logMessage(std::string("ERROR: ohPropertyValue: ") + e.what(), 2);
        return 0;
    }
}

extern "C" short int* ohDeleteObject(char *handleObject) {
    try {
        static short int ret = TRUE;
        ObjectHandler::instance().deleteObject(handleObject);
        return &ret;
    } catch (const std::exception &e) {
        logMessage(std::string("ERROR: ohDeleteObject: ") + e.what(), 2);
        return 0;
    }
}

extern "C" short int* ohDeleteAllObjects() {
    try {
        static short int ret = TRUE;
        ObjectHandler::instance().deleteAllObjects();
        return &ret;
    } catch (const std::exception &e) {
        logMessage(std::string("ERROR: ohDeleteAllObjects: ") + e.what(), 2);
        return 0;
    }
}

extern "C" short int* ohDependsOn(
        OPER *dummy0,
        OPER *dummy1,
        OPER *dummy2,
        OPER *dummy3,
        OPER *dummy4,
        OPER *dummy5,
        OPER *dummy6,
        OPER *dummy7,
        OPER *dummy8,
        OPER *dummy9) {
    try {
        static short int ret = FALSE;
        return &ret;
    } catch (const std::exception &e) {
        logMessage(std::string("ERROR: ohDependsOn: ") + e.what(), 2);
        return 0;
    }
}

extern "C" short int* ohGetGcEnabled() {
    try {
        static short int ret;
        ret = ObjectHandler::instance().getGcEnabled();
        return &ret;
    } catch (const std::exception &e) {
        logMessage(std::string("ERROR: ohGetGcEnabled: ") + e.what(), 2);
        return 0;
    }
}

extern "C" short int* ohSetGcEnabled(short int *newValue) {
    try {
        static short int ret = TRUE;
        ObjectHandler::instance().setGcEnabled(*newValue != 0);
        return &ret;
    } catch (const std::exception &e) {
        logMessage(std::string("ERROR: ohSetGcEnabled: ") + e.what(), 2);
        return 0;
    }
}

extern "C" short int* ohCallGC() {
    try {
        static short int ret = TRUE;
        ObjectHandler::instance().collectGarbage();
        return &ret;
    } catch (const std::exception &e) {
        logMessage(std::string("ERROR: ohCallGC: ") + e.what(), 2);
        return 0;
    }
}

extern "C" char* ohSetLogFile(char *logFileName, OPER *logLevel) {
    try {
        static char ret[XL_MAX_STR_LEN];
        long logLevelScalar;
        operToScalar(logLevelScalar, *logLevel, 4);
        setLogFile(logFileName, logLevelScalar);
        stringToChar(ret, logFileName);
        return ret;
    } catch (...) {
        return 0;
    }
}

extern "C" long* ohSetLogLevel(long *logLevel) {
    try {
        static long ret;
        setLogLevel(*logLevel);
        ret = *logLevel;
        return &ret;
    } catch (const std::exception &e) {
        logMessage(std::string("ERROR: ohSetLogLevel: ") + e.what(), 2);
        return 0;
    }
}

extern "C" char* ohLogMessage(char *message, OPER *logLevel) {
    try {
        static char ret[XL_MAX_STR_LEN];
        long logLevelScalar;
        operToScalar(logLevelScalar, *logLevel, 4);
        logMessage(message, logLevelScalar);
        stringToChar(ret, message);
        return ret;
    } catch (...) {
        return 0;
    }
}

extern "C" short int* ohLogObject(char *handleObject) {
    try {
        static short int ret = TRUE;
        logObject(handleObject);
        return &ret;
    } catch (const std::exception &e) {
        logMessage(std::string("ERROR: ohLogObject: ") + e.what(), 2);
        return 0;
    }
}

extern "C" short int* ohLogAllObjects() {
    try {
        static short int ret = TRUE;
        logAllObjects();
        return &ret;
    } catch (const std::exception &e) {
        logMessage(std::string("ERROR: ohLogAllObjects: ") + e.what(), 2);
        return 0;
    }
}


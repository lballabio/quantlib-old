
/*
 Copyright (C) 2005 Eric Ehlers

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it under the
 terms of the QuantLib license.  You should have received a copy of the
 license along with this program; if not, please email quantlib-dev@lists.sf.net
 The license is also available online at http://quantlib.org/html/license.html

 This program is distributed in the hope that it will be useful, but WITHOUT
 boost::any WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
*/

#if defined(HAVE_CONFIG_H)     // Dynamically created by configure
    #include <qla/config.hpp>
#endif
//#include <ql/qldefines.hpp>
#include <qla/ohfunctions.hpp>

namespace QuantLibAddin {

    const bool ohDeleteAllObjects() {
        ObjHandler::ObjectHandler::instance().deleteAllObjects();
        return true;
    }

    const bool ohDeleteObject(const std::string &handleObject) {
        ObjHandler::ObjectHandler::instance().deleteObject(handleObject);
        return true;
    }

    const std::vector < std::string > ohPropertyNames(const std::string &handleObject) {
        return ObjHandler::ObjectHandler::instance().propertyNames(handleObject);
    }

    const boost::any ohPropertyValue(
            const std::string &handleObject,
            const std::string &propertyName) {
        return ObjHandler::ObjectHandler::instance().propertyValue(handleObject, propertyName);
    }

    const long ohObjectCount() {
        return ObjHandler::ObjectHandler::instance().objectCount();
    }

    const std::vector < std::string > ohHandleList() {
        return ObjHandler::ObjectHandler::instance().handleList();
    }

    const bool ohLogAllObjects() {
        ObjHandler::logAllObjects();
        return true;
    }

    const std::string &ohLogMessage(
            const std::string &logMessage,
            const long &logLevel) {
        ObjHandler::logMessage(logMessage, logLevel);
        return logMessage;
    }

    const bool ohLogObject(
            const std::string &handleObject) {
        ObjHandler::logObject(handleObject);
        return true;
    }

    const std::string &ohSetLogFile(
            const std::string &logFileName,
            const long &logLevel) {
        ObjHandler::setLogFile(logFileName, logLevel);
        return logFileName;
    }

    const long &ohSetLogLevel(
            const long &logLevel) {
        ObjHandler::setLogLevel(logLevel);
        return logLevel;
    }

    const bool ohSetConsole(
            const long &console,
            const long &logLevel) {
        ObjHandler::setConsole(console, logLevel);
        return true;
    }

    const std::string ohVersion() {
        return ObjHandler::version();
    }

    const bool ohDependsOn() {
        return true;
    }

}


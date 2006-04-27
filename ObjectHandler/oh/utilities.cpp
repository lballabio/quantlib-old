
/*
 Copyright (C) 2004, 2005, 2006 Eric Ehlers
 Copyright (C) 2006 Plamen Neykov

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
    #include <oh/config.hpp>
#endif
#include <oh/utilities.hpp>
#include <oh/logger.hpp>
#include <oh/exception.hpp>
#include <sstream>

namespace ObjHandler {

    DLL_API std::string storeObject(
            const std::string &instanceName,
            const obj_ptr &object) {
        std::string ret = 
            ObjectHandler::instance().storeObject(instanceName, object);
        return ret;
    }

    DLL_API ObjHandler::obj_ptr retrieveObject(
            const std::string &handle) {
        return ObjectHandler::instance().retrieveObject(handle);
    }

    std::string version() {
        return OBJHANDLER_VERSION;
    }

    void setLogFile(
            const std::string &logFileName,
            const int &logLevel) {
        Logger::instance().setLogFile(logFileName, logLevel);
    }

    DLL_API void logMessage(
            const std::string &message,
            const int &level) {
        Logger::instance().logMessage(message, level);
    }

    void setLogLevel(const int &logLevel) {
        Logger::instance().setLogLevel(logLevel);
    }

    void setConsole(
            const int &console,
            const int &logLevel) {
        Logger::instance().setConsole(console, logLevel);
    }

    void logObject(const std::string &handle) {
        std::ostringstream msg;
        obj_ptr object = 
            ObjectHandler::instance().retrieveObject(handle);
        if (object) {
            msg << "log dump of object with handle = " << handle << std::endl;
            msg << *object.get();
        } else {
            msg << "no object in repository with handle = " << handle << std::endl;
        }
        Logger::instance().logMessage(msg.str());
    }

    void logAllObjects() {
        std::ostringstream msg;
        ObjectHandler::instance().dump(msg);
        Logger::instance().logMessage(msg.str());
    }

}


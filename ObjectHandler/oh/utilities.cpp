
/*
 Copyright (C) 2004, 2005 Eric Ehlers

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

    void OH_LOGFILE(
            const std::string &logFileName,
            const int &logLevel) {
        Logger::instance().setLogFile(logFileName, logLevel);
    }

    void OH_LOG_MESSAGE(
            const std::string &message,
            const int &level) {
        Logger::instance().logMessage(message, level);
    }

    void OH_LOG_LEVEL(const int &logLevel) {
        Logger::instance().setLogLevel(logLevel);
    }

    void OH_CONSOLE(
            const int &console,
            const int &logLevel) {
        Logger::instance().setConsole(console, logLevel);
    }

    void OH_LOG_OBJECT(const std::string &handle) {
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

    void OH_LOG_ALL_OBJECTS() {
        std::ostringstream msg;
        ObjectHandler::instance().dump(msg);
        Logger::instance().logMessage(msg.str());
    }

    const Properties& OH_QUERY_OBJECT(
            const std::string &handle) {
        boost::shared_ptr<Object> object =
                ObjectHandler::instance().retrieveObject(handle);
        if (!object)
                throw Exception("error retrieving object " + handle);
        return object->getProperties();
    }

}


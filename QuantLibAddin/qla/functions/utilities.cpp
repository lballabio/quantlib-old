
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
    #include <qla/config.hpp>
#endif
#include <qla/functions/utilities.hpp>
#include <ql/qldefines.hpp>
#include <oh/logger.hpp>
#include <sstream>

using namespace ObjHandler;

namespace QuantLibAddin {

    std::string QL_VER() {
        std::ostringstream s;
        s << "QuantLib version " << QL_VERSION;
        return s.str();
    }

    std::string QL_OH_VER() {
        std::ostringstream s;
        s << "ObjectHandler version " << OBJHANDLER_VERSION;
        return s.str();
    }

    const Properties& QL_QUERY(
            const std::string &handle) {
        boost::shared_ptr<Object> object =
                ObjectHandler::instance().retrieveObject(handle);
        if (!object)
                throw Exception("error retrieving object " + handle);
        return object->getProperties();
    }

    std::string QL_LOGFILE(const std::string &logFileName,
            const int &logLevel) {
        Logger::instance().setLogFile(logFileName, logLevel);
        return logFileName;
    }

    void QL_CONSOLE(const int &console,
            const int &logLevel){
        Logger::instance().setConsole(console, logLevel);
    }

    void QL_LOGLEVEL(const int &logLevel) {
        Logger::instance().setLogLevel(logLevel);
    }

    std::string QL_LOGMESSAGE(const std::string &message,
            const int &level) {
        Logger::instance().logMessage(message, level);
        return message;
    }

}


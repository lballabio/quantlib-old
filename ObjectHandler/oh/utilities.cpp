
/*
 Copyright (C) 2007 Ferdinando Ametrano
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
#include <oh/objecthandler.hpp>
#include <oh/exception.hpp>
#include <boost/algorithm/string/classification.hpp>
#include <boost/algorithm/string/split.hpp>
#include <sstream>

using std::string;
using boost::algorithm::token_compress_off;
using boost::algorithm::token_compress_on;
using boost::algorithm::is_any_of;


namespace ObjHandler {

    string version() {
        return OBJHANDLER_VERSION;
    }

    string setLogFile(const string &logFileName,
                      const int &logLevel) {
        Logger::instance().setLogFile(logFileName, logLevel);
        return logFileName;
    }

    DLL_API void logMessage(const string &message,
                            const int &level) {
        Logger::instance().logMessage(message, level);
    }

    void setLogLevel(const int &logLevel) {
        Logger::instance().setLogLevel(logLevel);
    }

    void setConsole(const int &console,
                    const int &logLevel) {
        Logger::instance().setConsole(console, logLevel);
    }

    void logObject(const string &objectID) {
        std::ostringstream msg;
        boost::shared_ptr<Object> object = 
            ObjectHandler::instance().retrieveObjectImpl(objectID);
        if (object) {
            msg << "log dump of object with ID = " << objectID << std::endl;
            msg << *object.get();
        } else {
            msg << "no object in repository with ID = " << objectID << std::endl;
        }
        Logger::instance().logMessage(msg.str());
    }

    void logAllObjects() {
        std::ostringstream msg;
        ObjectHandler::instance().dump(msg);
        Logger::instance().logMessage(msg.str());
    }

    // parse a whitespace-delimited list of symbols 
    // into a vector of strings
    std::vector<string> split(const string &line,
                              const string &delim,
                              bool token_compress) {
        std::vector<string> ret;
        return boost::algorithm::split(ret, line, is_any_of(delim),
                    token_compress ? token_compress_on : token_compress_off);
    }

}


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
#include <oh/objhandlerdefines.hpp>
#include <oh/utilities.hpp>
#include <oh/exception.hpp>
#include <oh/logger.hpp>

namespace ObjHandler {

    std::ostream& operator<<(std::ostream& out, const any_ptr& a) {
        if (a->empty())
            return out << "null";
        else if (a->type() == typeid(int))
            return out << boost::any_cast<int>(*a);
        else if (a->type() == typeid(double))
            return out << boost::any_cast<double>(*a);
        else if (a->type() == typeid(std::string))
            return out << boost::any_cast<std::string>(*a);
        else
            throw Exception("any_ptr << operator: unrecognized type");
//            return out << "unrecognized type";
    }

    void setLogFile(const std::string &logFileName, 
            const int &logLevel) {
        Logger::instance().setLogFile(logFileName, logLevel);
    }

    void setConsole(const int &console, 
            const int &logLevel) {
        Logger::instance().setConsole(console, logLevel);
    }

    void setLogLevel(const int &logLevel) {
        Logger::instance().setLogLevel(logLevel);
    }

    void logMessage(const std::string &message, 
            const int &level) {
        Logger::instance().logMessage(message, level);
    }

}


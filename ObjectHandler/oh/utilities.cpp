
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

#include <oh/objhandlerdefines.hpp>
#include <oh/utilities.hpp>
#include <oh/exception.hpp>
#include <sstream>
#include <fstream>

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

    // FIXME - below is a temporary approach to logging
    // which will be revised in version 0.0.2.

    std::string logFileName;    // "" = logging disabled

    int setLogFile(const std::string &newLogFileName) {
        std::ofstream logFile;
        if (!newLogFileName.length()) {
            logFileName = "";
            return 0;
        }
        if (!logFileName.compare(std::string(newLogFileName)))
            return 1;    // continue logging to same file
        logFile.open(newLogFileName.c_str(), std::ios::app);
        if (logFile.is_open()) {
            logFile << "logging enabled" << std::endl;
            logFile.close();
            logFileName = newLogFileName;
            return 1;
        } else {
            logFileName = "";
            throw Exception("setLogFile: error opening logfile");
        }
    }

    void logMessage(const std::string &msg) {
        std::ofstream log1;
        if (logFileName.length()) {
            log1.open(logFileName.c_str(), std::ios::app);
            if (log1.is_open()) {
                log1 << msg << std::endl;
                log1.close();
            } else // error - disable logging
                logFileName = "";
        }
    }
}


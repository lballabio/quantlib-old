
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

/*! \file utilities.hpp
    \brief diagnostic and information functions for QuantLibAddin
*/

#ifndef qla_utilities_hpp
#define qla_utilities_hpp

#include <oh/objhandler.hpp>

namespace QuantLibAddin {

/*! \group utilities
    util func xxx
*/

    /*! return the version of QuantLib
    */
    std::string QL_VER();

    /*! return the version of ObjectHandler
    */
    std::string QL_OH_VER();

    /*! enable/disable logging
    */
    std::string QL_LOGFILE(
        /*! name of the logfile
            (pass null string to disable logging)
        */
        const std::string &logFileName);

    /*! write a message to the logfile
    */
    void QL_LOGMESSAGE(
        /*! string to be written to logfile
            (will be automatically suffixed with a carriage return)
        */
        const std::string &msg);

    /*! return the vector of Properties
        describing an Object in the ObjectHandler
    */
    const ObjHandler::Properties& QL_QUERY(
        /*! handle of object to be interrogated
        */
        const std::string &handle);

}

#endif


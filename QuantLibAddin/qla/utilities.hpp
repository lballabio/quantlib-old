
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

/*! \file
    \brief diagnostic and information functions for QuantLibAddin
*/

#ifndef qla_utilities_hpp
#define qla_utilities_hpp

#include <oh/objhandler.hpp>
#include <oh/utilities.hpp>

/*! 
    #define for ObjectHandler factory function
    to construct an object of class \a X.
*/
#define QL_OBJECT_MAKE(X) ObjHandler::Factory<QuantLibAddin::X>::makeObject
/*! 
    #define for ObjectHandler function retrieveObject().
    Retrieves Object with handle \a X.
*/
#define QL_OBJECT_GET(X) ObjHandler::ObjectHandler::instance().retrieveObject(X)
/*! 
    #define for ObjectHandler function deleteObject().
    Deletes Object with handle \a X.
*/
#define QL_OBJECT_DELETE(X) ObjHandler::ObjectHandler::instance().deleteObject(X)
/*! 
    #define for ObjectHandler function deleteAllObjects().
    Deletes all Objects in the ObjectHandler repository.
*/
#define QL_OBJECT_DELETE_ALL() ObjHandler::ObjectHandler::instance().deleteAllObjects()
/*! 
    #define for ObjectHandler function logObject().
    Logs description of Object with handle \a X.
*/
#define QL_LOG_OBJECT(X) ObjHandler::logObject(X)
/*! 
    #define for ObjectHandler function logAllObjects().
    Logs descriptions of all Objects in ObjectHandler repository.
*/
#define QL_LOG_ALL_OBJECTS() ObjHandler::logAllObjects()

namespace QuantLibAddin {

/*! \group utilities
    diagnostic and information functions for QuantLibAddin
*/

    /*! return the version of QuantLib
    */
    std::string QL_VER();

    /*! return the version of ObjectHandler
    */
    std::string QL_OH_VER();

    /*! return the vector of Properties
        describing an Object in the ObjectHandler
    */
    const ObjHandler::Properties& QL_QUERY(
        /*! handle of object to be interrogated
        */
        const std::string &handle);

    //! \name logging framework
    //@{
    //! Specify file to which messages should be logged.
    /*! Redirect logging to named file.  Logging to any
        previously specified logfile is discontinued.
        If the specified file doesn't already exist it is created.
        If the specified file does already exist, new messages are
        appended to it.  If the specified file cannot be opened, 
        an error is thrown.
        This function accepts an additional optional argument
        logLevel which is passed as an argument to setLogLevel 
        (see below). logLevel defaults to 4 (info).
    */
    std::string QL_LOGFILE(const std::string &logFileName,
            const int &logLevel = 4);
    //! Direct logging to the console (stdout)
    /*! Logging to the console is disabled by default.
        Call this function with a parameter of 1 to enable
        logging to the console or 0 (the default) to disable it.
        This function accepts an additional optional argument
        logLevel which is passed as an argument to setLogLevel 
        (see below). logLevel defaults to 4 (info).
    */
    void QL_CONSOLE(const int &console = 0,
            const int &logLevel = 4);
    //! Specify threshold for logging messages.
    /*! Accepts one parameter int with the following values:
        0 - logging disabled
        1 - fatal
        2 - error
        3 - warn
        4 - info
        5 - debug.
        The logging level is cumulative e.g. a level of 3 means that
        messages of levels warn, error and fatal are logged.
        If the specified level is not in the range 0-5
        an error is thrown.
    */
    void QL_LOGLEVEL(const int &logLevel);
    //! Log a message.
    /*! The optional level parameter specifies the logging level
        for the message, if omitted this defaults to 4 (info).
    */
    std::string QL_LOGMESSAGE(const std::string &message,
            const int &level = 4);
//    void QL_LOG_OBJECT(const std::string &handle);

    // FIXME this needs to be moved to options file
    const ObjHandler::Properties& QL_OPTION_SETENGINE(
            const std::string &handle,
            const std::string &engineName,
            const long &timeSteps);

}

#endif



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
    \brief ObjectHandler utilities
*/

#ifndef oh_utilities_hpp
#define oh_utilities_hpp

#include <oh/objecthandler.hpp>

/*! 
    #define for ObjectHandler factory function
    to construct an object of class \a X.
*/
#define OH_OBJECT_MAKE(X)       ObjHandler::Factory<X>::makeObject
/*! 
    #define for ObjectHandler function retrieveObject().
    Retrieves Object with handle \a X.
*/
#define OH_OBJECT_GET(X)        ObjHandler::ObjectHandler::instance().retrieveObject(X)
/*! 
    #define for ObjectHandler function deleteObject().
    Deletes Object with handle \a X.
*/
#define OH_OBJECT_DELETE(X)     ObjHandler::ObjectHandler::instance().deleteObject(X)
/*! 
    #define for ObjectHandler function deleteAllObjects().
    Deletes all Objects in the ObjectHandler repository.
*/
#define OH_OBJECT_DELETE_ALL()  ObjHandler::ObjectHandler::instance().deleteAllObjects()

namespace ObjHandler {

    //! Write Object with given handle to log file.
    /*! Writes a warning message to log file
        if no object is found with given handle.
    */
    void OH_LOG_OBJECT(const std::string &handle);
    //! Write all Objects to log file.
    /*! Takes no action if ObjectHandler
        repository is empty.
    */
    void OH_LOG_ALL_OBJECTS();
    /*! 
        Wrapper for function Logger::instance().setLogFile().
        Specify name of log file.
    */
    void OH_LOGFILE(const std::string &logFileName,
                const int &logLevel = 4);
    /*! 
        Wrapper for function Logger::instance().setLogLevel().
        Set logging threshold.
    */
    void OH_LOGLEVEL(const int &logLevel);
    /*! 
        Wrapper for function Logger::instance().setConsole().
        Fork log messages to stdout.
    */
    void OH_CONSOLE(const int &console = 0,
                const int &logLevel = 4);
    /*! 
        Wrapper for function Logger::instance().logMessage()
        Write a message to the log file.
    */
    void OH_LOGMESSAGE(
            const std::string &message,
            const int &level = 4);

}

#endif


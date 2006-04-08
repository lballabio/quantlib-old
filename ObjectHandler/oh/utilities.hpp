
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

/*! \file
    \brief ObjectHandler utilities
*/

#ifndef oh_utilities_hpp
#define oh_utilities_hpp

#include <oh/objecthandler.hpp>

//! ObjectHandler function retrieveObject
/*! Retrieve Object of class \a CLASS with handle \a HANDLE.
*/
#define OH_GET_OBJECT( NAME, HANDLE, CLIENT_CLASS ) \
    boost::shared_ptr < CLIENT_CLASS > NAME = \
        boost::dynamic_pointer_cast< CLIENT_CLASS > \
        (ObjHandler::retrieveObject( HANDLE )); \
    if (!NAME) { \
        std::ostringstream err; \
        err << "Unable to convert handle " << HANDLE \
            << " to object of class " #CLIENT_CLASS; \
        throw ObjHandler::Exception(err.str()); \
    }

#define OH_GET_OBJECT_EO( CLASS, HANDLE ) \
    boost::dynamic_pointer_cast< CLASS > \
    (ObjHandler::retrieveObjectEO( HANDLE ))
//! Object function getReference
/*! Retrieve a reference to underlying Object of class \a CLASS
    with handle \a HANDLE.
*/
#define OH_GET_REFERENCE( NAME, HANDLE, CLIENT_CLASS, UNDERLYING_CLASS ) \
    OH_GET_OBJECT(NAME ## temp, HANDLE, CLIENT_CLASS ) \
    const boost::shared_ptr< UNDERLYING_CLASS > NAME = \
        boost::static_pointer_cast< UNDERLYING_CLASS > \
        ( NAME ## temp->getReference() );

namespace ObjHandler {
    //! Store given Object in repository under given handle.
    /*! Any existing Objet with that handle is deleted.
    */
    DLL_API const std::string storeObject(
            const std::string &handleStub,
            const ObjHandler::obj_ptr &object);
    //! Retrieve named Object from repository.
    /*! Throws an exception if no Object exists with given handle.
    */
    DLL_API ObjHandler::obj_ptr retrieveObject(
            const std::string &handle);
    //! Retrieve named Object from repository.
    /*! If no Object exists with given handle an empty pointer is returned.
    */
    DLL_API ObjHandler::obj_ptr retrieveObjectEO(
            const std::string &handle);
    //! Return Property vector for given Object.
    /*! Throws an exception if no Object exists with given handle.
    */
    //const Properties& queryObject(const std::string &handle);
    //! Retrieve ObjectHandler version string
    const std::string version();
    /** \name Logging framework
     *  These functions wrap calls to the Logger class
        simplifying the syntax for the client application
        and alleviating the need for the client application
        to #include log4cxx headers.
    */
    //@{
    //! Wrapper for function Logger::instance().setLogFile().
    /*! Specify name of log file.
    */
    void setLogFile(
        const std::string &logFileName,
        const int &logLevel = 4);
    //! Wrapper for function Logger::instance().logMessage()
    /*! Write a message to the log file.
    */
    DLL_API void logMessage(
        const std::string &message,
        const int &level = 4);
    //! Wrapper for function Logger::instance().setLogLevel().
    /*! Set logging threshold.
    */
    void setLogLevel(const int &logLevel);
    //! Wrapper for function Logger::instance().setConsole().
    /*! Fork log messages to stdout.
    */
    void setConsole(
        const int &console = 0,
        const int &logLevel = 4);
    //! Write Object with given handle to log file.
    /*! Writes a warning message to log file
        if no object is found with given handle.
    */
    void logObject(const std::string &handle);
    //! Write all Objects to log file.
    /*! Takes no action if ObjectHandler
        repository is empty.
    */
    void logAllObjects();
    //@}
}

#endif


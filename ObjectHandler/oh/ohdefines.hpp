
/*
 Copyright (C) 2005, 2006, 2007 Eric Ehlers
 Copyright (C) 2004 Ferdinando Ametrano

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it
 under the terms of the QuantLib license.  You should have received a
 copy of the license along with this program; if not, please email
 <quantlib-dev@lists.sf.net>. The license is also available online at
 <http://quantlib.org/license.shtml>.

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
*/

/*! \file
    \brief Preprocessor directives for ObjectHandler compilation
*/

#ifndef oh_defines_hpp
#define oh_defines_hpp

#include <boost/config.hpp>
#include <boost/version.hpp>
// Boost version 1.34.1 is now required for the boost::filesystem features
// used in classes derived from ObjectHandler::SerializationFactory in
// downstream applications e.g. QuantLibAddin.  If you don't require that
// functionality you can probably disable this test without problems.
#if BOOST_VERSION < 103401
    #error using an old version of Boost, please update.
#endif

//! version string
#ifdef _DEBUG
    #define OBJHANDLER_VERSION "0.9.0-debug"
#else
    #define OBJHANDLER_VERSION "0.9.0"
#endif

//! version hexadecimal number
#define OBJHANDLER_HEX_VERSION 0x000900f0
//! version string for output lib name
#define OBJHANDLER_LIB_VERSION "0_9_0"

#include <cctype>
#if defined(BOOST_NO_STDC_NAMESPACE)
    namespace std { using ::tolower; using ::toupper; }
#endif

#ifdef XLL_EXPORTS
    #define DLL_API __declspec(dllexport)
    #define COMPILING_XLL_DYNAMIC
#elif XLL_IMPORTS
    #define DLL_API __declspec(dllimport)
    #define COMPILING_XLL_DYNAMIC
#elif XLL_STATIC
    #define DLL_API
#else
    #define DLL_API
#endif

#if defined BOOST_MSVC
#pragma warning(disable : 4996)
#pragma warning(disable : 4244)
#endif

/*! \def OH_GET_OBJECT
    Get a boost shared pointer to a class derived from Object.
*/
#define OH_GET_OBJECT( NAME, ID, OBJECT_CLASS ) \
    boost::shared_ptr< OBJECT_CLASS > NAME; \
    ObjectHandler::Repository::instance().retrieveObject(NAME, ID);

/*! \def OH_GET_REFERENCE
    Get a boost shared pointer to the client library object referenced by an
    ObjectHandler::Object.
*/
#define OH_GET_REFERENCE( NAME, ID, OBJECT_CLASS, LIBRARY_CLASS ) \
    OH_GET_OBJECT(NAME ## temp, ID, OBJECT_CLASS ) \
    boost::shared_ptr<LIBRARY_CLASS> NAME; \
    NAME ## temp->getLibraryObject(NAME);

/*! \def OH_GET_REFERENCE_DEFAULT
    Like OH_GET_REFERENCE but only attempt retrieval if id supplied.
*/
#define OH_GET_REFERENCE_DEFAULT( NAME, ID, OBJECT_CLASS, LIBRARY_CLASS ) \
    boost::shared_ptr<LIBRARY_CLASS> NAME; \
    if (!ID.empty()) { \
        OH_GET_OBJECT(NAME ## temp, ID, OBJECT_CLASS ) \
        NAME ## temp->getLibraryObject(NAME); \
    }

/*! \def OH_GET_UNDERLYING
    Get a direct reference to the underlying object wrapped by the
    ObjectHandler::Object.
*/
#define OH_GET_UNDERLYING( NAME, ID, OBJECT_CLASS, LIBRARY_CLASS ) \
    OH_GET_REFERENCE(NAME ## temp, ID, OBJECT_CLASS, LIBRARY_CLASS ) \
    const LIBRARY_CLASS &NAME = *(NAME ## temp.get());

/*! \def OH_GET_UNDERLYING_NONCONST
    Like OH_GET_UNDERLYING but without const qualifier.
*/
#define OH_GET_UNDERLYING_NONCONST( NAME, ID, OBJECT_CLASS, LIBRARY_CLASS ) \
    OH_GET_REFERENCE(NAME ## temp, ID, OBJECT_CLASS, LIBRARY_CLASS ) \
    LIBRARY_CLASS &NAME = *(NAME ## temp.get());

/*! \def OH_LOG_MESSAGE(message)
    Log the given message.
*/

#define OH_LOG_MESSAGE(message) \
do { \
    std::ostringstream _oh_msg_stream; \
    _oh_msg_stream << message; \
    ObjectHandler::logMessage(_oh_msg_stream.str()); \
} while (false)

/*! \def OH_LOG_ERROR(message)
    Log the given error message.
*/

#define OH_LOG_ERROR(message) \
do { \
    std::ostringstream _oh_msg_stream; \
    _oh_msg_stream << message; \
    ObjectHandler::logMessage(_oh_msg_stream.str(), 1); \
} while (false)

#endif


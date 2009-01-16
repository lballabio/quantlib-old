/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2005, 2006, 2007, 2009 Eric Ehlers
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

#if BOOST_VERSION < 103401
    #error using an old version of Boost, please update to 1.34.1 or higher.
#endif

// Workaround for problem with boost 1.34.1 + gcc 4.3.2 (or all 4.3.x?) for
// #include <boost/serialization/vector.hpp>
#if (BOOST_VERSION == 103401 && __GNUC__ == 4 && __GNUC_MINOR__ == 3) // && __GNUC_PATCHLEVEL__ == 2
#define BOOST_NO_INTRINSIC_INT64_T
#endif

//! Version string.
#ifdef _DEBUG
    #define OBJHANDLER_VERSION "0.9.8-debug"
#else
    #define OBJHANDLER_VERSION "0.9.8"
#endif

//! Version hexadecimal number.
#define OBJHANDLER_HEX_VERSION 0x000908f0
//! Version string for output lib name.
#define OBJHANDLER_LIB_VERSION "0_9_8"

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
#pragma warning(disable : 4996)     // Using a deprecated function
//#pragma warning(disable : 4244)     // An integer type is converted to a smaller integer type
#pragma warning(disable : 4267)     // Sending a size_t to an output stream
// non-ASCII characters - Disabling this warning here is ineffective
// and the change has been made instead under project properties
//#pragma warning(disable: 4819)

// behavior change: an object of POD type constructed with
// an initializer of the form () will be default-initialized
#pragma warning(disable:4345)

//#pragma warning(disable:4503)

#endif

#if defined BOOST_MSVC
#define STRICMP _stricmp
#else
#define STRICMP strcasecmp
#endif

//! Get a boost shared pointer to a class derived from Object.
#define OH_GET_OBJECT( NAME, ID, OBJECT_CLASS ) \
    boost::shared_ptr<OBJECT_CLASS > NAME; \
    ObjectHandler::Repository::instance().retrieveObject(NAME, ID);

//! Like OH_GET_OBJECT but only attempt retrieval if id supplied.
#define OH_GET_OBJECT_DEFAULT( NAME, ID, OBJECT_CLASS ) \
    boost::shared_ptr<OBJECT_CLASS > NAME; \
    if (!ID.empty()) { \
        ObjectHandler::Repository::instance().retrieveObject(NAME, ID); \
    }

//! Get a shared pointer to the library object referenced by an Object.
#define OH_GET_REFERENCE( NAME, ID, OBJECT_CLASS, LIBRARY_CLASS ) \
    OH_GET_OBJECT(NAME ## temp, ID, OBJECT_CLASS ) \
    boost::shared_ptr<LIBRARY_CLASS> NAME; \
    NAME ## temp->getLibraryObject(NAME);

//! Like OH_GET_REFERENCE but only attempt retrieval if id supplied.
#define OH_GET_REFERENCE_DEFAULT( NAME, ID, OBJECT_CLASS, LIBRARY_CLASS ) \
    boost::shared_ptr<LIBRARY_CLASS> NAME; \
    if (!ID.empty()) { \
        OH_GET_OBJECT(NAME ## temp, ID, OBJECT_CLASS ) \
        NAME ## temp->getLibraryObject(NAME); \
    }

//! Get a direct reference to the underlying object wrapped by the Object.
#define OH_GET_UNDERLYING( NAME, ID, OBJECT_CLASS, LIBRARY_CLASS ) \
    OH_GET_REFERENCE(NAME ## temp, ID, OBJECT_CLASS, LIBRARY_CLASS ) \
    const LIBRARY_CLASS &NAME = *(NAME ## temp.get());

//! Like OH_GET_UNDERLYING but without const qualifier.
#define OH_GET_UNDERLYING_NONCONST( NAME, ID, OBJECT_CLASS, LIBRARY_CLASS ) \
    OH_GET_REFERENCE(NAME ## temp, ID, OBJECT_CLASS, LIBRARY_CLASS ) \
    LIBRARY_CLASS &NAME = *(NAME ## temp.get());

//! Log the given message.
#define OH_LOG_MESSAGE(message) \
do { \
    std::ostringstream _oh_msg_stream; \
    _oh_msg_stream << message; \
    ObjectHandler::logWriteMessage(_oh_msg_stream.str()); \
} while (false)

//! Log the given error message.
#define OH_LOG_ERROR(message) \
do { \
    std::ostringstream _oh_msg_stream; \
    _oh_msg_stream << message; \
    ObjectHandler::logWriteMessage(_oh_msg_stream.str(), 1); \
} while (false)

//! An empty constructor for a class derived from Object.
#define OH_OBJ_CTOR(derived_class, base_class) \
derived_class( \
const boost::shared_ptr<ObjectHandler::ValueObject>& properties, \
bool permanent) \
: base_class(properties, permanent) {}

//! An empty class derived from Object.
#define OH_OBJ_CLASS(derived_class, base_class) \
class derived_class : \
public base_class { \
protected: \
OH_OBJ_CTOR(derived_class, base_class) \
}

//! An empty constructor for a class derived from LibraryObject.
#define OH_LIB_CTOR(derived_class, base_class) \
derived_class( \
const boost::shared_ptr<ObjectHandler::ValueObject>& properties, \
bool permanent) \
: ObjectHandler::LibraryObject<base_class>(properties, permanent) {}

//! An empty class derived from LibraryObject.
#define OH_LIB_CLASS(derived_class, base_class) \
class derived_class : \
public ObjectHandler::LibraryObject<base_class> { \
protected: \
OH_LIB_CTOR(derived_class, base_class) \
}

#endif


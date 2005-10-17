
/*
 Copyright (C) 2005 Eric Ehlers
 Copyright (C) 2004 Ferdinando Ametrano

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

#ifndef oh_defines_hpp
#define oh_defines_hpp

#include <boost/config.hpp>
#include <boost/version.hpp>
#if BOOST_VERSION < 103100
    #error using an old version of Boost, please update.
#endif

#if (_MSC_VER == 1200)
#  define OBJHANDLER_PATCH_MSVC6
#endif

//! version string
#ifdef _DEBUG
    #define OBJHANDLER_VERSION "0.1.2-debug"
#else
    #define OBJHANDLER_VERSION "0.1.2"
#endif

//! version hexadecimal number
#define OBJHANDLER_HEX_VERSION 0x000102f0
//! version string for output lib name
#define OBJHANDLER_LIB_VERSION "0_1_2"

#include <cctype>
#if defined(BOOST_NO_STDC_NAMESPACE)
    namespace std { using ::tolower; using ::toupper; }
#endif

#ifdef XLL_EXPORTS
    #define DLL_API __declspec(dllexport)
    #define COMPILING_XLL
#elif XLL_IMPORTS
    #define DLL_API __declspec(dllimport)
    #define COMPILING_XLL
#elif XLL_STATIC
    #define DLL_API
    #define COMPILING_XLL
#else
    #define DLL_API
#endif

#if defined BOOST_MSVC
#pragma warning(disable : 4996)
#pragma warning(disable : 4244)
#endif

#endif


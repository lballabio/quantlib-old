
/*
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

#ifndef objhandler_defines_h
#define objhandler_defines_h

#include <boost/config.hpp>
#include <boost/version.hpp>
#if BOOST_VERSION < 103100
    #error using an old version of Boost, please update.
#endif

//! version string
#ifdef _DEBUG
    #define OBJHANDLER_VERSION "0.1.1-debug"
#else
    #define OBJHANDLER_VERSION "0.1.1"
#endif

//! version hexadecimal number
#define OBJHANDLER_HEX_VERSION 0x000101f0
//! version string for output lib name
#define OBJHANDLER_LIB_VERSION "0_1_1"

#if defined(HAVE_CONFIG_H)     // Dynamically created by configure
    #include <oh/config.hpp>
/* Use BOOST_MSVC instead of _MSC_VER since some other vendors 
   (Metrowerks, for example) also #define _MSC_VER
*/
#elif defined BOOST_MSVC       // Microsoft Visual C++
#  include <oh/autolink.hpp>
#endif

#include <cctype>
#if defined(BOOST_NO_STDC_NAMESPACE)
    namespace std { using ::tolower; using ::toupper; }
#endif

#endif


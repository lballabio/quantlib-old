/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2006, 2007 Eric Ehlers
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

#ifndef qladdindefines_h
#define qladdindefines_h

#include <boost/config.hpp>
#include <boost/version.hpp>
#if BOOST_VERSION < 103100
    #error using an old version of Boost, please update.
#endif

#include <oh/ohdefines.hpp>
#if OBJHANDLER_HEX_VERSION < 0x000908f0
    #error using an old version of ObjectHandler, please update.
#endif

#include <ql/version.hpp>
#if QL_HEX_VERSION < 0x000908f0
    #error using an old version of QuantLib, please update.
#endif

//! version string
#ifdef _DEBUG
    #define QLADDIN_VERSION "0.9.8-debug"
#else
    #define QLADDIN_VERSION "0.9.8"
#endif

//! version hexadecimal number
#define QLADDIN_HEX_VERSION 0x000908f0
//! version string for output lib name
#define QLADDIN_LIB_VERSION "0_9_8"

#if defined BOOST_MSVC
#pragma warning(disable : 4996)     // Using a deprecated function
#endif

#endif


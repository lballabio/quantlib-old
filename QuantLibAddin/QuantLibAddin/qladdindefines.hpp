
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

#ifndef qladdin_defines_h
#define qladdin_defines_h

//! version string
#ifdef _DEBUG
    #define QLADDIN_VERSION "0.3.8-debug"
#else
    #define QLADDIN_VERSION "0.3.8"
#endif

//! version hexadecimal number
#define QLADDIN_HEX_VERSION 0x000308f0
//! version string for output lib name
#define QLADDIN_LIB_VERSION "0_3_8"

/* Use BOOST_MSVC instead of _MSC_VER since some other vendors (Metrowerks,
   for example) also #define _MSC_VER

*/
#include <boost/config.hpp>
#ifdef BOOST_MSVC
#  include <QuantLibAddin/autolink.hpp>
#endif

#include <QuantLibAddin/objects/objectoption.hpp>
#include <QuantLibAddin/objects/objectstochastic.hpp>
#include <ql/quantlib.hpp>
#include <ObjectHandler/objecthandler.hpp>

#endif


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

#ifndef qladdin_version_h
#define qladdin_version_h

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

#endif


/*
 Copyright (C) 2002, 2003 Ferdinando Ametrano

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it under the
 terms of the QuantLib license.  You should have received a copy of the
 license along with this program; if not, please email ferdinando@ametrano.net
 The license is also available online at http://quantlib.org/html/license.html

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
*/
/*! \file qlxl.hpp
    \brief QuantLib Excel add-in

    \fullpath
    qlxl/%qlxl.hpp
*/

// $Id$

#ifndef qlxl_h
#define qlxl_h

#include <xlw/xlw.h>
#include <ql/quantlib.hpp>


//! version hexadecimal number
#define QLXL_HEX_VERSION 0x000304a1

//! version string
#ifdef QL_DEBUG
    #define QLXL_VERSION "0.3.4a1-debug"
#else
    #define QLXL_VERSION "0.3.4a1"
#endif


#if QL_HEX_VERSION < 0x000303f0
    #error using an old version of QuantLib, please update
#endif

#if XLW_HEX_VERSION < 0x010203a0
    #error using an old version of xlw, please update
#endif

#endif


/*
 Copyright (C) 2005 Eric Ehlers

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

#ifndef xlsdkdefines_hpp
#define xlsdkdefines_hpp

#include <windows.h>
#include <xlsdk/xlcall.h>
#include <xlsdk/framewrk.hpp>
#include <boost/config.hpp>

#define DLLEXPORT extern "C" __declspec(dllexport)

#define VECTOR "<VECTOR>"
#define MATRIX "<MATRIX>"
#define XL_MAX_STR_LEN 255

// parameters registered with Excel as OPER (P) are declared as XLOPER
#define OPER XLOPER

typedef struct {
    WORD rows;
    WORD columns;
    double array[1];
} FP;

// suppress VC8 'strncpy deprecated' warning
//#if defined BOOST_MSVC
//#pragma warning(disable : 4996)
//#endif

#endif


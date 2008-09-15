/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2007 Eric Ehlers

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
    \brief General utility functions for the Excel platform
*/

#ifndef ohxl_utilities_hpp
#define ohxl_utilities_hpp

#include <oh/ohdefines.hpp>
#include <xlsdk/xlsdkdefines.hpp>

//! Free any memory associated with the XLOPER.
DLL_API void freeOper(XLOPER *px);

//! Determine whether the input value comprises a list.
/*! Returns true if the input value is a string containing
    one or more ',' or ';' characters.  Returns false otherwise.
*/
DLL_API bool isList(const OPER *xValue);

//! Convert a delimited list into a vector.
/*! The input value is a string containing one or more values
    delimited by ',' or ';'.  These are extracted and written
    to a vector which is written to the second parameter.
*/
DLL_API void splitOper(const OPER *xFrom, OPER *xTo);

#endif


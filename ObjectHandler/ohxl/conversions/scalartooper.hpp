/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2005, 2006, 2007 Eric Ehlers
 Copyright (C) 2008 Plamen Neykov

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
    \brief Conversion function scalarToOper - convert an Excel OPER to a scalar value
*/

#ifndef ohxl_conversions_scalartooper_hpp
#define ohxl_conversions_scalartooper_hpp

#include <oh/ohdefines.hpp>
#include <xlsdk/xlsdkdefines.hpp>
#include <string>
#include <oh/property.hpp>

namespace ObjectHandler {

    //! Convert a long to an OPER.
    DLL_API void scalarToOper(const long &value, OPER &xLong, bool expandVector = true);
    //! Convert a double to an OPER.
    DLL_API void scalarToOper(const double &value, OPER &xDouble, bool expandVector = true);
    //! Convert a bool to an OPER.
    DLL_API void scalarToOper(const bool &value, OPER &xBoolean, bool expandVector = true);
    //! Convert a char * to an OPER.
    DLL_API void scalarToOper(const char *value, OPER &xChar, bool expandVector = true);
    //! Convert a string to an OPER.
    DLL_API void scalarToOper(const std::string &value, OPER &xString, bool expandVector = true);
    //! Convert a property_t to an OPER.
    DLL_API void scalarToOper(const property_t &value, OPER &xAny, bool expandVector = true);

}

#endif


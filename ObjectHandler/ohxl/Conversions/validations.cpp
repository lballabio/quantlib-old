/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2005 Plamen Neykov
 Copyright (C) 2004, 2005, 2006, 2007 Eric Ehlers

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

#include <ohxl/Conversions/validations.hpp>
#include <oh/exception.hpp>
#include <sstream>

namespace ObjectHandler {

    bool validateMulti(const OPER *xMulti) {
        for (int i=0; i<xMulti->val.array.rows * xMulti->val.array.columns; ++i)
            if (xMulti->val.array.lparray[i].xltype & xltypeErr)
                return false;
        return true;
    }

    DLL_API void validateRange(const OPER *xRange, const std::string &parameterName) {
        OH_REQUIRE(!(xRange->xltype & xltypeErr), "Parameter '" << parameterName << "' has error value");
        OH_REQUIRE(!(xRange->xltype & xltypeMulti) || validateMulti(xRange), 
            "Parameter '" << parameterName << "' has error value");
    }

    DLL_API void stringToChar(const std::string &value, char *c) {
        int len = __min(XL_MAX_STR_LEN - 1, value.length());
        strncpy(c, value.c_str(), len);
        c[len] = 0;
    }

}


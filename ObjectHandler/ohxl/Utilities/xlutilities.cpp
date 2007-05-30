
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

#include <oh/utilities.hpp>
#include <ohxl/Utilities/xlutilities.hpp>
#include <ohxl/Conversions/opertoscalar.hpp>
#include <ohxl/Conversions/vectortooper.hpp>

DLL_API void freeOper(XLOPER *px) {
    if (px->xltype & xltypeStr && px->val.str)
        delete [] px->val.str;
    else if (px->xltype & xltypeMulti && px->val.array.lparray) {
        int size = px->val.array.rows * px->val.array.columns;
        for (int i=0; i<size; ++i)
            if (px->val.array.lparray[i].xltype & xltypeStr
            &&  px->val.array.lparray[i].val.str)
                delete [] px->val.array.lparray[i].val.str;
        delete [] px->val.array.lparray;
    }
}

DLL_API void splitOper(const OPER *xFrom, OPER *xTo) {
    std::string text;
    ObjectHandler::operToScalar(*xFrom, text);
    std::vector<std::string> vec = ObjectHandler::split(text, ",;", false);
    ObjectHandler::vectorToOper(vec, *xTo, false);
}

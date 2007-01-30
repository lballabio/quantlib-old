
/*
 Copyright (C) 2007 Ferdinando Ametrano
 Copyright (C) 2005, 2006 Eric Ehlers

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

#ifndef ohxl_conversions_vectortooper_hpp
#define ohxl_conversions_vectortooper_hpp

#include <ohxl/Conversions/scalartooper.hpp>
#include <ohxl/functioncall.hpp>
#include <vector>

namespace ObjHandler {

    // FIXME emergency hack to support automatic coercion of input strings 
    // to vectors thru a call to ohSplit().  Normally the functions below 
    // set xlbitDLLFree but this needs to be disabled in the above case.
    // This is accomplished with input variable dllToFree which defaults
    // to true to preserve the old behavior.

    template <class T>
    void vectorToOper(const std::vector<T> &v, OPER &xVector, bool dllToFree = true) {
        vectorToOper(v.begin(), v.end(), xVector, dllToFree);
    }

    template <class T>
    inline void vectorToOper(T begin, T end, OPER &xVector, bool dllToFree = true) {
        std::size_t size = end-begin;
        if (size==0) {
            xVector.xltype = xltypeErr;
            xVector.val.err = xlerrNA;
            return;
        }

        if (FunctionCall::instance().getCallerDimensions() == Row) {
            xVector.val.array.columns = size;
            xVector.val.array.rows    = 1;
        } else {
            xVector.val.array.rows    = size;
            xVector.val.array.columns = 1;
        }
        xVector.val.array.lparray = new OPER[size]; 
        if (!xVector.val.array.lparray)
            throw Exception("vectorToOper: error on call to new");
        xVector.xltype = xltypeMulti;
        if (dllToFree)
            xVector.xltype = (xltypeMulti | xlbitDLLFree);
        else
            xVector.xltype = xltypeMulti;
        for (unsigned int i=0; i<size; ++i, ++begin)
            scalarToOper(*begin, xVector.val.array.lparray[i], dllToFree);
    }


}

#endif

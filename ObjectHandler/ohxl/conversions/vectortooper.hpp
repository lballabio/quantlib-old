/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2007 Ferdinando Ametrano
 Copyright (C) 2005, 2006, 2007 Eric Ehlers

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
    \brief Conversion function vectorToOper - Convert STL vector to Excel OPER
*/

#ifndef ohxl_conversions_vectortooper_hpp
#define ohxl_conversions_vectortooper_hpp

#include <ohxl/ohxldefines.hpp>
#include <ohxl/conversions/scalartooper.hpp>
#include <ohxl/functioncall.hpp>
#include <vector>

namespace ObjectHandler {

    //@{
    //! Convert type std::vector<T> to an Excel OPER.
    /*! If dllToFree is true then the function sets the xlbitDLLFree bit if necessary.
    */
    template <class T>
    void vectorToOper(/*const*/ std::vector<T> &v, OPER &xVector, bool dllToFree = true) {
        vectorToOper<std::vector<T>::const_iterator>(v.begin(), v.end(), xVector, dllToFree);
    }

    template <class T>
    void vectorToOper(T begin, T end, OPER &xVector, bool dllToFree = true) {
        std::size_t size = end - begin;
        if (size == 0) {
            xVector.xltype = xltypeErr;
            xVector.val.err = xlerrNA;
            return;
        }

        if (FunctionCall::instance().callerDimensions() == CallerDimensions::Row) {
            xVector.val.array.columns = size;
            xVector.val.array.rows = 1;
        } else {
            xVector.val.array.rows = size;
            xVector.val.array.columns = 1;
        }

        xVector.val.array.lparray = new OPER[size]; 
        xVector.xltype = xltypeMulti;
        if (dllToFree)
            xVector.xltype |= xlbitDLLFree;
        for (unsigned int i=0; i<size; ++i, ++begin)
            scalarToOper(*begin, xVector.val.array.lparray[i], dllToFree, false);
    }
    //@}

}

#endif


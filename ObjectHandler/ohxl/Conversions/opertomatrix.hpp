
/*
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
    \brief Conversion function operToMatrix - convert an OPER to a matrix.
*/

#ifndef ohxl_conversions_opertomatrix_hpp
#define ohxl_conversions_opertomatrix_hpp

#include <ohxl/Conversions/opertoscalar.hpp>
#include <vector>

namespace ObjectHandler {

    //! Convert an Excel OPER to type std::vector<std::vector<T> >.
    template <class T>
    std::vector<std::vector<T> > operToMatrix(const OPER &xMatrix) {

        if (xMatrix.xltype & xltypeNil
        ||  xMatrix.xltype & xltypeMissing
        ||  xMatrix.xltype & xltypeErr && xMatrix.val.err == xlerrNA)
            return std::vector<std::vector<T> >();

        OH_REQUIRE(!(xMatrix.xltype & xltypeErr), "input value has type=error");

        Xloper xTemp;
        const OPER *xMulti;

        if (xMatrix.xltype == xltypeMulti)
            xMulti = &xMatrix;
        else {
            Excel(xlCoerce, &xTemp, 2, &xMatrix, TempInt(xltypeMulti));
            xMulti = &xTemp;
        }

        std::vector<std::vector<T> > ret;
        ret.reserve(xMulti->val.array.rows);
        for (int i=0; i<xMulti->val.array.rows; ++i) {
            std::vector<T> row;
            row.reserve(xMulti->val.array.columns);
            for (int j=0; j<xMulti->val.array.columns; ++j) {
                T value;
                operToScalar(xMulti->val.array.lparray[i * xMulti->val.array.columns + j], value);
                row.push_back(value);
            }
            ret.push_back(row);
        }

        return ret;
    }

    //! Convert an Excel FP to type std::vector<std::vector<T> >.
    template <class T>
    std::vector<std::vector<T> > fpToMatrix(const FP &fpMatrix) {
        std::vector<std::vector<T> > ret;
        ret.reserve(fpMatrix.rows);
        for (int i=0; i<fpMatrix.rows; ++i) {
            std::vector<T> row;
            row.reserve(fpMatrix.columns);
            for (int j=0; j<fpMatrix.columns; ++j)
                row.push_back(fpMatrix.array[i * fpMatrix.columns + j]);
            ret.push_back(row);
        }
        return ret;
    }

}

#endif


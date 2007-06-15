
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
    \brief Conversion function operToMatrix - convert an OPER to a matrix
*/

#ifndef ohxl_conversions_opertomatrix_hpp
#define ohxl_conversions_opertomatrix_hpp

#include <ohxl/convert_oper.hpp>
#include <vector>

namespace ObjectHandler {

    //! Helper template wrapper for operToMatrixImpl
    /*! Accept an OPER as input and wrap this in class ConvertOper.
        This simplifies syntax in client applications.
    */
    template <class T>
    std::vector<std::vector<T> > operToMatrix(
        const OPER &xMatrix, 
        const std::string &paramName) {

        return operToMatrixImpl<T>
            (ConvertOper(xMatrix, false), paramName);
    }

    //! Convert a value of type ConvertOper to a matrix.
    template <class T>
    std::vector<std::vector<T> > operToMatrixImpl(
        const ConvertOper &xMatrix, 
        const std::string &paramName) {

        try {
            if (xMatrix.missing()) return std::vector<std::vector<T> >();

            OH_REQUIRE(!xMatrix.error(), "input value has type=error");

            const OPER *xMulti;
            Xloper xCoerce;  // Freed automatically

            if (xMatrix->xltype == xltypeMulti)
                xMulti = xMatrix.get();
            else {
                Excel(xlCoerce, &xCoerce, 2, xMatrix.get(), TempInt(xltypeMulti));
                xMulti = &xCoerce;
            }

            std::vector<std::vector<T> > ret;
            ret.reserve(xMulti->val.array.rows);
            for (int i=0; i<xMulti->val.array.rows; ++i) {
                std::vector<T> row;
                row.reserve(xMulti->val.array.columns);
                for (int j=0; j<xMulti->val.array.columns; ++j) {
                    row.push_back(operToScalar<T>(xMulti->val.array.lparray[i * xMulti->val.array.columns + j]));                
                }
                ret.push_back(row);
            }

            return ret;
        } catch (const std::exception &e) {
            OH_FAIL("operToMatrixImpl: error converting parameter '" << paramName 
                << "' to type '" << typeid(T).name() << "' : " << e.what());
        }
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


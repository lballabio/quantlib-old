
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
    \brief Conversion function operToVector - convert an OPER to a std::vector.
*/

#ifndef ohxl_conversions_opertovector_hpp
#define ohxl_conversions_opertovector_hpp

#include <ohxl/Conversions/opertoscalar.hpp>
#include <ohxl/Utilities/xlutilities.hpp>
#include <vector>

namespace ObjectHandler {

    //! Convert an Excel OPER to a std::vector of type T.
    template <class T>
    std::vector<T> operToVector(const OPER &xVector, 
                   const std::string &paramName) {

        if (xVector.xltype & xltypeNil
        ||  xVector.xltype & xltypeMissing
        ||  xVector.xltype & xltypeErr && xVector.val.err == xlerrNA)
            return std::vector<T>();

        OH_REQUIRE(!(xVector.xltype & xltypeErr),
            "input value '" << paramName << "' has type=error");

        OPER xTemp;
        bool excelToFree = false;
        bool xllToFree = false;
        try {
            const OPER *xMulti;

            if (xVector.xltype == xltypeMulti) {
                xMulti = &xVector;
            } else if (xVector.xltype == xltypeStr) {
                splitOper(&xVector, &xTemp);
                xMulti = &xTemp;
                xllToFree = true;
            } else {
                Excel(xlCoerce, &xTemp, 2, &xVector, TempInt(xltypeMulti));
                xMulti = &xTemp;
                excelToFree = true;
            }

            std::vector<T> ret;
            ret.reserve(xMulti->val.array.rows * xMulti->val.array.columns);
            for (int i=0; i<xMulti->val.array.rows * xMulti->val.array.columns; ++i) {
                T value;
                operToScalar(xMulti->val.array.lparray[i], value);
                ret.push_back(value);
            }

            if (excelToFree) {
                Excel(xlFree, 0, 1, &xTemp);
            } else if (xllToFree) {
                freeOper(&xTemp);
            }

            return ret;
        } catch (const std::exception &e) {
            if (excelToFree) {
                Excel(xlFree, 0, 1, &xTemp);
            } else if (xllToFree) {
                freeOper(&xTemp);
            }
            OH_FAIL("operToVector: error converting parameter '" << paramName << "' : " << e.what());
        }
    }

    //! Convert an Excel FP to a std::vector of type T.
    template <class T>
    std::vector<T> fpToVector(const FP &fpVector) {
        std::vector<T> ret;
        ret.reserve(fpVector.rows * fpVector.columns);
        for (int i=0; i<fpVector.rows * fpVector.columns; ++i)
            ret.push_back(fpVector.array[i]);
        return ret;
    }

}

#endif


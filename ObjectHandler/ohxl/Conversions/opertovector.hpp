
/*
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

#ifndef ohxl_conversions_opertovector_hpp
#define ohxl_conversions_opertovector_hpp

#include <ohxl/Conversions/opertoscalar.hpp>
#include <ohxl/Utilities/utilities.hpp>
#include <vector>

namespace ObjHandler {

    template <class T>
    std::vector<T> operToVector(const OPER &xVector) {
        OPER xTemp;
        bool excelToFree = false;
        bool xllToFree = false;
        try {
            if (xVector.xltype & xltypeErr)
                throw Exception("input value has type=error");
            if (xVector.xltype & (xltypeMissing | xltypeNil))
                return std::vector<T>();

            const OPER *xMulti;

            if (xVector.xltype == xltypeMulti) {
                xMulti = &xVector;
            } else if (xVector.xltype == xltypeStr) {
                std::string text;
                operToScalar(xVector, text);
                std::vector<std::string> vec = split(text, ",;", false);
                vectorToOper(vec, xTemp);
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
            OH_FAIL("operToVector: " << e.what());
        }
    }

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

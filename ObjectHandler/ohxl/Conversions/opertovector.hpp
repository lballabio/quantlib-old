
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
#include <vector>

namespace ObjHandler {

    template <class T>
    std::vector<T> operToVector(const OPER &xVector) {
        OPER xTemp;
        bool needToFree = false;
        try {
            if (xVector.xltype & xltypeErr)
                throw Exception("input value has type=error");
            if (xVector.xltype & (xltypeMissing | xltypeNil))
                return std::vector<T>();

            const OPER *xMulti;

            if (xVector.xltype == xltypeMulti)
                xMulti = &xVector;
            else {
                Excel(xlCoerce, &xTemp, 2, &xVector, TempInt(xltypeMulti));
                xMulti = &xTemp;
                needToFree = true;
            }

            std::vector<T> ret;
            ret.reserve(xMulti->val.array.rows * xMulti->val.array.columns);
            for (int i=0; i<xMulti->val.array.rows * xMulti->val.array.columns; ++i) {
                T value;
                operToScalar(xMulti->val.array.lparray[i], value);
                ret.push_back(value);
            }

            if (needToFree)
                Excel(xlFree, 0, 1, &xTemp);

            return ret;
        } catch (const std::exception &e) {
            if (needToFree)
                Excel(xlFree, 0, 1, &xTemp);
            std::ostringstream msg;
            msg << "operToVector: " << e.what();
            throw Exception(msg.str());
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

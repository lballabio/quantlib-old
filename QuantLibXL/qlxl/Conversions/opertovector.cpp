
/*
 Copyright (C) 2006 Eric Ehlers

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

#include <qlxl/Conversions/opertovector.hpp>
#include <ohxl/Utilities/utilities.hpp>

namespace ObjHandler {

    //QuantLib::Array operToVector(const FP &fpVector) {
    //    QuantLib::Array a(fpVector.rows * fpVector.columns);
    //    for (int i=0; i<fpVector.rows * fpVector.columns; ++i)
    //        a[i] = fpVector.array[i];
    //    return a;
    //}

    QuantLib::Array operToVector(const OPER &xVector) {
        OPER xTemp;
        bool excelToFree = false;
        bool xllToFree = false;
        try {
            if (xVector.xltype & xltypeErr)
                throw Exception("input value has type=error");
            if (xVector.xltype & (xltypeMissing | xltypeNil))
                return QuantLib::Array();

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

            int size = xMulti->val.array.rows * xMulti->val.array.columns;
            QuantLib::Array a(size);
            for (int i=0; i<size; ++i) {
                operToScalar(xMulti->val.array.lparray[i], a[i]);
            }

            if (excelToFree) {
                Excel(xlFree, 0, 1, &xTemp);
            } else if (xllToFree) {
                freeOper(&xTemp);
            }

            return a;
        } catch (const std::exception &e) {
            if (excelToFree) {
                Excel(xlFree, 0, 1, &xTemp);
            } else if (xllToFree) {
                freeOper(&xTemp);
            }
            OH_FAIL("operToVector: " << e.what());
        }
    }
}


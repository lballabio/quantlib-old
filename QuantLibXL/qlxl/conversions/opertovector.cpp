
/*
 Copyright (C) 2006 Eric Ehlers

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

#include <qlxl/conversions/opertovector.hpp>
#include <ohxl/utilities/xlutilities.hpp>

namespace QuantLibXL {

    QuantLib::Array operToQlArray(const OPER &xVector, 
            const std::string paramName) {
        OPER xTemp;
        bool excelToFree = false;
        bool xllToFree = false;
        try {
            OH_REQUIRE(!(xVector.xltype & xltypeErr), 
                "input value '" << paramName << "' has type=error");
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
                a[i] = ObjectHandler::operToScalar<double>(xMulti->val.array.lparray[i]);
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
            OH_FAIL("operToVector: error converting parameter '" << paramName << "' : " << e.what());
        }
    }
}


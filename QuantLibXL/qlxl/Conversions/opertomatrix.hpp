
/*
 Copyright (C) 2006, 2007 Eric Ehlers

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

#ifndef qlxl_conversions_opertomatrix_hpp
#define qlxl_conversions_opertomatrix_hpp

#include <oh/ohdefines.hpp>
#include <xlsdk/xlsdkdefines.hpp>
#include <ql/handle.hpp>
#include <ql/quote.hpp>
#include <ql/math/matrix.hpp>
#include <vector>

// Override functions in ObjectHandler namespace
namespace ObjectHandler {

    QuantLib::Matrix operToMatrix(const FP &fpVector);

    //std::vector<std::vector<QuantLib::RelinkableHandle<QuantLib::Quote> > > 
    //fpToMatrixHandle(const FP &fpVector);

    template <class qloClass, class qlClass>
    std::vector<std::vector<QuantLib::RelinkableHandle<qlClass> > > 
    operToMatrixHandle(const OPER &xMatrix) {

        if (xMatrix.xltype & xltypeNil
        ||  xMatrix.xltype & xltypeMissing
        ||  xMatrix.xltype & xltypeErr && xMatrix.val.err == xlerrNA)
            return std::vector<std::vector<QuantLib::RelinkableHandle<qlClass> > >();
        OH_REQUIRE(!(xMatrix.xltype & xltypeErr), "input value has type=error");

        Xloper xTemp;
        const OPER *xMulti;

        if (xMatrix.xltype == xltypeMulti)
            xMulti = &xMatrix;
        else {
            Excel(xlCoerce, &xTemp, 2, &xMatrix, TempInt(xltypeMulti));
            xMulti = &xTemp;
        }

        std::vector<std::vector<QuantLib::RelinkableHandle<qlClass> > > ret;
        ret.reserve(xMulti->val.array.rows);
        for (int i=0; i<xMulti->val.array.rows; ++i) {
            std::vector<QuantLib::RelinkableHandle<qlClass> > row;
            row.reserve(xMulti->val.array.columns);
            for (int j=0; j<xMulti->val.array.columns; ++j) {
                std::string id;
                operToScalar(xMulti->val.array.lparray[i * xMulti->val.array.columns + j], id);
                OH_GET_OBJECT(obj, id, ObjectHandler::Object)
                QuantLib::RelinkableHandle<qlClass> handle =
                    QuantLibAddin::CoerceHandle<qloClass, qlClass>()(obj);
                row.push_back(handle);
            }
            ret.push_back(row);
        }

        return ret;

    }

}

#endif


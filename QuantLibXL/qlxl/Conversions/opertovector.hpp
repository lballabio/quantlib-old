
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

#ifndef qlxl_conversions_opertovector_hpp
#define qlxl_conversions_opertovector_hpp

#include <ohxl/Conversions/all.hpp>
#include <ql/Math/array.hpp>
#include <qlxl/Conversions/opertoscalar.hpp>

namespace ObjHandler {

    QuantLib::Array operToVector(const FP &fpVector);

    template <class T>
    std::vector<T> fpToVectorLibrary(const FP &fpVector) {
        std::vector<T> ret;
        ret.reserve(fpVector.rows * fpVector.columns);
        for (int i=0; i<fpVector.rows * fpVector.columns; ++i) {
            T item;
            cppToLibrary(fpVector.array[i], item);
            ret.push_back(item);
        }
        return ret;
    }

    template <class qlClass, class qloClass>
    std::vector<boost::shared_ptr<qlClass> >
    operToObjectVector(const OPER &xVector) {
        OPER xTemp;
        bool needToFree = false;
        try {
            if (xVector.xltype & xltypeErr)
                throw Exception("input value is #NULL (xltypeErr)");
            if (xVector.xltype & (xltypeMissing | xltypeNil))
                return std::vector<boost::shared_ptr<qlClass> >();

            const OPER *xMulti;

            if (xVector.xltype == xltypeMulti)
                xMulti = &xVector;
            else {
                Excel(xlCoerce, &xTemp, 2, &xVector, TempInt(xltypeMulti));
                xMulti = &xTemp;
                needToFree = true;
            }

            std::vector<boost::shared_ptr<qlClass> > ret;
            ret.reserve(xMulti->val.array.rows * xMulti->val.array.columns);
            for (int i=0; i<xMulti->val.array.rows * xMulti->val.array.columns; ++i) {
                std::string id;
                operToScalar(xMulti->val.array.lparray[i], id);
                OH_GET_REFERENCE(obj, id, qloClass, qlClass)
                ret.push_back(obj);
            }

            if (needToFree)
                Excel(xlFree, 0, 1, &xTemp);

            return ret;
        } catch (const std::exception &e) {
            if (needToFree)
                Excel(xlFree, 0, 1, &xTemp);
            std::ostringstream msg;
            msg << "operToObjectVector: " << e.what();
            throw Exception(msg.str());
        }
    }

}

#endif


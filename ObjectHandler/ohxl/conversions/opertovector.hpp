/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

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
    \brief Conversion function operToVector - convert an OPER to a std::vector
*/

#ifndef ohxl_conversions_opertovector_hpp
#define ohxl_conversions_opertovector_hpp

#include <ohxl/utilities/xlutilities.hpp>
#include <vector>
#include <boost/shared_ptr.hpp>

namespace ObjectHandler {

    //! Helper template wrapper for operToVectorImpl
    /*! \li Accept an OPER as input and wrap this in class ConvertOper
        \li Specify VariantToScalar as the algorithm to be used

        This simplifies syntax in client applications.
    */
    template <class T>
    std::vector<T> operToVector(
        const OPER &xVector, 
        const std::string &paramName) {

        return operToVectorImpl<T, VariantToScalar<ConvertOper, T> >
            (ConvertOper(xVector, false), paramName);
    }

    //! Helper template wrapper for operToVectorImpl
    /*! \li Accept an OPER as input and wrap this in class ConvertOper
        \li Specify VariantToScalarObject as the algorithm to be used

        This simplifies syntax in client applications.
    */
    template <class LibraryClass, class ObjectClass>
    std::vector<boost::shared_ptr<LibraryClass> >
    operToObjectVector(
        const OPER &xVector, 
        const std::string &paramName) {

        return operToVectorImpl<
            boost::shared_ptr<LibraryClass>, 
            VariantToScalarObject<ConvertOper, LibraryClass, ObjectClass> >
                (ConvertOper(xVector, false), paramName);
    }

    //template <class T>
    //std::vector<T> operToEnumVector(
    //    const OPER &xVector, 
    //    const std::string &paramName) {

    //    return operToVectorImpl<T, Create<T> >(xVector, paramName);
    //}

    //! Convert a value of type ConvertOper to a vector.
    template <class T, class F>
    std::vector<T> operToVectorImpl(
        const ConvertOper &xVector, 
        const std::string &paramName) {

        OPER xSplit;            // Must be freed manually
        bool xllToFree = false;
        try {

            if (xVector.missing()) return std::vector<T>();

            OH_REQUIRE(!xVector.error(), "input value has type=error");

            const OPER *xMulti;
            Xloper xCoerce;      // Freed automatically

            if (xVector->xltype == xltypeMulti) {
                xMulti = xVector.get();
            } else if (xVector->xltype == xltypeStr) {
                splitOper(xVector.get(), &xSplit);
                xMulti = &xSplit;
                xllToFree = true;
            } else {
                Excel(xlCoerce, &xCoerce, 2, xVector.get(), TempInt(xltypeMulti));
                xMulti = &xCoerce;
            }

            std::vector<T> ret;
            ret.reserve(xMulti->val.array.rows * xMulti->val.array.columns);
            for (int i=0; i<xMulti->val.array.rows * xMulti->val.array.columns; ++i) {
                ret.push_back(F()(ConvertOper(xMulti->val.array.lparray[i])));
            }

            if (xllToFree) freeOper(&xSplit);

            return ret;
        } catch (const std::exception &e) {
            if (xllToFree) freeOper(&xSplit);
            OH_FAIL("operToVectorImpl: error converting parameter '" << paramName 
                << "' to type '" << typeid(T).name() << "' : " << e.what());
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


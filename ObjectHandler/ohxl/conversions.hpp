
/*
 Copyright (C) 2005 Eric Ehlers

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

#ifndef ohxl_conversions_hpp
#define ohxl_conversions_hpp

#include <oh/objhandlerdefines.hpp>
#include <xlsdk/xlsdkdefines.hpp>
#include <vector>
#include <string>
#include <boost/any.hpp>

namespace ObjHandler {

    DLL_API std::string getHandleFull(const std::string &handle);
    DLL_API void stringToChar(char *c, const std::string &s);
    DLL_API void stringToXloper(XLOPER &xStr, const std::string &s);
    DLL_API void scalarAnyToXloper(
        XLOPER &xScalar, 
        const boost::any &any, 
        const bool &expandVectors = false);

    DLL_API void vectorLongToXloper(XLOPER &xVector, const std::vector < long > &v);
    DLL_API void vectorDoubleToXloper(XLOPER &xVector, const std::vector < double > &v);
    DLL_API void vectorBoolToXloper(XLOPER &xVector, const std::vector < bool > &v);
    DLL_API void vectorStringToXloper(XLOPER &xVector, const std::vector < std::string > &v);
    DLL_API void vectorAnyToXloper(XLOPER &xVector, const std::vector < boost::any > &v);
    DLL_API void matrixLongToXloper(XLOPER &xMatrix, const std::vector < std::vector < long > > &vv);
    DLL_API void matrixDoubleToXloper(XLOPER &xMatrix, const std::vector < std::vector < double > > &vv);
    DLL_API void matrixBoolToXloper(XLOPER &xMatrix, const std::vector < std::vector < bool > > &vv);
    DLL_API void matrixStringToXloper(XLOPER &xMatrix, const std::vector < std::vector < std::string > > &vv);
    DLL_API void matrixAnyToXloper(XLOPER &xMatrix, const std::vector < std::vector < boost::any > > &vv);

    DLL_API long operToScalarLong(const OPER *xScalar, const long &defaultValue);
    DLL_API double operToScalarDouble(const OPER *xScalar, const double &defaultValue);
    DLL_API bool operToScalarBool(const OPER *xScalar, const bool &defaultValue);
    DLL_API std::string operToScalarString(const OPER *xScalar, const std::string &defaultValue = "");
    DLL_API boost::any operToScalarAny(const OPER *xScalar);

    DLL_API std::vector < long > fpToVectorLong(const FP *fpVector);
    DLL_API std::vector < double > fpToVectorDouble(const FP *fpVector);
    DLL_API std::vector < long > operToVectorLong(const OPER *xVector);
    DLL_API std::vector < double > operToVectorDouble(const OPER *xVector);
    DLL_API std::vector < bool > operToVectorBool(const OPER *xVector);
    DLL_API std::vector < std::string > operToVectorString(const OPER *xVector);
    DLL_API std::vector < boost::any > operToVectorAny(const OPER *xVector);

    DLL_API std::vector < std::vector < long > >fpToMatrixLong(const FP *fpMatrix);
    DLL_API std::vector < std::vector < double > >fpToMatrixDouble(const FP *fpMatrix);
    DLL_API std::vector < std::vector < long > >operToMatrixLong(const OPER *xMatrix);
    DLL_API std::vector < std::vector < double > >operToMatrixDouble(const OPER *xMatrix);
    DLL_API std::vector < std::vector < bool > >operToMatrixBool(const OPER *xMatrix);
    DLL_API std::vector < std::vector < std::string > >operToMatrixString(const OPER *xMatrix);
    DLL_API std::vector < std::vector < boost::any > >operToMatrixAny(const OPER *xMatrix);

}

#endif

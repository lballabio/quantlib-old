
/*
 Copyright (C) 2004, 2005 Eric Ehlers

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

#ifndef xl_xlutils_hpp
#define xl_xlutils_hpp

#include <windows.h>
#include <Addins/Excel/xlcall.h>
#include <Addins/Excel/framewrk.hpp>
#include <string>

#define DLLEXPORT extern "C" __declspec(dllexport)
#define XL_MAX_STR_LEN 255

std::string xloperToString(const XLOPER &xOp);
void stringToXloper(XLOPER &xStr, const std::string &s);
std::string getHandleFull(const std::string &handle);
void stringToChar(char *c, const std::string &s);

void propertyVectorToXloper(
    LPXLOPER xArray,
    ObjHandler::Properties properties,
    const std::string &handle);

void scalarAnyToXloper(
    XLOPER &xScalar, 
    const boost::any &any, 
    const bool &expandVectors = false);

void vectorLongToXloper(XLOPER &xVector, std::vector < long > &v);
void vectorDoubleToXloper(XLOPER &xVector, std::vector < double > &v);
void vectorBoolToXloper(XLOPER &xVector, std::vector < bool > &v);
void vectorStringToXloper(XLOPER &xVector, std::vector < std::string > &v);
void vectorAnyToXloper(XLOPER &xVector, std::vector < boost::any > &v);

void matrixLongToXloper(XLOPER &xMatrix, std::vector < std::vector < long > > &vv);
void matrixDoubleToXloper(XLOPER &xMatrix, std::vector < std::vector < double > > &vv);
void matrixBoolToXloper(XLOPER &xMatrix, std::vector < std::vector < bool > > &vv);
void matrixStringToXloper(XLOPER &xMatrix, std::vector < std::vector < std::string > > &vv);
void matrixAnyToXloper(XLOPER &xMatrix, std::vector < std::vector < boost::any > > &vv);

boost::any xloperToScalarAny(const LPXLOPER xScalar);

std::vector < long > xloperToVectorLong(const LPXLOPER xVec);
std::vector < double > xloperToVectorDouble(const LPXLOPER xVec);
std::vector < bool > xloperToVectorBool(const LPXLOPER xVec);
std::vector < std::string > xloperToVectorString(const LPXLOPER xVec);
std::vector < boost::any > xloperToVectorAny(const LPXLOPER xVec);

std::vector < std::vector < long > >xloperToMatrixLong(const LPXLOPER xVec);
std::vector < std::vector < double > >xloperToMatrixDouble(const LPXLOPER xVec);
std::vector < std::vector < bool > >xloperToMatrixBool(const LPXLOPER xVec);
std::vector < std::vector < std::string > >xloperToMatrixString(const LPXLOPER xVec);
std::vector < std::vector < boost::any > >xloperToMatrixAny(const LPXLOPER xVec);

#endif



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

// parameters registered with Excel as OPER (P) are declared as XLOPER
#define OPER XLOPER

typedef struct {
    WORD rows;
    WORD columns;
    double array[1];
} FP;

std::string getHandleFull(const std::string &handle);
void stringToChar(char *c, const std::string &s);

void vectorLongToXloper(XLOPER &xVector, const std::vector < long > &v);
void vectorDoubleToXloper(XLOPER &xVector, const std::vector < double > &v);
void vectorBoolToXloper(XLOPER &xVector, const std::vector < bool > &v);
void vectorStringToXloper(XLOPER &xVector, const std::vector < std::string > &v);
void vectorAnyToXloper(XLOPER &xVector, const std::vector < boost::any > &v);

void matrixLongToXloper(XLOPER &xMatrix, const std::vector < std::vector < long > > &vv);
void matrixDoubleToXloper(XLOPER &xMatrix, const std::vector < std::vector < double > > &vv);
void matrixBoolToXloper(XLOPER &xMatrix, const std::vector < std::vector < bool > > &vv);
void matrixStringToXloper(XLOPER &xMatrix, const std::vector < std::vector < std::string > > &vv);
void matrixAnyToXloper(XLOPER &xMatrix, const std::vector < std::vector < boost::any > > &vv);

long operToScalarLong(const OPER *xScalar, const long &defaultValue);
double operToScalarDouble(const OPER *xScalar, const double &defaultValue);
bool operToScalarBool(const OPER *xScalar, const bool &defaultValue);
std::string operToScalarString(const OPER *xScalar, const std::string &defaultValue = "");
boost::any operToScalarAny(const OPER *xScalar);

std::vector < long > fpToVectorLong(const FP *fpVector);
std::vector < double > fpToVectorDouble(const FP *fpVector);
std::vector < long > operToVectorLong(const OPER *xVector);
std::vector < double > operToVectorDouble(const OPER *xVector);
std::vector < bool > operToVectorBool(const OPER *xVector);
std::vector < std::string > operToVectorString(const OPER *xVector);
std::vector < boost::any > operToVectorAny(const OPER *xVector);

std::vector < std::vector < long > >fpToMatrixLong(const FP *fpMatrix);
std::vector < std::vector < double > >fpToMatrixDouble(const FP *fpMatrix);
std::vector < std::vector < long > >operToMatrixLong(const OPER *xMatrix);
std::vector < std::vector < double > >operToMatrixDouble(const OPER *xMatrix);
std::vector < std::vector < bool > >operToMatrixBool(const OPER *xMatrix);
std::vector < std::vector < std::string > >operToMatrixString(const OPER *xMatrix);
std::vector < std::vector < boost::any > >operToMatrixAny(const OPER *xMatrix);

#endif


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

void setXLOPERString(XLOPER &xStr, const char *s);
void anyToXLOPER(const ObjHandler::any_ptr &any, XLOPER &xOp);
char *getHandleFull(const char *handle);
void setValues(LPXLOPER xArray,
               ObjHandler::Properties properties,
               const char *handle);

template < typename T >
class Conversion {
public:

    static void convertArray(const LPXLOPER xVec, T* &a, long &sz) {
        XLOPER xVal;
        if (xlretSuccess != Excel(xlCoerce, &xVal, 2, xVec, TempInt(xltypeMulti)))
            throw exception("convertArray: error on call to xlCoerce");
        sz = xVal.val.array.rows * xVal.val.array.columns;
        a = new T[sz];
        for (int i=0; i<sz; i++) {
            XLOPER xOp;
            if (xlretSuccess != Excel(xlCoerce, &xOp, 2, &xVal.val.array.lparray[i], TempInt(xltypeNum)))
                throw exception("convertArray: error on call to xlCoerce");
            a[i] = xOp.val.num;
        }
        Excel(xlFree, 0, 1, &xVal);
    }

    static void convertArray(const LPXLOPER xVec, char** &a, long &sz) {
        XLOPER xVal;
        if (xlretSuccess != Excel(xlCoerce, &xVal, 2, xVec, TempInt(xltypeMulti)))
            throw exception("convertArray: error on call to xlCoerce");
        sz = xVal.val.array.rows * xVal.val.array.columns;
        a = new char*[sz];
        for (int i=0; i<sz; i++) {
            XLOPER xOp;
            if (xlretSuccess != Excel(xlCoerce, &xOp, 2, &xVal.val.array.lparray[i], TempInt(xltypeStr)))
                throw exception("convertArray: error on call to xlCoerce");
            a[i] = new char[xOp.val.str[0] + 1];
            strncpy(a[i], &xOp.val.str[1], xOp.val.str[0]);
            a[i][xOp.val.str[0]] = 0;
            Excel(xlFree, 0, 1, &xOp);
        }
        Excel(xlFree, 0, 1, &xVal);
    }

    static void convertMatrix(const LPXLOPER xMat, T** &a, long &r, long &c) {
        XLOPER xVal;
        if (xlretSuccess != Excel(xlCoerce, &xVal, 2, xMat, TempInt(xltypeMulti)))
            throw exception("convertMatrix: error on call to xlCoerce");
        r = xVal.val.array.rows;
        c = xVal.val.array.columns;
        a = new T*[r];
        for (int i=0; i<r; i++) {
            a[i] = new T[c];
            for (int j=0; j<c; j++) {
                XLOPER xOp;
                if (xlretSuccess != Excel(xlCoerce, &xOp, 2, &xVal.val.array.lparray[i * c + j], TempInt(xltypeNum)))
                    throw exception("convertMatrix: error on call to xlCoerce");
                a[i][j] = xOp.val.num;
            }
        }
    }

};

#endif

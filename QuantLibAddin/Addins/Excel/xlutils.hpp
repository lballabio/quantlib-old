
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

std::string XLOPERtoString(const XLOPER &xOp);
void stringToXLOPER(XLOPER &xStr, const char *s);
void anyToXLOPER(const ObjHandler::any_ptr &any, XLOPER &xOp);
std::string getHandleFull(const std::string &handle);
void setValues(LPXLOPER xArray,
               ObjHandler::Properties properties,
               const std::string &handle);

template < typename T >
class Conversion {
public:

    static std::vector < T >convertVector(const LPXLOPER xVec) {
        XLOPER xVal;
        if (xlretSuccess != Excel(xlCoerce, &xVal, 2, xVec, TempInt(xltypeMulti)))
            throw exception("convertArray: error on call to xlCoerce");
        std::vector < T >ret;
        for (int i=0; i<xVal.val.array.rows * xVal.val.array.columns; i++) {
            XLOPER xOp;
            if (xlretSuccess != Excel(xlCoerce, &xOp, 2, &xVal.val.array.lparray[i], TempInt(xltypeNum)))
                throw exception("convertArray: error on call to xlCoerce");
            ret.push_back(xOp.val.num);
        }
        Excel(xlFree, 0, 1, &xVal);
        return ret;
    }

    static std::vector < std::string >convertStrVector(const LPXLOPER xVec) {
        XLOPER xVal;
        if (xlretSuccess != Excel(xlCoerce, &xVal, 2, xVec, TempInt(xltypeMulti)))
            throw exception("convertArray: error on call to xlCoerce");
        std::vector < std::string >ret;
        for (int i=0; i<xVal.val.array.rows * xVal.val.array.columns; i++)
            ret.push_back(XLOPERtoString(xVal.val.array.lparray[i]));
        Excel(xlFree, 0, 1, &xVal);
        return ret;
    }

    static std::vector < std::vector < T > >convertMatrix(const LPXLOPER xMat) {
        XLOPER xVal;
        if (xlretSuccess != Excel(xlCoerce, &xVal, 2, xMat, TempInt(xltypeMulti)))
            throw exception("convertMatrix: error on call to xlCoerce");
        std::vector < std::vector < T > >ret;
        for (int i=0; i<xVal.val.array.rows; i++) {
            std::vector < T >row;
            for (int j=0; j<xVal.val.array.columns; j++) {
                XLOPER xOp;
                if (xlretSuccess != Excel(xlCoerce, &xOp, 2, &xVal.val.array.lparray[i * xVal.val.array.columns + j], TempInt(xltypeNum)))
                    throw exception("convertMatrix: error on call to xlCoerce");
                row.push_back(xOp.val.num);
            }
            ret.push_back(row);
        }
        return ret;
    }

};

#endif


/*
 Copyright (C) 2004 Eric Ehlers

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

#include <qla/qladdin.hpp>
#include <Addins/Excel/xlutils.hpp>
#include <exception>

using namespace ObjHandler;
using namespace QuantLibAddin;

void anyToXLOPER(const any_ptr &any, XLOPER &xOp) {
    if (any->type() == typeid(int)) {
        xOp.xltype = xltypeInt;
        xOp.val.w = boost::any_cast<int>(*any);
    } else if (any->type() == typeid(double)) {
        xOp.xltype = xltypeNum;
        xOp.val.num = boost::any_cast<double>(*any);
    } else if (any->type() == typeid(std::string)) {
        std::string s = boost::any_cast<std::string>(*any);
        setXLOPERString(xOp, s.c_str());
    } else
        xOp.xltype = xltypeErr;
}

void setValues(LPXLOPER xArray, Properties properties, const char *handle) {
    xArray->xltype = xltypeMulti;
    xArray->xltype |= xlbitDLLFree;
    xArray->val.array.rows = 1;
    xArray->val.array.columns = properties.size() + 1;
    xArray->val.array.lparray = new XLOPER[properties.size() + 1]; 
    if (!xArray->val.array.lparray)
        throw("setValues: error on call to new");
    setXLOPERString(xArray->val.array.lparray[0], handle);
    for (unsigned int i = 0; i < properties.size(); i++) {
        ObjectProperty property = properties[i];
        any_ptr a = property();
        anyToXLOPER(a, xArray->val.array.lparray[i + 1]);
    }
}

std::string XLOPERtoString(LPXLOPER xOp) {
    XLOPER xStr;
    if (xlretSuccess != Excel(xlCoerce, &xStr, 2, xOp, TempInt(xltypeStr))) 
        throw exception("XLOPERtoString: error on call to xlCoerce");
    std::string s;
    s.assign(xStr.val.str + 1, xStr.val.str[0]);
    Excel(xlFree, 0, 1, &xStr);
    return s;
}

void setXLOPERString(XLOPER &xStr, const char *s) {
    xStr.xltype = xltypeStr;
    int len = __min(255, strlen(s));    // XLOPER string max length is 255
    xStr.val.str = new char[ len + 1 ]; // caller needs to delete
    if (!xStr.val.str) 
        throw exception("error calling new in function setXLOPERString");
    strncpy(xStr.val.str + 1, s, len);
    xStr.val.str[0] = len;
}

// FIXME need to call xlfree
char *getHandleFull(const char *handle) {
    XLOPER xCaller, xRef;
    if (xlretSuccess != Excel(xlfCaller, &xCaller, 0))
        throw exception("getHandleFull: error on call to xlfCaller");
    if (xlretSuccess != Excel(xlfGetCell, &xRef, 2, TempInt(1), &xCaller))
        throw exception("getHandleFull: error on call to xlfGetCell");
    XLOPER xStr;
    if (xlretSuccess != Excel(xlCoerce, &xStr, 2, &xRef, TempInt(xltypeStr))) 
        throw exception("getHandleFull: error on call to xlCoerce");
    char *ret = new char[strlen(handle) + xStr.val.str[0] + 2];
    strncpy(ret, handle, strlen(handle));
    ret[strlen(handle)] = '#';
    strncpy(ret + strlen(handle) + 1, &xStr.val.str[1], xStr.val.str[0]);
    ret[strlen(handle) + xStr.val.str[0] + 1] = 0;
    Excel(xlFree, 0, 1, &xStr);
    return ret;
}


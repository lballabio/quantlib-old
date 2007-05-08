
/*
 Copyright (C) 2005 Plamen Neykov
 Copyright (C) 2004, 2005, 2006, 2007 Eric Ehlers

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

#include <ohxl/Conversions/opertoscalar.hpp>
#include <oh/exception.hpp>
#include <ohxl/xloper.hpp>

namespace ObjectHandler {

    DLL_API void operToScalar(const OPER &xScalar, long &ret) {
        if (xScalar.xltype == xltypeNum)
            ret = xScalar.val.num;
        else {
            OPER xLong;
            Excel(xlCoerce, &xLong, 2, &xScalar, TempInt(xltypeInt));
            ret = xLong.val.w;
        }
    }

    DLL_API void operToScalar(const OPER &xScalar, double &ret) {
        if (xScalar.xltype == xltypeNum)
            ret = xScalar.val.num;
        else {
            OPER xDouble;
            Excel(xlCoerce, &xDouble, 2, &xScalar, TempInt(xltypeNum));
            ret = xDouble.val.num;
        }
    }

    DLL_API void operToScalar(const OPER &xScalar, bool &ret) {
        if (xScalar.xltype == xltypeBool)
            ret = xScalar.val.boolean != 0;
        else {
            OPER xBool;
            Excel(xlCoerce, &xBool, 2, &xScalar, TempInt(xltypeBool));
            ret = xBool.val.boolean != 0;
        }
    }

    DLL_API void operToScalar(const OPER &xScalar, std::string &ret) {

        const OPER *xString;

        if (xScalar.xltype == xltypeStr) {
            xString = &xScalar;
        } else {
            Xloper xTemp;
            Excel(xlCoerce, &xTemp, 2, &xScalar, TempInt(xltypeStr));
            xString = &xTemp;
        }

        //if (xString->val.str[0])
        //    ret.assign(xString->val.str + 1, xString->val.str[0]);
        // experimental workaround for apparent bug in Excel API
        // where the value for the string length wraps around the byte
        int stringLength = xString->val.str[0];
        if (stringLength < 0) stringLength += 256;
        if (stringLength)
            ret.assign(xString->val.str + 1, stringLength);
    }

    DLL_API void operToScalar(const OPER &xScalar, boost::any &ret) {

        if (xScalar.xltype & xltypeNil
        ||  xScalar.xltype & xltypeMissing
        ||  xScalar.xltype & xltypeErr && xScalar.val.err == xlerrNA) {
            ret = boost::any();
            return;
        }

        OH_REQUIRE(!(xScalar.xltype & xltypeErr), "input value has type=error");

        if (xScalar.xltype == xltypeNum)
            ret = xScalar.val.num;
        else if (xScalar.xltype == xltypeBool)
            ret = xScalar.val.boolean != 0;
        else if (xScalar.xltype == xltypeStr) {
            std::string value;
            operToScalar(xScalar, value);
            ret = value;
        } else {
            OH_FAIL("operToScalar: unexpected datatype: " << xScalar.xltype);
        }
    }

}


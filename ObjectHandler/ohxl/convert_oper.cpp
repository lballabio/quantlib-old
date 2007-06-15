
/*
 Copyright (C) 2007 Eric Ehlers

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

#include <ohxl/convert_oper.hpp>

namespace ObjectHandler {

    ConvertOper::ConvertOper(const OPER &xIn, const bool &decayVectorToScalar) {

        if (decayVectorToScalar && xIn.xltype & xltypeMulti) {
            if (xIn.val.array.rows == 1 && xIn.val.array.columns == 1) {
                oper_ = &xIn.val.array.lparray[0];
            } else {
                OH_FAIL("input value is vector or matrix, expected scalar");
            }
        } else {
            oper_ = &xIn;
        }        
    }

    ConvertOper::operator long() const {
        if (oper_->xltype == xltypeNum)
            return oper_->val.num;
        else {
            OPER xLong;
            Excel(xlCoerce, &xLong, 2, oper_, TempInt(xltypeInt));
            return xLong.val.w;
        }
    }

    ConvertOper::operator double() const {
        if (oper_->xltype == xltypeNum)
            return oper_->val.num;
        else {
            OPER xDouble;
            Excel(xlCoerce, &xDouble, 2, oper_, TempInt(xltypeNum));
            return xDouble.val.num;
        }
    }

    ConvertOper::operator bool() const {
        if (oper_->xltype == xltypeBool)
            return oper_->val.boolean != 0;
        else {
            OPER xBool;
            Excel(xlCoerce, &xBool, 2, oper_, TempInt(xltypeBool));
            return xBool.val.boolean != 0;
        }
    }

    ConvertOper::operator std::string() const {
        const OPER *xString;

        if (oper_->xltype == xltypeStr) {
            xString = oper_;
        } else {
            Xloper xTemp;
            Excel(xlCoerce, &xTemp, 2, oper_, TempInt(xltypeStr));
            xString = &xTemp;
        }
        return strConv(xString);
    }

    ConvertOper::operator ObjectHandler::Variant() const {

        if (missing()) {
            return ObjectHandler::Other(ObjectHandler::Null);
        } else if (error()) {
            return ObjectHandler::Other(ObjectHandler::Error);
        } else if (oper_->xltype == xltypeNum) {
            return oper_->val.num;
        } else if (oper_->xltype == xltypeBool) {
            return oper_->val.boolean != 0;
        } else if (oper_->xltype == xltypeStr) {
            return strConv(oper_);
        } else {
            OH_FAIL("ConvertOper: unexpected datatype: " << oper_->xltype);
        }
    }

    bool ConvertOper::missing() const {
        return (oper_->xltype & xltypeNil
        ||  oper_->xltype & xltypeMissing
        ||  oper_->xltype & xltypeErr && oper_->val.err == xlerrNA);
    }

    bool ConvertOper::error() const {
        return (oper_->xltype & xltypeErr && oper_->val.err != xlerrNA);
    }

    Type ConvertOper::type() const {
        if (missing()) {
            return Null;
        } else if (error()) {
            return Error;
        } else if (oper_->xltype & xltypeNum) {
            return Double;
        } else if (oper_->xltype & xltypeBool) {
            return Boolean;
        } else if (oper_->xltype & xltypeStr) {
            return String;
        } else if (oper_->xltype & xltypeMulti) {
            return Array;
        } else {
            OH_FAIL("unknown type");
        }
    }

    std::string ConvertOper::strConv(const OPER *xString) {

        //if (xString->val.str && xString->val.str[0])
        //    ret.assign(xString->val.str + 1, xString->val.str[0]);

        std::string ret;
        if (xString->val.str) {
            int stringLength = xString->val.str[0];
            // Experimental workaround for apparent bug in Excel API
            // where the value for the string length wraps around the byte
            if (stringLength < 0) stringLength += 256;
            if (stringLength)
                ret.assign(xString->val.str + 1, stringLength);
        }
        return ret;
    }

}

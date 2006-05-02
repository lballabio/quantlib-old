
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

#include <oh/objecthandler.hpp>
#include <oh/exception.hpp>
#include <ohxl/functioncall.hpp>
#include <ohxl/conversions.hpp>
#include <sstream>

namespace ObjHandler {

FunctionCall *FunctionCall::instance_ = 0;

    FunctionCall::FunctionCall() {
        if (instance_)
            throw Exception("Multiple attempts to initialize global FunctionCall object");
        instance_ = this;
        xCaller.xltype = 0;
        xReftext.xltype = 0;
        address_ = "";
    }

    FunctionCall::~FunctionCall() {
        instance_ = 0;
        if (xCaller.xltype) Excel(xlFree, 0, 1, &xCaller);
        if (xReftext.xltype) Excel(xlFree, 0, 1, &xReftext);
    }

    FunctionCall &FunctionCall::instance() {
        if (instance_)
            return *instance_;
        else
            throw Exception("Attempt to reference uninitialized FunctionCall object");
    }

    const XLOPER *FunctionCall::getCallerReference() {
        if (!xCaller.xltype) Excel(xlfCaller, &xCaller, 0);
        return &xCaller;
    }

    const XLOPER *FunctionCall::getCallerAddress() {
        if (!xReftext.xltype) Excel(xlfReftext, &xReftext, 1, getCallerReference());
        return &xReftext;
    }

    const std::string &FunctionCall::getAddressString() {
        if (address_.empty()) {
            XLOPER xAddress;
            try {
                Excel(xlfGetCell, &xAddress, 2, TempNum(1), getCallerReference());
                operToScalar(address_, xAddress);
            } catch (const std::exception &e) {
                Excel(xlFree, 0, 1, &xAddress);
                std::ostringstream err;
                err << "FunctionCall::getAddressString(): " << e.what();
                throw Exception(err.str().c_str());
            }
        }
        return address_;
    }

    void FunctionCall::clearCell() {
        
        // XLOPERs which might need freeing in the event of an exception

        XLOPER xOldName;
        XLOPER xValue;

        try {

            // exit if calling cell is #VALUE

            Excel(xlfGetCell, &xValue, 2, TempNum(5), getCallerReference());
            if (xValue.xltype & xltypeErr) return;
            Excel(xlFree, 0, 1, &xValue);

            // get name if any

            Excel(xlfGetDef, &xOldName, 1, getCallerAddress());

            // if name - delete associated object

            if (xOldName.xltype == xltypeStr) {
                std::string oldKey;
                operToScalar(oldKey, xOldName);
                ObjectHandler::instance().deleteKey(oldKey);
            }
            Excel(xlFree, 0, 1, &xOldName);

        } catch (const std::exception &e) {

            // free any memory that may have been allocated

            Excel(xlFree, 0, 2, &xOldName, &xValue);

            // propagate the exception

            std::ostringstream err;
            err << "FunctionCall::clearCell(): " << e.what();
            throw Exception(err.str().c_str());
        }

    }

}


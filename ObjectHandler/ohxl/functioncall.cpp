
/*
 Copyright (C) 2006, 2007 Eric Ehlers

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

#include <oh/exception.hpp>
#include <ohxl/repositoryxl.hpp>
#include <ohxl/functioncall.hpp>
#include <ohxl/xloper.hpp>
#include <ohxl/Conversions/opertoscalar.hpp>
#include <sstream>
#include <string>

namespace ObjectHandler {

    FunctionCall *FunctionCall::instance_ = 0;

    FunctionCall::FunctionCall(const std::string functionName) :
            functionName_(functionName), 
            callerDimensions_(CallerDimensions::Uninitialized),
            callerType_(CallerType::Uninitialized), 
            error_(false) {
        OH_REQUIRE(!instance_, "Multiple attempts to initialize global FunctionCall object");
        instance_ = this;
        Excel(xlfCaller, &xCaller_, 0);
        if (xCaller_->xltype == xltypeRef || xCaller_->xltype == xltypeSRef) {
            Excel(xlfReftext, &xReftext_, 1, &xCaller_);
            operToScalar(xReftext_(), refStr_);
        }
    }

    FunctionCall::~FunctionCall() {
        if (!error_ && !(xCaller_->xltype & xltypeErr)) 
            RepositoryXL::instance().clearError();
        instance_ = 0;
    }

    FunctionCall &FunctionCall::instance() {
        OH_REQUIRE(instance_, "Attempt to reference uninitialized FunctionCall object");
        return *instance_;
    }

    const XLOPER *FunctionCall::callerReference() {
        return &xCaller_;
    }

    const XLOPER *FunctionCall::callerAddress() {
        return &xReftext_;
    }

    const XLOPER *FunctionCall::callerArray() {
        if (!xMulti_->xltype) Excel(xlCoerce, &xMulti_, 2, &xCaller_, TempInt(xltypeMulti));
        return &xMulti_;
    }

    const std::string &FunctionCall::addressString() {
        if (address_.empty()) {
            Xloper xAddress;
            Excel(xlfGetCell, &xAddress, 2, TempNum(1), &xCaller_);
            operToScalar(xAddress(), address_);
        }
        return address_;
    }

    CallerDimensions::Type FunctionCall::callerDimensions() {
        // determine dimensions of calling range
        // at present we're only interested in row vs column
        // this could be extended to detect scalar / matrix
        if (callerDimensions_ == CallerDimensions::Uninitialized) {
            const XLOPER *xMulti = callerArray();
            if (xMulti->val.array.rows == 1 && xMulti->val.array.columns > 1)
                callerDimensions_ = CallerDimensions::Row;
            else
                callerDimensions_ = CallerDimensions::Column;
        }
        return callerDimensions_;
    }

    CallerType::Type FunctionCall::callerType() {
        if (callerType_ == CallerType::Uninitialized) {
            if (xCaller_->xltype == xltypeRef || xCaller_->xltype == xltypeSRef)
                callerType_ = CallerType::Cell;
            //else if (xCaller->xltype == xltypeMulti)
            //    callerType_ = CallerType::Menu;
            //else if (xCaller->xltype & xltypeErr)
            //    callerType_ = CallerType::VBA;
            else
                callerType_ = CallerType::Unknown;
        }
        return callerType_;
    }

}


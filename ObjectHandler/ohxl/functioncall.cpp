/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2006, 2007, 2008 Eric Ehlers

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
#include <ohxl/convert_oper.hpp>
#include <sstream>
#include <string>

namespace ObjectHandler {

    FunctionCall *FunctionCall::instance_ = 0;

    FunctionCall::FunctionCall(const std::string functionName) :
            functionName_(functionName), 
            callerDimensions_(CallerDimensions::Uninitialized),
            error_(false) {
        OH_REQUIRE(!instance_, "Multiple attempts to initialize global FunctionCall object");
        instance_ = this;

        Excel(xlfCaller, &xCaller_, 0);
        if (xCaller_->xltype == xltypeRef || xCaller_->xltype == xltypeSRef) {
            Excel(xlfReftext, &xReftext_, 1, &xCaller_);
            refStr_ = ConvertOper(xReftext_());
            callerType_ = CallerType::Cell;
        } else if (xCaller_->xltype & xltypeErr) {
            callerType_ = CallerType::VBA;
        } else if (xCaller_->xltype == xltypeMulti) {
            callerType_ = CallerType::Menu;
        } else {
            callerType_ = CallerType::Unknown;
        }
    }

    FunctionCall::~FunctionCall() {
        if (!error_) {
            if (callerType_ == CallerType::Cell) {
                RepositoryXL::instance().clearError();
            } else if (callerType_ == CallerType::VBA || callerType_ == CallerType::Menu) {
                RepositoryXL::instance().clearVbaError();
            }
        }
        instance_ = 0;
    }

    FunctionCall &FunctionCall::instance() {
        OH_REQUIRE(instance_, "Attempt to reference uninitialized FunctionCall object");
        return *instance_;
    }

    const XLOPER *FunctionCall::callerArray() {
        if (!xMulti_->xltype) Excel(xlCoerce, &xMulti_, 2, &xCaller_, TempInt(xltypeMulti));
        return &xMulti_;
    }

    const std::string &FunctionCall::addressString() {
        if (address_.empty()) {
            Xloper xAddress;
            Excel(xlfGetCell, &xAddress, 2, TempNum(1), &xCaller_);
            address_ = ConvertOper(xAddress());
        }
        return address_;
    }

    CallerDimensions::Type FunctionCall::callerDimensions() {
        // Determine dimensions of calling range.
        // At present we're only interested in row vs. column,
        // this could be extended to detect scalar / matrix.
        if (callerDimensions_ == CallerDimensions::Uninitialized) {
            const XLOPER *xMulti = callerArray();
            if (xMulti->val.array.rows == 1 && xMulti->val.array.columns > 1)
                callerDimensions_ = CallerDimensions::Row;
            else
                callerDimensions_ = CallerDimensions::Column;
        }
        return callerDimensions_;
    }

    // Code to determine whether we've been called from the Excel function wizard.
    // This test is very expensive in terms of CPU.
    //
    // This code is based on code from MSDN and has inherited some bugs from the
    // MSDN version:
    // 1) The Function Wizard dialog is assumed to be any window of class
    //    "bosa_sdm_XL" but that class can also indicate the "Edit->Replace" dialog
    // 2) The main excel parent window is identified only by the LOWORD half of its
    //    DWORD handle
    //
    // Consequently there is the risk that this function will return false positives
    // i.e. we'll conclude that we've been called from the Function Wizard when in
    // fact we've been called while
    // 1) the "Edit->Replace" dialog is active
    // 2) some other Excel session is displaying the Function Wizard or
    //    "Edit->Replace" dialog

    typedef struct {
        bool bFuncWiz;
        short hwndXLMain;
    } EnumStruct;

    // The class name of the window for the Function Wizard dialog.  Various
    // versions of Excel may suffix this string with additional characters.
    #define WIZ_ID_BUF "bosa_sdm_XL"
    #define WIZ_ID_BUF_LEN 12

    // Called by EnumWindows (below) for each open window
    bool CALLBACK EnumProc(HWND hwnd, EnumStruct *pEnum) {

        // Retrieve the class name of the current window.  We only want to
        // compare the resulting string with the "bosa_sdm_XL" value so we set
        // the buffer size such that any trailing characters are truncated.

        char class_name[WIZ_ID_BUF_LEN];
        GetClassName(hwnd, class_name, WIZ_ID_BUF_LEN);

        if (stricmp(class_name, WIZ_ID_BUF) == 0) {

            // Apparently this window is the Excel Function Wizard dialog.
            // Now check whether the ID of this window's parent matches that of
            // our main Excel window as returned by xlGetHwnd (below).

            if (LOWORD((DWORD) GetParent(hwnd)) == pEnum->hwndXLMain) {
                pEnum->bFuncWiz = TRUE; // We've (probably) been called from the wizard
                return false;           // Tell EnumWindows to stop
            }

        }

        return true;                    // Tell EnumWindows to continue
    }

    short int getWinID() {

        // Retrieve the LOWORD half of the DWORD handle
        // of the main Excel window.

        XLOPER xHwndMain;
        Excel(xlGetHwnd, &xHwndMain, 0);

        return xHwndMain.val.w;
    }

    bool FunctionCall::calledByFunctionWizard() {

        // ID of the main Excel window.  This value is retrieved once
        // for this running instance of the Addin.

        static short int winID = getWinID();

        // Call EnumWindows which iteratively calls EnumProc which sets
        // enm.bFuncWiz if the Excel Function Wizard dialog is detected.

        EnumStruct enm = { false, winID };
        EnumWindows((WNDENUMPROC)EnumProc, (LPARAM)&enm);

        return enm.bFuncWiz;
    }

    std::string FunctionCall::callerName() const {
        if (callerType_ == CallerType::Cell) {
            Xloper xName;
            Excel(xlfGetDef, &xName, 1, FunctionCall::instance().callerAddress());
            if (xName->xltype == xltypeStr)
                return ConvertOper(xName());
            return "";
        } else {
            return "VBA";
        }
    }

}


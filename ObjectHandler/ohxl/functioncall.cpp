
/*
 Copyright (C) 2006, 2007 Eric Ehlers

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

#include <ohxl/objecthandlerxl.hpp>
#include <oh/exception.hpp>
#include <ohxl/functioncall.hpp>
#include <ohxl/Conversions/opertoscalar.hpp>
#include <sstream>
#include <string>

namespace ObjHandler {

    FunctionCall *FunctionCall::instance_ = 0;

    FunctionCall::FunctionCall(const std::string functionName) 
        : functionName_(functionName), callerDimensions_(Uninitialized),
          callerType_(Uninitialized2), error_(false) {
        if (instance_)
            throw Exception("Multiple attempts to initialize global FunctionCall object");
        instance_ = this;
        xCaller_.xltype = 0;
        xReftext_.xltype = 0;
        xMulti_.xltype = 0;
        address_ = "";
    }

    FunctionCall::~FunctionCall() {
        if (!error_)
            ObjectHandlerXL::instance().clearError();
        instance_ = 0;
        if (xCaller_.xltype) Excel(xlFree, 0, 1, &xCaller_);
        if (xReftext_.xltype) Excel(xlFree, 0, 1, &xReftext_);
        if (xMulti_.xltype) Excel(xlFree, 0, 1, &xMulti_);
    }

    FunctionCall &FunctionCall::instance() {
        if (instance_)
            return *instance_;
        else
            throw Exception("Attempt to reference uninitialized FunctionCall object");
    }

    const XLOPER *FunctionCall::getCallerReference() {
        if (!xCaller_.xltype) Excel(xlfCaller, &xCaller_, 0);
        return &xCaller_;
    }

    const XLOPER *FunctionCall::getCallerAddress() {
        if (!xReftext_.xltype) Excel(xlfReftext, &xReftext_, 1, getCallerReference());
        return &xReftext_;
    }

    const XLOPER *FunctionCall::getCallerArray() {
        if (!xMulti_.xltype) Excel(xlCoerce, &xMulti_, 2, getCallerReference(), TempInt(xltypeMulti));
        return &xMulti_;
    }

    const std::string &FunctionCall::getAddressString() {
        // addins call this function from within their catch() 
        // so this function must not throw
        if (address_.empty()) {
            XLOPER xAddress;
            xAddress.xltype = 0;
            try {
                Excel(xlfGetCell, &xAddress, 2, TempNum(1), getCallerReference());
                operToScalar(xAddress, address_);
                Excel(xlFree, 0, 1, &xAddress);
            } catch (...) {
                if (xAddress.xltype) 
                    Excel(xlFree, 0, 1, &xAddress);
                address_ = "caller address unknown";
            }
        }
        return address_;
    }

    const std::string &FunctionCall::getFunctionName() {
        return functionName_;
    }

    const std::string &FunctionCall::getFormula() {
        if (formula_.empty()) {
            XLOPER xFormula;
            try {
                Excel(xlfGetFormula, &xFormula, 1, getCallerReference());
                operToScalar(xFormula, formula_);
                Excel(xlFree, 0, 1, &xFormula);
            } catch (const std::exception &e) {
                Excel(xlFree, 0, 1, &xFormula);
                std::ostringstream err;
                err << "FunctionCall::getFormula(): " << e.what();
                throw Exception(err.str());
            }
        }
        return formula_;
    }

    CallerDimensions FunctionCall::getCallerDimensions() {
        // determine dimensions of calling range
        // at present we're only interested in row vs column
        // this could be extended to detect scalar / matrix
        if (callerDimensions_ == Uninitialized) {
            const XLOPER *xMulti = getCallerArray();
            if (xMulti->val.array.rows == 1 && xMulti->val.array.columns > 1)
                callerDimensions_ = Row;
            else
                callerDimensions_ = Column;
        }
        return callerDimensions_;
    }

    CallerType FunctionCall::getCallerType() {
        if (callerType_ == Uninitialized2) {
            const XLOPER *xCaller = getCallerReference();
            if (xCaller->xltype == xltypeRef || xCaller->xltype == xltypeSRef)
                callerType_ = Cell;
            //else if (xCaller->xltype == xltypeMulti)
            //    callerType_ = Menu;
            //else if (xCaller->xltype == xltypeErr)
            //    callerType_ = VBA;
            else
                callerType_ = Unknown;
        }
        return callerType_;
    }

    // Code to determine whether we've been called from the Excel function wizard.
    //
    // This code is called by every function in the Addin and some attempt has been 
    // made to optimize performance e.g. caching variables whose values don't change.
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

    bool FunctionCall::IsCalledByFuncWiz() {

        // ID of the main Excel window.  This value is retrieved once
        // for this running instance of the Addin.

        static short int winID = getWinID();

        // Call EnumWindows which iteratively calls EnumProc which sets
        // enm.bFuncWiz if the Excel Function Wizard dialog is detected.

        EnumStruct enm = { false, winID };
        EnumWindows((WNDENUMPROC)EnumProc, (LPARAM)&enm);

        return enm.bFuncWiz;
    }

}

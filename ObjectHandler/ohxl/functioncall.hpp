
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

/*! \file
    \brief Class FunctionCall - Handle state related to the function in progress.
*/

#ifndef ohxl_functioncall_hpp
#define ohxl_functioncall_hpp

#include <xlsdk/xlsdkdefines.hpp>
#include <ohxl/ohxldefines.hpp>
#include <ohxl/callingrange.hpp>
#include <ohxl/xloper.hpp>
#include <boost/shared_ptr.hpp>

namespace ObjectHandler {

    struct CallerDimensions {
        enum Type { Uninitialized, Row, Column /*, Matrix, Scalar */ };
    };

    struct CallerType {
        enum Type { Uninitialized, Cell, /* Menu, VBA, */ Unknown };
    };

    //! Singleton encapsulating state relating to Excel function call.
    /*! An instance of this object is instantiated on the stack when the
        function is invoked such that the object goes out of scope when
        the function exits.  This class allows global access to
        function-specific state e.g. the return value of xlfGetCaller.
    */
    class DLL_API FunctionCall {
    public:
        //! \name Structors and static members
        //@{
        //! Constructor - Store the name of the function in progress.
        FunctionCall(const std::string functionName);
        //! Destructor - Clean up any resources that may have been acquired.
        ~FunctionCall();
        //! A reference to the global FunctionCall Singleton.
        /*! Clients of this class access it with a call to
                FunctionCall::instance()
        */
        static FunctionCall &instance();
        //@}

        //! Reference to the caller as returned by Excel function xlfCaller.
        const XLOPER *callerReference();
        //! Address of the caller as returned by Excel function xlfReftext.
        const XLOPER *callerAddress();
        //! Calling range, coerced to type xltypeMulti.
        /*! If the caller is not an Excel range then this function returns
            an XLOPER with xltype initialized to zero.
        */
        const XLOPER *callerArray();
        //! Address of the caller as returned by Excel function xlfGetCell.
        const std::string &addressString();
        //! The function name that was passed to the constructor.
        const std::string &functionName() { return functionName_; }
        //! Text reference of the caller as returned by  xlfReftext, converted to a std::string
        const std::string &refStr() { return refStr_; }
        //! The dimensions of the calling range.
        /*! Presently this function only differentiates between Row and Column,
            this could be extended to distinguish Scalar (currently treated as Column)
            and Matrix.
        */
        CallerDimensions::Type callerDimensions();
        //! The type of the caller.
        /*! Presently this function only differentiates between Cell and Other,
            this could be extended to distinguish calls from Excel VBA, and from Excel
            menu items.
        */
        CallerType::Type callerType();
        //! Called to indicate that an error occurred during execution of the function.
        void setError() { error_ = true; }
    private:
        static FunctionCall *instance_;
        std::string functionName_;
        std::string address_;
        std::string refStr_;
        Xloper xCaller_;
        Xloper xReftext_;
        Xloper xMulti_;
        CallerDimensions::Type callerDimensions_;
        CallerType::Type callerType_;
        bool error_;
    };

}

#endif


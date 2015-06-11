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

/*! \file
    \brief Class FunctionCall - Handle state related to the function in progress
*/

#ifndef ohxl_functioncall_hpp
#define ohxl_functioncall_hpp

#include <xlsdk/xlsdkdefines.hpp>
#include <ohxl/ohxldefines.hpp>
#include <ohxl/xloper.hpp>
#include <boost/shared_ptr.hpp>

namespace ObjectHandler {

    //! The dimensions of the calling range.
    struct CallerDimensions {
        enum Type { Uninitialized, Row, Column /*, Matrix, Scalar */ };
    };

    //! The environment which called the function that is currently running.
    /*! Classes FunctionCall and RepositoryXL presently treat VBA and Menu
        the same, which is a valid assumption given existing usage.
        The assumption would not hold in the case where this XLL is invoked
        from an Excel menu owned by a non-VBA process e.g. an XLL in C/C++.
    */
    struct CallerType {
        enum Type { Cell, VBA, Menu, Unknown };
    };

    //! Singleton encapsulating state relating to Excel function call.
    /*! An instance of this object is instantiated on the stack when the
        function is invoked such that the object goes out of scope when
        the function exits.  This class allows global access to
        function-specific state e.g. a reference to the range from which
        the active function was called.
    */
    class DLL_API FunctionCall {
    public:
        //! \name Structors and static members
        //@{
        //! Constructor - Store the name of the function in progress.
        /*! The constructor calls xlfCaller and xlfReftext as the results of
            these functions are always required in the destructor.
        */
        FunctionCall(const std::string functionName);
        //! Destructor - Clean up whatever resources were acquired.
        /*! If the function has completed successfully then any error message
            that may be associated with the calling cell is cleared.
        */
        ~FunctionCall();
        //! A reference to the global FunctionCall Singleton.
        /*! Clients of this class access it with a call to
            \code
                FunctionCall::instance()
            \endcode
        */
        static FunctionCall &instance();
        //@}

        //! \name Excel API Wrappers
        //@{
        //! Reference to the caller as returned by Excel function xlfCaller.
        const XLOPER *callerReference() { return &xCaller_; }
        //! Address of the caller as returned by Excel function xlfReftext.
        const XLOPER *callerAddress() { return &xReftext_; }
        //! Calling range, coerced to type xltypeMulti.
        /*! If the caller is not an Excel range then this function returns
            an XLOPER with xltype initialized to zero.
        */
        const XLOPER *callerArray();
        //! Whether or not this function has been called from the Function Wizard.
        /*! This test is expensive.
        */
        bool calledByFunctionWizard();
        //! Name if any associated with calling range.
        std::string callerName() const;
        //@}

        //! \name Conversions
        //@{
        //! Address of the caller from xlfGetCell, converted to a string.
        const std::string &addressString();
        //! Text reference of the caller as returned by  xlfReftext, converted to a string
        const std::string &refStr() { return refStr_; }
        //@}

        //! \name Inspectors
        //@{
        //! The function name that was passed to the constructor.
        const std::string &functionName() { return functionName_; }
        //! The dimensions of the calling range.
        /*! Presently this function only differentiates between Row and Column,
            this could be extended to distinguish Scalar (currently treated as Column)
            and Matrix.
        */
        CallerDimensions::Type callerDimensions();
        //! The type of the caller.
        CallerType::Type callerType() { return callerType_; }
        //@}

        //! \name Error Messages
        //@{
        //! Called to indicate that an error occurred during execution of the function.
        void setError() { error_ = true; }
        //@}

    private:
        static FunctionCall *instance_;
#pragma warning(push)
#pragma warning(disable: 4251)
        std::string functionName_;
        std::string address_;
        std::string refStr_;
#pragma warning(pop)
        Xloper xCaller_;
        Xloper xReftext_;
        Xloper xMulti_;
        CallerDimensions::Type callerDimensions_;
        CallerType::Type callerType_;
        bool error_;
    };

}

#endif


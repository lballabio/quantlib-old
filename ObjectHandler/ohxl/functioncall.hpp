
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
    \brief FunctionCall class
*/

#ifndef ohxl_functioncall_hpp
#define ohxl_functioncall_hpp

#include <xlsdk/xlsdkdefines.hpp>
#include <ohxl/ohxldefines.hpp>
#include <ohxl/callingrange.hpp>
#include <ohxl/xloper.hpp>
#include <boost/shared_ptr.hpp>

//! ObjectHandler
/*! name space for the Object Handler
*/
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
        FunctionCall(const std::string functionName);
        ~FunctionCall();
        static FunctionCall &instance();
        const XLOPER *callerReference();
        const XLOPER *callerAddress();
        const XLOPER *callerArray();
        const std::string &addressString();
        const std::string &functionName() { return functionName_; }
        const std::string &refStr() { return refStr_; }
        CallerDimensions::Type callerDimensions();
        CallerType::Type callerType();
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

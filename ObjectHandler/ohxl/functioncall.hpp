
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

/*! \file
    \brief FunctionCall class
*/

#ifndef ohxl_functioncall_hpp
#define ohxl_functioncall_hpp

#include <xlsdk/xlsdkdefines.hpp>

//! ObjHandler
/*! name space for the Object Handler
*/
namespace ObjHandler {

    enum CallerDimensions { Uninitialized, Row, Column, /* Matrix, Scalar */ };
    enum CallerType { Uninitialized2, Cell, /* Menu, VBA, */ Unknown };

    //! Singleton encapsulating state relating to Excel function call.
    /*! An instance of this object is instantiated on the heap when the
        function is invoked such that the object goes out of scope when
        the function exits.  This class allows global access to
        function-specific state e.g. the return value of xlfGetCaller.
    */
    class DLL_API FunctionCall {
    public:
        FunctionCall(const std::string functionName);
        ~FunctionCall();
        static FunctionCall &instance();
        const XLOPER *getCallerReference();
        const XLOPER *getCallerAddress();
        const XLOPER *getCallerArray();
        const std::string &getAddressString();
        const std::string &getFunctionName();
        const std::string &getFormula();
        //bool outerFunction();
        CallerDimensions getCallerDimensions();
        CallerType getCallerType();
        bool IsCalledByFuncWiz();
        void setError() {
            error_ = true;
        }
    private:
        static FunctionCall *instance_;
        XLOPER xCaller_;
        XLOPER xReftext_;
        XLOPER xMulti_;
        std::string address_;
        std::string formula_;
        std::string functionName_;
        CallerDimensions callerDimensions_;
        CallerType callerType_;
        bool error_;
    };

}

#endif

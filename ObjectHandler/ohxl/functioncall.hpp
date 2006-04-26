
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

#ifndef oh_functioncall_hpp
#define oh_functioncall_hpp

#include <xlsdk/xlsdkdefines.hpp>

//! ObjHandler
/*! name space for the Object Handler
*/
namespace ObjHandler {

    //! Singleton encapsulating state relating to Excel function call.
    /*! An instance of this object is instantiated on the heap when the
        function is invoked such that the object goes out of scope when
        the function exits.  This class allows global access to
        function-specific state e.g. the return value of xlfGetCaller.
    */
    class FunctionCall {
    public:
        FunctionCall();
        ~FunctionCall();
        static FunctionCall &instance();
        void clearCell();
        const XLOPER *getCallerReference();
        const XLOPER *getCallerAddress();
    private:
        static FunctionCall *instance_;
        XLOPER xCaller;
        XLOPER xReftext;
        bool initialized_;
        void initialize();
    };

}

#endif


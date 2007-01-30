
/*
 Copyright (C) 2005, 2006 Eric Ehlers

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

#ifndef ohxl_conversions_opertoscalar_hpp
#define ohxl_conversions_opertoscalar_hpp

#include <oh/objhandlerdefines.hpp>
#include <xlsdk/xlsdkdefines.hpp>
#include <string>
#include <boost/any.hpp>

namespace ObjHandler {

    DLL_API void operToScalar(const OPER &xScalar, long        &ret);
    DLL_API void operToScalar(const OPER &xScalar, double      &ret);
    DLL_API void operToScalar(const OPER &xScalar, bool        &ret);
    DLL_API void operToScalar(const OPER &xScalar, std::string &ret);
    DLL_API void operToScalar(const OPER &xScalar, boost::any  &ret);

    template <class T>
    T operToScalar(const OPER &xScalar, 
                   const T &defaultValue, 
                   const std::string paramName) {

        if ((xScalar.xltype & xltypeNil)
        ||  (xScalar.xltype & xltypeMissing)
        || ((xScalar.xltype & xltypeErr) && (xScalar.val.err == xlerrNA)))
            return defaultValue;
        OH_REQUIRE(!(xScalar.xltype & xltypeErr), 
            paramName << " - input value has type=error");

        try {
            T returnValue;
            operToScalar(xScalar, returnValue);
            return returnValue;
        } catch(const std::exception &e) {
            OH_FAIL("unable to convert parameter '" << paramName 
                << "' to type " << typeid(T).name()
                << " - " << e.what());
        }
    }

}

#endif


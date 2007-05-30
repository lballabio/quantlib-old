
/*
 Copyright (C) 2005, 2006, 2007 Eric Ehlers

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
    \brief Conversion function operToScalar - convert an Excel OPER to a scalar value.
*/

#ifndef ohxl_conversions_opertoscalar_hpp
#define ohxl_conversions_opertoscalar_hpp

#include <oh/ohdefines.hpp>
#include <xlsdk/xlsdkdefines.hpp>
#include <string>
#include <boost/any.hpp>

namespace ObjectHandler {

    //@{
    //! Convert an OPER to a scalar value.
    /*! These functions are usually wrapped in the callOperToScalar template.
    */
    DLL_API void operToScalar(const OPER &xScalar, long &ret);
    DLL_API void operToScalar(const OPER &xScalar, double &ret);
    DLL_API void operToScalar(const OPER &xScalar, bool &ret);
    DLL_API void operToScalar(const OPER &xScalar, std::string &ret);
    DLL_API void operToScalar(const OPER &xScalar, boost::any &ret);
    //@}

    //! Template function callOperToScalar - wrapper for operToScalar.
    /*! This template implements error handling common to all overrides
        of the operToScalar function.
    */
    template <class T>
    T callOperToScalar(const OPER &xIn, 
                       const std::string &parameterName) {

        OH_REQUIRE(!(xIn.xltype & xltypeErr), 
                   "input value '" << parameterName << "' has type=error");

        const OPER *xScalar;
        if (xIn.xltype & xltypeMulti) {
            if (xIn.val.array.rows == 1 && xIn.val.array.rows == 1) {
                xScalar = &xIn.val.array.lparray[0];
            } else {
                OH_FAIL("input value '" << parameterName << "' is vector or matrix, expected scalar");
            }
        } else {
            xScalar = &xIn;
        }

        try {
            T returnValue;
            operToScalar(*xScalar, returnValue);
            return returnValue;
        } catch(const std::exception &e) {
            OH_FAIL("unable to convert parameter '" << parameterName 
                << "' to type " << typeid(T).name()
                << " - " << e.what());
        }

    }

    //! Template function callOperToScalar - wrapper for operToScalar.
    /*! Special processing for the case where a default value is supplied.
    */
    template <class T>
    T callOperToScalar(const OPER &xScalar, 
            const std::string &parameterName,
            const T &defaultValue) {

        if (xScalar.xltype & xltypeNil
        ||  xScalar.xltype & xltypeMissing
        ||  xScalar.xltype & xltypeErr && xScalar.val.err == xlerrNA)
            return defaultValue;

        return callOperToScalar<T>(xScalar, parameterName);

    }

}

#endif


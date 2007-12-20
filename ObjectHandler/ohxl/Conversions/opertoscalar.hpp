/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

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
    \brief Conversion function operToScalar - convert an Excel OPER to a scalar value
*/

#ifndef oh_conversions_opertoscalar_hpp
#define oh_conversions_opertoscalar_hpp

#include <oh/ohdefines.hpp>
#include <xlsdk/xlsdkdefines.hpp>
#include <oh/Conversions/varianttoscalar.hpp>
#include <ohxl/convert_oper.hpp>
#include <string>

namespace ObjectHandler {

    //! Helper template wrapper for VariantToScalar functor.
    /*! This template supplies ConvertOper as the V template argument
        for VariantToScalar, simplifying syntax in client applications.
    */
    template <class T>
    T operToScalar(const OPER &oper) {

        return VariantToScalar<ConvertOper, T>()(ConvertOper(oper));
    }

    //! Helper wrapper for variantToScalar template.
    template <class T>
    T operToScalar(
        const OPER &oper,
        const std::string &parameterName) {

        return variantToScalar<ConvertOper, T>(
            ConvertOper(oper), parameterName);
    }

    //! Helper wrapper for variantToScalar template.
    template <class T>
    T operToScalar(
        const OPER &oper,
        const std::string &parameterName,
        const T &defaultValue) {

        return variantToScalar<ConvertOper, T>(
            ConvertOper(oper), parameterName, defaultValue);
    }

    //! Helper wrapper for variantToScalar template.
    template <class T>
    T operToScalar(
        const OPER &oper,
        const std::string &parameterName,
        const T &defaultValue,
        const T &errorValue) {

        return variantToScalar<ConvertOper, T>(
            ConvertOper(oper), parameterName, defaultValue, errorValue);
    }

}

#endif


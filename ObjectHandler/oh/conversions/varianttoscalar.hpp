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
    \brief Templates to convert a variant to a scalar value
*/

#ifndef oh_conversions_varianttoscalar_hpp
#define oh_conversions_varianttoscalar_hpp

#include <oh/ohdefines.hpp>
#include <oh/exception.hpp>
#include <oh/repository.hpp>
#include <string>
#include <boost/shared_ptr.hpp>

namespace ObjectHandler {

    //! Convert a variant to a scalar
    /*! This template functor is the crux of all data conversions of variant
        types for function input values and boost::serialization.  This template
        allows a single conversion algorithm to be used for both
        - Input arguments to an Addin function (using a platform-specific variant
          type such as ConvertOper for Excel)
        - Deserialization of class ValueObject (using C++ variant type Variant)

        \param T - The type to which the variant is converted.
        \param V - A functor supporting conversion from the variant to T.  This
        may be Variant, ConvertOper, or some user defined class supporting the
        necessary interface.

        Conversion algorithms in client applications typically customize this
        template in one of two ways:
        - Partial specialization on T - override the template for conversion
          to datatypes specific to the client application
        - Supply a client specific implementation of the V functor
    */
    template <class V, class T>
    struct VariantToScalar {
        T operator()(const V &variant) { return variant; }
    };

    //! Convert a Variant to a string and retrieve the corresponding Object.
    /*! The Object reference is downcast as necessary.
    */
    template <class V, class LibraryClass, class ObjectClass>
    struct VariantToScalarObject {
        boost::shared_ptr<LibraryClass> operator()(const V &variant) {
            OH_GET_REFERENCE(obj, variant, ObjectClass, LibraryClass)
            return obj;
        }
    };

    //! Wrapper to provide error handling for the VariantToScalar functor.
    template <class V, class T>
    T variantToScalar(const V &variant, 
        const std::string &parameterName) {

        OH_REQUIRE(!variant.error(), 
            "input value '" << parameterName << "' has type=error");

        try {
            return VariantToScalar<V, T>()(variant);
        } catch(const std::exception &e) {
            OH_FAIL("unable to convert parameter '" << parameterName 
                << "' to type '" << typeid(T).name() << "' - " << e.what());
        }
    }

    //! Helper template to support defaults for missing values.
    template <class V, class T>
    T variantToScalar(const V &variant, 
        const std::string &parameterName,
        const T &defaultValue) {

        if (variant.missing()) return defaultValue;
        return variantToScalar<V, T>(variant, parameterName);
    }

    //! Helper template to replace erroneous input with specified value.
    template <class V, class T>
    T variantToScalar(const V &variant, 
        const std::string &parameterName,
        const T &defaultValue,
        const T &errorValue) {

        if (variant.error()) return errorValue;
        return variantToScalar<V, T>(variant, parameterName, defaultValue);
    }

}

#endif


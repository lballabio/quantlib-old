
/*
 Copyright (C) 2007 Eric Ehlers

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and develohVariants - http://quantlib.org/

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
    \brief Helper templates simplifying syntax for clients of class Variant.
*/

#ifndef oh_conversions_ohvarianttoscalar_hpp
#define oh_conversions_ohvarianttoscalar_hpp

#include <oh/ohdefines.hpp>
#include <oh/Conversions/varianttoscalar.hpp>

namespace ObjectHandler {

    //! Helper template wrapper for VariantToScalar functor.
    /*! This template supplies Variant as the V template argument
        for VariantToScalar, simplifying syntax in client applications.
    */
    template <class T>
    T ohVariantToScalar(const Variant &variant) {

        return VariantToScalar<Variant, T>()(variant);
    }

    //! Helper wrapper for variantToScalar template.
    template <class T>
    T ohVariantToScalar(
        const Variant &variant,
        const std::string &parameterName) {

        return variantToScalar<Variant, T>(
            variant, parameterName);
    }

    //! Helper wrapper for variantToScalar template.
    template <class T>
    T ohVariantToScalar(
        const Variant &variant,
        const std::string &parameterName,
        const T &defaultValue) {

        return variantToScalar<Variant, T>(
            variant, parameterName, defaultValue);
    }

    //! Helper wrapper for variantToScalar template.
    template <class T>
    T ohVariantToScalar(
        const Variant &variant,
        const std::string &parameterName,
        const T &defaultValue,
        const T &errorValue) {

        return variantToScalar<Variant, T>(
            variant, parameterName, defaultValue, errorValue);
    }

    //! Helper template to call ohVariantToScalar on a vector.
    template <class T>
    std::vector<T> ohVariantToVector(
        const std::vector<Variant> &variant,
        const std::string &parameterName) {

        try {
            std::vector<T> ret;
            for (std::vector<Variant>::const_iterator i = variant.begin(); i != variant.end(); ++i) {
                ret.push_back(ohVariantToScalar<T>(*i));                
            }
            return ret;
        } catch (const std::exception &e) {
            OH_FAIL("ohVariantToVector: unable to convert parameter '" << parameterName 
                << "' to type '" << typeid(T).name() << "' - " << e.what());
        }
    }

    //! Helper template to call VariantToScalarObject on a vector.
    template <class LibraryClass, class ObjectClass>
    std::vector<boost::shared_ptr<LibraryClass> >
    ohVariantToObjectVector(
        const std::vector<Variant> &variant,
        const std::string &parameterName) {

        try {
            std::vector<boost::shared_ptr<LibraryClass> > ret;
            for (std::vector<Variant>::const_iterator i = variant.begin(); i != variant.end(); ++i) {
                ret.push_back(VariantToScalarObject<Variant, LibraryClass, ObjectClass>()(*i));
            }
            return ret;
        } catch (const std::exception &e) {
            OH_FAIL("ohVariantToObjectVector: unable to convert parameter '" << parameterName 
                << "' to type '" << typeid(boost::shared_ptr<LibraryClass>).name() << "' - " << e.what());
        }
    }

    //! Helper template to call ohVariantToScalar on a matrix.
    template <class T>
    std::vector<std::vector<T> > ohVariantToMatrix(
        const std::vector<std::vector<Variant> > &variant,
        const std::string &parameterName) {

        try {
            std::vector<std::vector<T> > ret;
            for (std::vector<std::vector<Variant> >::const_iterator i = variant.begin();
                i != variant.end(); ++i) {
                std::vector<T> rowOut;
                for (std::vector<Variant>::const_iterator j = i->begin(); j != i->end(); ++j) {
                    rowOut.push_back(ohVariantToScalar<T>(*j));                
                }
                ret.push_back(rowOut);                
            }
            return ret;
        } catch (const std::exception &e) {
            OH_FAIL("ohVariantToMatrix: unable to convert parameter '" << parameterName 
                << "' to type '" << typeid(T).name() << "' - " << e.what());
        }
    }

}

#endif


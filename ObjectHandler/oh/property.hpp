/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2007 Eric Ehlers
 Copyright (C) 2008 Luigi Ballabio
 Copyright (C) 2008 Plamen Neykov

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
    \brief Class Variant - A class to represent any of a selection of native C++ datatypes
*/

#ifndef oh_property_hpp
#define oh_property_hpp

#include <oh/ohdefines.hpp>
#include <oh/exception.hpp>
#include <string>
#include <vector>
#include <boost/variant.hpp>
#include <boost/serialization/access.hpp>
#include <boost/serialization/nvp.hpp>
#include <boost/serialization/variant.hpp>
#include <oh/conversions/convert2.hpp>

namespace ObjectHandler {

    struct empty_property_tag {
        template<class Archive>
        void serialize(Archive &ar, const unsigned int) {}
    };

    typedef boost::make_recursive_variant<empty_property_tag, bool, std::string, long, double,
            std::vector<boost::recursive_variant_> >::type property_base;

    class property_t : public property_base {
        friend class boost::serialization::access;
    public:
        typedef std::vector<property_base> vector;

        property_t() {}

        property_t(const property_t& t) : property_base(static_cast<const property_base&>(t)) {}

        property_t(const char* s) : property_base(std::string(s)) {}

        template<typename T>
        property_t(const T& t) : property_base(t) {}

        template<typename T>
        property_t(const std::vector<T>& vct) {
            vector row;
            for(typename std::vector<T>::const_iterator i = vct.begin(); i != vct.end(); ++i)
                row.push_back(*i);

            property_base::operator= <vector>(row);
        }

        template<typename T>
        property_t(const std::vector<std::vector<T> >& mtx) {
            vector matrix;
            for(typename std::vector<std::vector<T> >::const_iterator i = mtx.begin(); i != mtx.end(); ++i) {
                vector row;
                for(typename std::vector<T>::const_iterator j = i->begin(); j != i->end(); ++j)
                    row.push_back(*j);
                matrix.push_back(row);
            }
            property_base::operator= <vector>(matrix);
        }

        property_t& operator=(const property_t& rhs) {
            property_base::operator=(static_cast<const property_base&>(rhs));
            return *this;
        }

        template<typename T>
        operator T() const { return boost::get<T>(*this); }
        
        operator property_t() const { return *this; }
        
        bool missing() const { return which() == 0; }

    protected:
        template<class Archive>
        void serialize(Archive &ar, const unsigned int ver) {
            boost::serialization::serialize(ar, *static_cast<property_base*>(this), ver);
        }
    };

    namespace vector {
        template<class value_t>
        std::vector<value_t> convert2(const property_t& c, const std::string &parameterName) {
            try {
                const property_t::vector& vct = boost::get<property_t::vector>(c);
                std::vector<value_t> ret;
                for(property_t::vector::const_iterator i = vct.begin(); i != vct.end(); ++i) {
                    ret.push_back(ObjectHandler::convert2<value_t, property_t>(*i)); //implicit property_t constructor call!
                }
                return ret;
            } catch(const std::exception &e) {
                OH_FAIL("vector property: unable to convert parameter '" << parameterName 
                    << "' to type '" << typeid(value_t).name() << "' - " << e.what());
            }
        }
    }
    
    namespace matrix {
        template<class value_t>
        std::vector<std::vector<value_t> > convert2(const property_t& c, const std::string &parameterName) {
            try {
                const property_t::vector& matrix = boost::get<property_t::vector>(c);
                std::vector<std::vector<value_t> > ret;
                for(property_t::vector::const_iterator i = matrix.begin(); i != matrix.end(); ++i) {
                    std::vector<value_t> rowOut;
                    const property_t::vector& rowIn = boost::get<property_t::vector>(*i);
                    for(property_t::vector::const_iterator j = rowIn.begin(); j != rowIn.end(); ++j) {
                        rowOut.push_back(ObjectHandler::convert2<value_t, property_t>(*j)); //implicit property_t constructor call!
                    }
                    ret.push_back(rowOut);                
                }
                return ret;
            } catch(const std::exception &e) {
                OH_FAIL("property matrix: unable to convert parameter '" << parameterName 
                    << "' to type '" << typeid(value_t).name() << "' - " << e.what());
            }
        }
    }

    //! Definition of the user property type.
    /*typedef boost::make_recursive_variant<empty_property_tag, long, double, std::string,
        std::vector<bool>, std::vector<long>, std::vector<double>, std::vector<std::string>, 
        std::vector<std::vector<bool> >, std::vector<std::vector<long> >, std::vector<std::vector<double> >, std::vector<std::vector<std::string> >, 
        std::vector<boost::recursive_variant_>,
        std::vector<std::vector<boost::recursive_variant_> > >::type property_t;

    typedef boost::make_recursive_variant<
        empty_property_tag, long, double, std::string,
        std::vector<boost::recursive_variant_> >::type property_t;*/
    
    /*template<>
    struct convert2<bool> {
        bool operator()(const property_t& p, const std::string &parameterName, bool defaultValue) {
            #pragma warning(disable:4800)
            return convert2<long>()(p, parameterName, defaultValue);
            #pragma warning(default:4800)
        }

        bool operator()(const property_t& p) {
            #pragma warning(disable:4800)
            return convert2<long>()(p);
            #pragma warning(default:4800)
        }
    };*/

    //! Helper template to call ohVariantToScalar on a vector.
    /*template <class T>
    std::vector<T> propertyVector2(
        const std::vector<property_t> &variant,
        const std::string &parameterName) {

        try {
            std::vector<T> ret;
            for (std::vector<property_t>::const_iterator i = variant.begin(); i != variant.end(); ++i) {
                ret.push_back(convert2<T>()(*i));
            }
            return ret;
        } catch (const std::exception &e) {
            OH_FAIL("ohVariantToVector: unable to convert parameter '" << parameterName 
                << "' to type '" << typeid(T).name() << "' - " << e.what());
        }
    }*/

    //! Helper template to call ohVariantToScalar on a matrix.
    /*template <class T>
    std::vector<std::vector<T> > propertyMatrix2(
        const std::vector<std::vector<property_t> > &variant,
        const std::string &parameterName) {

        try {
            std::vector<std::vector<T> > ret;
            for (std::vector<std::vector<property_t> >::const_iterator i = variant.begin();
                i != variant.end(); ++i) {
                std::vector<T> rowOut;
                for (std::vector<property_t>::const_iterator j = i->begin(); j != i->end(); ++j) {
                    rowOut.push_back(convert2<T>()(*j));                
                }
                ret.push_back(rowOut);                
            }
            return ret;
        } catch (const std::exception &e) {
            OH_FAIL("ohVariantToMatrix: unable to convert parameter '" << parameterName 
                << "' to type '" << typeid(T).name() << "' - " << e.what());
        }
    }*/

    /*//! Helper template to call VariantToScalarObject on a vector.
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
    }*/

    /* //! Typedef for the datatype of the variant value data member
    typedef boost::variant<long, double, bool, std::string, Other> ScalarVariant;

    //! Definition of the user property type.
    typedef boost::variant<bool, long, double, std::string, ScalarVariant,
            std::vector<bool>, std::vector<long>, std::vector<double>, std::vector<std::string>, std::vector<ScalarVariant>,
            std::vector<std::vector<bool> >, std::vector<std::vector<long> >, std::vector<std::vector<double> >, 
            std::vector<std::vector<std::string> >, std::vector<std::vector<ScalarVariant> >
        > Variant;*/

    //! A class to represent any of a selection of native C++ datatypes
    /*!
        This class wraps a boost::variant value with conversion operators and
        additional functionality required by ObjectHandler.

        The same interface is supported by classes ConvertOper and Variant, allowing
        any conversion algorithm that is based on VariantToScalar to be applied to
        either class, or to other user-defined types which support the same interface.
    */
/*    class Variant {
        friend class boost::serialization::access;

    public:
        //! Constructor - initialize the variant.
        Variant(const VariantDef &variant = Other()) : variant_(variant) {}
        Variant(const long &l) : variant_(l) {}
        Variant(const double &d) : variant_(d) {}
        Variant(const bool &b) : variant_(b) {}
        Variant(const char *s) : variant_(std::string(s)) {}
        Variant(const std::string &s) : variant_(s) {}

        //! \name Inspectors
        //@{
        //! Return the datatype of the underlying value.
        Type type() const;
        //! Indicate whether the Variant value is missing.
        bool missing() const;
        //! Indicate whether the Variant contains an error value.
        bool error() const;
        //@}

        //! \name Conversion Operators
        //@{
        //! Convert the variant to a long.
        operator unsigned int() const;
        //! Convert the variant to a long.
        operator long() const;
        //! Convert the variant to a double.
        operator double() const;
        //! Convert the variant to a boolean.
        operator bool() const;
        //! Convert the variant to a std::string.
        operator std::string() const;
        //@}

        //! Return a const reference to the underlying value.
        const VariantDef &variant() const { return variant_; }

    private:
        // Support for boost::serialization.
        template<class Archive>
        void serialize(Archive &ar, const unsigned int) {
            ar & boost::serialization::make_nvp<VariantDef>("variant_", variant_);
        }
        // The underlying boost::variant value.
        VariantDef variant_;
    };

    inline Type Variant::type() const {
        Type ret;
        VariantType variantType(ret);
        boost::apply_visitor(variantType, variant_);
        return ret;
    }

    inline bool Variant::missing() const {
        bool ret;
        VariantMissing variantMissing(ret);
        boost::apply_visitor(variantMissing, variant_);
        return ret;
    }

    inline bool Variant::error() const {
        bool ret;
        VariantError variantError(ret);
        boost::apply_visitor(variantError, variant_);
        return ret;
    }

    inline Variant::operator long() const {
        long ret;
        VariantToLong variantToLong(ret);
        boost::apply_visitor(variantToLong, variant_);
        return ret;
    }

    inline Variant::operator unsigned int() const {
        long ret;
        VariantToLong variantToLong(ret);
        boost::apply_visitor(variantToLong, variant_);
        return static_cast<unsigned int>(ret);
    }

    inline Variant::operator double() const {
        double ret;
        VariantToDouble variantToDouble(ret);
        boost::apply_visitor(variantToDouble, variant_);
        return ret;
    }

    inline Variant::operator bool() const {
        bool ret;
        VariantToBoolean variantToBoolean(ret);
        boost::apply_visitor(variantToBoolean, variant_);
        return ret;
    }

    inline Variant::operator std::string() const {
        std::string ret;
        VariantToString variantToString(ret);
        boost::apply_visitor(variantToString, variant_);
        return ret;
    }

    inline std::ostream &operator<<(std::ostream &out, const Variant &variant) {
        return out << variant.variant();
    }
*/
}

#endif



/*
 Copyright (C) 2007 Eric Ehlers

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

#ifndef oh_variant_hpp
#define oh_variant_hpp

#include <oh/ohdefines.hpp>
#include <oh/other.hpp>
#include <oh/variantvisitors.hpp>
#include <boost/variant.hpp>
#include <boost/serialization/access.hpp>
#include <boost/serialization/nvp.hpp>
#include <string>

namespace ObjectHandler {

    //! A class to represent any of a selection of native C++ datatypes
    /*!
        This class wraps a boost::variant value with conversion operators and
        additional functionality required by ObjectHandler.

        The same interface is supported by classes ConvertOper and Variant, allowing
        any conversion algorithm that is based on VariantToScalar to be applied to
        either class, or to other user-defined types which support the same interface.
    */
    class Variant {
        friend class boost::serialization::access;

    public:
        //! Typedef for the datatype of the variant value data member
        typedef boost::variant<long, double, bool, std::string, Other> VariantDef;

        //! Constructor - initialize the variant.
        Variant(const VariantDef &variant = Other()) : variant_(variant) {}

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

}

#endif


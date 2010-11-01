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
    \brief Class property_t - A class to represent any of a selection of native C++ datatypes
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

    //! Placeholder for a null value
    struct empty_property_tag {
		empty_property_tag(){}
        //! Serialize this value to/from an archive
        template<class Archive>
        void serialize(Archive &ar, const unsigned int) {}
    };

    //! The underlying types supported by property_t
    typedef boost::make_recursive_variant<empty_property_tag, bool, std::string, long, double,
            std::vector<boost::recursive_variant_> >::type property_base;

    //! A value of variant type
    /*! Class property_t is a wrapper for boost::variant, which is natively
        supported by boost::serialization.  Therefore any value of type property_t
        can be serialized and deserialized.
    */
    class property_t : public property_base {
        friend class boost::serialization::access;
    public:
        typedef std::vector<property_base> vector;

        //! \name Structors
        //@{
        //! Empty default constructor
        property_t() {}
        //! Explicit copy constructor
        property_t(const property_t& t) : property_base(static_cast<const property_base&>(t)) {}
        //! Construct from char*
        property_t(const char* s) : property_base(std::string(s)) {}
        //! Construct from T
        template<typename T>
        property_t(const T& t) : property_base(t) {}
        //! Construct from std::vector<T>
        template<typename T>
        property_t(const std::vector<T>& vct) {
            vector row;
            for(typename std::vector<T>::const_iterator i = vct.begin(); i != vct.end(); ++i)
                row.push_back(*i);

            property_base::operator= <vector>(row);
        }
        //! Construct from std::vector<std::vector<T> >
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
        //@}

        //! \name Operators
        //@{
        //! Assignment operator
        property_t& operator=(const property_t& rhs) {
            property_base::operator=(static_cast<const property_base&>(rhs));
            return *this;
        }
        //! Function call operator - return underlying data
        template<typename T>
        operator T() const { return boost::get<T>(*this); }
        //! Type conversion operator
        operator property_t() const { return *this; }
        //@}

        //! \name Inspectors
        //@{
        //! Boolean indicating whether underlying value is populated
        bool missing() const { return which() == 0; }
        //@}
    protected:
        //! \name Serialization
        //@{
        //! Serialize this value to/from the given archive
        template<class Archive>
        void serialize(Archive &ar, const unsigned int ver) {
            boost::serialization::serialize(ar, *static_cast<property_base*>(this), ver);
        }
        //@}
    };

    //! Template function to convert a vector from type property_t to type value_t
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
    
    //! Template function to convert a matrix from type property_t to type value_t
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

    //! Log the given property_t value to the given stream.
    inline std::ostream &operator<<(std::ostream &out, const property_t &p) {

        // FIXME - I can't figure out how to implement a visitor
        // for a recursive variant :-(
        //boost::apply_visitor(stream_visitor(out), p);

        if (const double* val = boost::get<double>(&p))
            out << *val;
        else if (const std::string* val = boost::get<std::string>(&p))
            out << *val;
        else if (const long* val = boost::get<long>(&p))
            out << *val;
        else if (const bool* val = boost::get<bool>(&p))
            out << *val;
        //else if (const empty_property_tag* val = boost::get<empty_property_tag>(&p))
        //    out << "[Null value]";
        //else if (const std::vector<boost::recursive_variant_>* val =
        //    boost::get<std::vector<boost::recursive_variant_> >(&p))
        //    out << "[Logging not supported for values of type vector]";
        else
            out << "[Attempt to log a value of unknown datatype : '" << p.type().name() << "']";
        return out;
    }

}

#endif


/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

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
    \brief class Other - Platform-independent representation of non-native types
*/

#ifndef oh_other_hpp
#define oh_other_hpp

#include <oh/ohdefines.hpp>
#include <oh/exception.hpp>
#include <oh/types.hpp>
#include <boost/serialization/access.hpp>
#include <boost/serialization/nvp.hpp>
#include <iostream>

namespace ObjectHandler {

    //! Platform-independent representation of non-native types
    /*! This class encapsulates types not native to C++, such
        as Error and Missing, in a container that supports the
        boost::serialization interface.
    */
    class Other {
        friend class boost::serialization::access;

    public:
        //! Constructor - initialize the datatype.
        Other(const Type &type = Null) : type_(type) {}
        //! Return the datatype.
        const Type &type() const { return type_; }

    private:
        // Support for boost::serialization.
        template<class Archive>
        void serialize(Archive &ar, const unsigned int) {
            ar & boost::serialization::make_nvp<Type>("type_", type_);
        }
        // The datatype represented.
        Type type_;
    };

    inline std::ostream &operator<<(std::ostream &out, const Other &other) {
        return out << other.type();
    }

}

#endif


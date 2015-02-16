/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl
 Copyright (C) 2003, 2004 StatPro Italia srl
 Copyright (C) 2010 Kakhkhor Abdijalilov

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

/*! \file null.hpp
    \brief null values
*/

#ifndef quantlib_null_hpp
#define quantlib_null_hpp

#include <ql/types.hpp>

#if defined(__GNUC__) && (((__GNUC__ == 4) && (__GNUC_MINOR__ >= 8)) || (__GNUC__ > 4))
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-local-typedefs"
#endif

#include <boost/type_traits.hpp>

#if defined(__GNUC__) && (((__GNUC__ == 4) && (__GNUC_MINOR__ >= 8)) || (__GNUC__ > 4))
#pragma GCC diagnostic pop
#endif

namespace QuantLib {

    //! template class providing a null value for a given type.
    template <class Type>
    class Null;


    namespace detail {

        template <bool>
        struct FloatingPointNull;

        // null value for floating-point types
        template <>
        struct FloatingPointNull<true> {
            static float nullValue() {
                return QL_NULL_REAL;
            }
        };

        // null value for integer types
        template <>
        struct FloatingPointNull<false> {
            static int nullValue() {
                return QL_NULL_INTEGER;
            }
        };

    }

    // default implementation for built-in types
    template <typename T>
    class Null {
      public:
        Null() {}
        operator T() const {
            return T(detail::FloatingPointNull<
                         boost::is_floating_point<T>::value>::nullValue());
        }
    };

#ifdef QLCPPAD
	template <class Base> class Null<CppAD::AD<Base> > {
  public:
    Null() {}
    operator CppAD::AD<Base>() const {
        return CppAD::AD<Base>(static_cast<Base>(Null<Base>()));
    }
    // this is needed, because in ad_assign.hpp line 124ff
    // assignment from T to AD<Base> is done via conversion from T
    // to Base and then to AD<Base>. If for example
    // T = Null<CppAD::AD<double>> we need to be able to convert
    // to double so that then conversion to AD<double> from this
    // works.
    operator Base() const { return static_cast<Base>(Null<Base>()); }
};
#endif

}


#endif

/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2015 Peter Caspers

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

/*! \file qlcppad.hpp
    \brief defines for CppAD usage
*/

#ifndef ql_cppad_hpp
#define ql_cppad_hpp

#include <cppad/cppad.hpp>

namespace CppAD {

    template<class Base> CppAD::AD<Base> max(CppAD::AD<Base> x,CppAD::AD<Base> y) {
        return CppAD::CondExpGt(x,y,x,y);
    }

    template<class Base> CppAD::AD<Base> min(CppAD::AD<Base> x,CppAD::AD<Base> y) {
        return CppAD::CondExpLt(x,y,x,y);
    }

}

using CppAD::max;
using CppAD::min;

using CppAD::pow;
using CppAD::log;
using CppAD::exp;
using CppAD::abs;
using CppAD::sqrt;

using CppAD::CondExpLt;
using CppAD::CondExpLe;
using CppAD::CondExpGt;
using CppAD::CondExpGe;
using CppAD::CondExpEq;

namespace QuantLib {

    template <class Base>
    class Null<CppAD::AD<Base> > {
      public:
        Null() {}
        operator CppAD::AD<Base>() const {
            return CppAD::AD<Base>(static_cast<Base>(Null<Base>()));
        }
        operator Base() const {
            return static_cast<Base>(Null<Base>());
        }
    };


}

#endif

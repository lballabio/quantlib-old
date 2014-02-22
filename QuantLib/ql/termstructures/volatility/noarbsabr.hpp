/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2014 Peter Caspers

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

/*! \file noarbsabr.hpp
    \brief No-arbitrage SABR
    Reference: Paul Doust, No-arbitrage SABR, 
               The Journal of Computational Finance (3–31) Volume 15/Number 3, Spring 2012
*/

#ifndef quantlib_noarb_sabr
#define quantlib_noarb_sabr

#include <ql/qldefines.hpp>
#include <ql/types.hpp>

namespace QuantLib {


    namespace detail {

        const Real d0(const Real forward,
                      const Real expiryTime,
                      const Real alpha,
                      const Real beta,
                      const Real nu,
                      const Real rho);

    }
    

}


#endif

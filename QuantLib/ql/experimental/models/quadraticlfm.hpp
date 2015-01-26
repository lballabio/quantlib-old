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

/*! \file quadraticlfm.hpp
    \brief deterministic lfm with quadratic local vol
*/

#ifndef quantlib_quadraticlfm_hpp
#define quantlib_quadraticlfm_hpp

#include <ql/types.hpp>
#include <ql/errors.hpp>
#include <vector>

#include <iostream>

namespace QuantLib {

class QuadraticLfm {

  public:
    QuadraticLfm(const std::vector<Real> &rateTimes,
                 const std::vector<Real> &initialForwards,
                 const std::vector<Real> &)
        : rateTimes_(rateTimes), initialForwards_(initialForwards) {

        N_ = rateTimes.size();
        QL_REQUIRE(N_ - 1 == initialForwards_.size(),
                   "rateTimes size ("
                       << N_
                       << ") minus 1 must be equal to number of forwards ("
                       << initialForwards_.size() << ")");
    }

    /* markovian projection local volatility \eta(t,S) */
    Real eta(const Real t, const Real S);

    /* S_{n,m} */
    Real S(const Size n, const Size m, const Size step = 1);

    /* dS_{n,m} / dL_i freezed at time zero */
    Real dSdL(const Size n, const Size m, const Size step, const Size i);

    /* t_{q-1} <= t < t_q */
    int q(const Real t);

  private:
    std::vector<Real> rateTimes_, initialForwards_;
    Size N_;
};
}

#endif

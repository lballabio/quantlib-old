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
    QuadraticLfm(std::vector<Real> &rateTimes,
                 std::vector<Real> &initialForwards,
                 std::vector<std::vector<std::vector<Real> > > &sigma,
                 std::vector<std::vector<Real> > &b,
                 std::vector<std::vector<Real> > &c)
        : rateTimes_(rateTimes), initialForwards_(initialForwards),
          sigma_(sigma), b_(b), c_(c) {
        N_ = rateTimes.size();
        QL_REQUIRE(N_ - 1 == initialForwards_.size(),
                   "rateTimes size ("
                       << N_
                       << ") minus 1 must be equal to number of forwards ("
                       << initialForwards_.size() << ")");
        K_ = sigma_.size();
        QL_REQUIRE(K_ >= 1, "number of factors ("
                                << K_ << ") must be greater or equal to one");
        for (Size k = 0; k < K_; ++k) {
            QL_REQUIRE(N_ - 1 == sigma_[k].size(),
                       "for factor k ("
                           << k << ") the number of sigma functions ("
                           << sigma_[k].size()
                           << ") must be equal to the number of forwards N-1 ("
                           << (N_ - 1) << ")");
            for (Size i = 0; i < N_ - 1; ++i) {
                QL_REQUIRE(N_ - 1 == sigma_[k][i].size(),
                           "for factor k (" << k << ") and Libor i (" << i
                                            << ") the piecewise sigma function "
                                               "must consist of N-1 ("
                                            << (N_ - 1) << ") values, but is ("
                                            << sigma_[k][i].size() << ")");
            }
        }
    }

    /* markovian projection local volatility \eta(t,S) */
    Real eta(const Real t, const Real S);

    /* dS_{n,m} / dL_i freezed at time zero */
    Real dSdL(const Size n, const Size m, const Size step, const Size i,
              const Real h = 1E-5);

    /* S_{n,m}(0) */
    Real S(const Size n, const Size m, const Size step = 1);

    /* P(0,t_n,t_m) */
    Real P(const Size n, const Size m);

    /* t_{q-1} <= t < t_q */
    int q(const Real t);

  private:
    std::vector<Real> &rateTimes_, initialForwards_;
    std::vector<std::vector<std::vector<Real> > > &sigma_;
    std::vector<std::vector<Real> > &b_, &c_;
    Size N_, K_;
};
}

#endif

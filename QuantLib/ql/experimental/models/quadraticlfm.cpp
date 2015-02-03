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

#include <ql/experimental/models/quadraticlfm.hpp>

#include <algorithm>

namespace QuantLib {

int QuadraticLfm::q(const Real t) {
    QL_REQUIRE(t >= 0.0 && t < rateTimes_[N_ - 2],
               "at time " << t << " all forwards are dead");
    return static_cast<int>(
        std::upper_bound(rateTimes_.begin(), rateTimes_.end(), t) -
        rateTimes_.begin());
}

Real QuadraticLfm::P(const Size n, const Size m) {
    QL_REQUIRE(N_ - 1 >= m && m > n && n >= 0, "for a discount factor 0 <= n ("
                                                   << n << ") < m (" << m
                                                   << ") <= N-1 (" << (N_ - 1)
                                                   << ") must hold");
    Real tmp = 1.0;
    for (Size i = n; i < m; ++i)
        tmp *= 1.0 / (1.0 + initialForwards_[i] * (rateTimes_[i+1] - rateTimes_[i]));
    return tmp;
}

Real QuadraticLfm::S(const Size n, const Size m, const Size step) {
    QL_REQUIRE(N_ - 1 >= m && m > n && n >= 0,
               "for a swap rate 0 <= n (" << n << ") < m (" << m << ") <= N-1 ("
                                          << (N_ - 1) << ") must hold");
    QL_REQUIRE((m - n) % step == 0,
               "m (" << m << ") minus n (" << n << ") = " << (m - n)
                     << " must be divisible by step (" << step << ")");
    Real annuity = 0.0;
    for (Size i = n + 1; i <= m; ++i)
        annuity += P(n, i);
    return (1.0 - P(n, m)) / annuity;
}

Real QuadraticLfm::dSdL(const Size n, const Size m, const Size step,
                        const Size i, const Real h) {
    QL_REQUIRE(N_ - 2 >= i && i >= 0, "for dSdL, 0 <= i (" << i << ") <= N-2 ("
                                                           << (N_ - 2)
                                                           << ") must hold");
    QL_REQUIRE(h > 0.0, "for dSdL h (" << h << ") must be positive");
    Real f = S(n, m, step);
    Real tmp = initialForwards_[i];
    initialForwards_[i] += h;
    Real fh = S(n, m, step);
    initialForwards_[i] = tmp;
    return (fh - f) / h;
}




} // namespace QuantLib

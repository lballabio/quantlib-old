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

#include <ql/experimental/models/lgmzetafunctionpiecewisealpha.hpp>

#include <iostream>

namespace QuantLib {

LgmZetaFunctionPiecewiseAlpha::LgmZetaFunctionPiecewiseAlpha(
    const std::vector<Real> &times, const std::vector<Real> &alphas)
    : LgmZetaFunction<LgmZetaFunctionPiecewiseAlpha>(), times_(times),
      alphas_(alphas), zetas_(std::vector<Real>(times.size())) {

    QL_REQUIRE(times.size() == alphas.size() - 1,
               "times (" << times.size() << ") and alphas (" << alphas.size()
                         << ") inconsistent");
    update();
}

const void LgmZetaFunctionPiecewiseAlpha::updateImpl() const {
    Real sum = 0.0;
    for (Size i = 0; i < times_.size(); ++i) {
        sum += alphas_[i] * alphas_[i] *
               (times_[i] - (i == 0 ? 0.0 : times_[i - 1]));
        zetas_[i] = sum;
    }
}

const Real LgmZetaFunctionPiecewiseAlpha::alphaImpl(const Time t) const {
    if (t < 0.0)
        return 0.0;
    return alphas_[std::min<Size>(
        std::upper_bound(times_.begin(), times_.end(), t) - times_.begin(),
        alphas_.size() - 1)];
}

const Real LgmZetaFunctionPiecewiseAlpha::zetaImpl(const Time t) const {
    if (t < 0.0)
        return 0.0;
    Size i = std::min<Size>(std::upper_bound(times_.begin(), times_.end(), t) -
                                times_.begin(),
                            times_.size() - 1);
    Real res = 0.0;
    if (i >= 1)
        res += zetas_[std::min(i - 1, zetas_.size() - 1)];
    res += alphas_[i] * alphas_[i] * (t - (i == 0 ? 0.0 :times_[i-1]));
    return res;
}

} // namespace QuantLib

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

#include <ql/experimental/models/lgmpiecewisealphaconstantkappa.hpp>

#include <iostream>

namespace QuantLib {

LgmPiecewiseAlphaConstantKappa::LgmPiecewiseAlphaConstantKappa(
    const std::vector<Real> &times, const std::vector<Real> &alphas,
    const Real &kappa)
    : LgmParametrization<LgmPiecewiseAlphaConstantKappa>(), times_(times),
      alphas_(alphas), kappa_(kappa), zetas_(std::vector<Real>(times.size())) {

    QL_REQUIRE(times.size() == alphas.size() - 1,
               "times (" << times.size() << ") and alphas (" << alphas.size()
                         << ") inconsistent");
    update();
}


} // namespace QuantLib

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

#include <ql/math/solvers1d/brent.hpp>
#include <ql/experimental/volatility/noarbsabr.hpp>

#include <boost/make_shared.hpp>
#include <boost/math/special_functions/gamma.hpp>
#include <boost/lambda/lambda.hpp>
#include <boost/lambda/bind.hpp>

namespace QuantLib {

namespace detail {

#include "noarbsabrabsprobs.dat"

D0Interpolator::D0Interpolator(const Real forward, const Real expiryTime,
                               const Real alpha, const Real beta, const Real nu,
                               const Real rho)
    : forward_(forward), expiryTime_(expiryTime), alpha_(alpha), beta_(beta),
      nu_(nu), rho_(rho), gamma_(1.0 / (2.0 * (1.0 - beta_))), target_(0.0) {

  QL_REQUIRE(beta_ <= 0.99 && beta_ > 0.0, "beta must be leq 0.99 and gt 0.0");
  integrator_ = boost::make_shared<GaussLobattoIntegral>(1000, 1E-8);
}

Real D0Interpolator::integrand(const Real y) const {
  return std::exp(-y) * std::pow(y, gamma_ - 1.0) / boost::math::tgamma(gamma_);
}

Real D0Interpolator::phi(const Real d0) const {
  if (d0 < 1e-12)
    return 150.0;
  Brent b;
  Real z = b.solve(
      boost::lambda::bind(&D0Interpolator::d0, this, boost::lambda::_1, d0),
      1E-8, 1.0, 0.0, 150.0);
  return z;
}

Real D0Interpolator::d0(const Real phi, const Real target) const {
  // for \beta = 0.99 (the upper bound we assume here) the integrand is
  // below 1e-19 if y is bigger than 150
  return integrator_->operator()(
      boost::lambda::bind(&D0Interpolator::integrand, this, boost::lambda::_1),
      phi, 150.0) - target;
}

} // namespace detail

} // namespace QuantLib

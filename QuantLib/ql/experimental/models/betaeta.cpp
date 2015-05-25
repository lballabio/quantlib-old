/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2015 Peter Caspers, Roland Lichters

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

#include <ql/experimental/models/betaeta.hpp>
#include <ql/errors.hpp>
#include <ql/math/comparison.hpp>
#include <ql/math/modifiedbessel.hpp>

#include <boost/math/special_functions/gamma.hpp>

namespace QuantLib {

BetaEta::BetaEta(const std::vector<Real> &times, const std::vector<Real> &alpha,
                 const std::vector<Real> &lambda, const Real &beta,
                 const Real &eta)
    : times_(times), alpha_(alpha), lambda_(lambda), beta_(beta), eta_(eta) {
    QL_REQUIRE(beta > 0.0, "beta (" << beta << ") must be positive");
    QL_REQUIRE(eta > 0.0 && eta <= 1.0, " eta (" << eta
                                                 << ") must be in (0,1]");
    QL_REQUIRE(alpha.size() == times.size() + 1,
               "alpha size (" << alpha.size()
                              << ") must be equal to times size ("
                              << times.size() << ") plus one");
    QL_REQUIRE(lambda.size() == times.size() + 1,
               "lambda size (" << lambda.size()
                               << ") must be equal to times size ("
                               << times.size() << ") plus one");
    for (Size i = 0; i < times.size(); ++i) {
        QL_REQUIRE(times[i] > 0.0, "time #" << i << " (" << times[i]
                                            << ") must be positive");
        if (i < times.size() - 1) {
            QL_REQUIRE(times[i] < times[i + 1],
                       "times must be strictly increasing, #"
                           << i << " and #" << (i + 1) << " are " << times[i]
                           << " and " << times[i + 1] << " respectively");
        }
    }
}

const Real BetaEta::p(const Time t0, const Real x0, const Real t,
                      const Real x) {
    Real nu = 1.0 / (2.0 - 2.0 * eta_);
    Real y0 = this->y(x0);
    Real y = this->y(x);
    Real tau0 = this->tau(t0);
    Real tau = this->tau(t);
    if (eta_ < 0.5) {
        if (close(y0, 0.0))
            return 0.0; // check this !
        if (x0 > -1.0 / beta_) {
            return 0.5 * std::pow(y / y0, nu) * y0 / (tau0 - tau) *
                   (modifiedBesselFunction_i(-nu, y * y0 / (tau0 - tau)) +
                    modifiedBesselFunction_i(nu, y * y0 / (tau0 - tau))) *
                   exp(-(y0 * y0 + y * y) / (2.0 * (tau0 - tau))) * dydx(y0);
        } else {
            return std::sin(M_PI * nu) * M_1_PI * std::pow(y / y0, nu) * y0 /
                   (tau0 - tau) *
                modifiedBesselFunction_k(nu, y * y0 / (tau0 - tau)) *
                   exp(-(y0 * y0 + y * y) / (2.0 * (tau0 - tau))) * dydx(y0);
        }
    } else {
        if (close(eta_, 1.0)) {
            // check this, should we return zero here ?
            if (close(x0, -1.0 / beta_) || close(x, -1.0 / beta_))
                return 0.0;
            return exp(-beta_ * y0) / std::sqrt(2.0 * M_PI * (tau0 - tau)) *
                   exp(-(y0 - y + beta_ * (tau0 - tau) / 2.0) *
                       (y0 - y + beta_ * (tau0 - tau) / 2.0) /
                       (2.0 * (tau0 - tau)));
        }
        if (close(y0, 0.0))
            return 0.0; // here singularTerm has to be retrieved
        return std::pow(y / y0, nu) * y0 / (tau0 - tau) *
               modifiedBesselFunction_i(nu, y * y0 / (tau0 - tau)) *
               exp(-(y0 * y0 + y * y) / (2.0 * (tau0 - tau))) * dydx(y0);
    }
}

const Real BetaEta::singularTerm_y0_0(const Time t0, const Time t,
                                      const Real x) {
    if (eta_ < 0.5 || close(eta_, 1.0))
        return 0.0;
    Real nu = 1.0 / (2.0 - 2.0 * eta_);
    Real y = this->y(x);
    Real tau0 = this->tau(t0);
    Real tau = this->tau(t);
    return boost::math::gamma_q(nu, y * y / (2.0 * (tau0 - tau)));
}

inline const Real BetaEta::tau(const Real t) const {
    Real res = 0.0;
    for (int i = 0; i < upperIndex(t); ++i) {
        res += alpha_[i] * alpha_[i] * (cappedTime(i + 1, t) - flooredTime(i));
    }
    return res;
}

inline const Real BetaEta::y(const Real x) const {
    // for y==1.0, should we maybe write
    // log(fabs(1.0+beta*x)) instead of log(1.0+beta*x) as in the paper ?
    return close(eta_, 1.0) ? std::log(std::fabs(1.0 + beta_ * x)) / beta_
                            : std::pow(std::fabs(1 + beta_ * x), 1.0 - eta_) /
                                  (beta_ * (1.0 - eta_));
}

inline const Real BetaEta::dydx(const Real y) const {
    return close(y, 1.0)
               ? std::exp(-beta_ * y)
               : std::pow((1.0 - eta_) * beta_ * y, -eta_ / (1.0 - eta_));
}

inline const int BetaEta::lowerIndex(const Time t) const {
    return static_cast<int>(std::upper_bound(times_.begin(), times_.end(), t) -
                            times_.begin());
}

inline const int BetaEta::upperIndex(const Time t) const {
    if (t < QL_MIN_POSITIVE_REAL)
        return 0;
    return static_cast<int>(std::upper_bound(times_.begin(), times_.end(),
                                             t - QL_MIN_POSITIVE_REAL) -
                            times_.begin()) +
           1;
}

inline const Real BetaEta::cappedTime(const Size index, const Real cap) const {
    return cap != Null<Real>() ? std::min(cap, time2(index)) : time2(index);
}

inline const Real BetaEta::flooredTime(const Size index,
                                       const Real floor) const {
    return floor != Null<Real>() ? std::max(floor, time2(index)) : time2(index);
}

inline const Real BetaEta::time2(const Size index) const {
    if (index == 0)
        return 0.0;
    if (index > times_.size())
        return QL_MAX_REAL;
    return times_[index - 1];
}

inline const Real BetaEta::alpha(const Size index) const {
    if (index >= alpha_.size())
        return alpha_.back();
    return alpha_[index];
}

inline const Real BetaEta::lambda(const Size index) const {
    if (index >= lambda_.size())
        return lambda_.back();
    return lambda_[index];
}

} // namespace QuantLib

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
    // TODO move the magic constants to some better place
    integrator_ = boost::shared_ptr<GaussLobattoIntegral>(
        new GaussLobattoIntegral(100, 1E-8));
};

class BetaEta::mIntegrand {
    const BetaEta *model_;
    const Real t0_, x0_, t_;

  public:
    mIntegrand(const BetaEta *model, const Real t0, const Real x0, const Real t)
        : model_(model), t0_(t0), x0_(x0), t_(t) {}
    Real operator()(Real x) const {
        return model_->p(t0_, x0_, t_, x) *
               exp(-model_->lambda(t_) * (x - x0_));
    }
};

// TODO, due to (4.11b) M can be tabulated a priori to increase efficiency
const Real
BetaEta::M(const Time t0, const Real x0, const Real t) const {
    // determine a suitable integration domain
    Real s = std::sqrt(tau(t0, t));
    // TODO move the magic constants to some better place
    Real a = x0 - 6.0 * s;
    Real b = x0 + 6.0 * s;
    return std::log(integrator_->operator()(mIntegrand(this, t0, x0, t), a, b));
};

const Real BetaEta::p(const Time t0, const Real x0, const Real t,
                      const Real x) const {
    Real nu = 1.0 / (2.0 - 2.0 * eta_);
    Real y0 = this->y(x0);
    Real y = this->y(x);
    Real tau0 = this->tau(t0);
    Real tau = this->tau(t);
    if (eta_ < 0.5) {
        if (close(y, 0.0)) // i.e. x = -1/beta
            return 0.0;    // TODO is it reasonable to return 0.0 here ?
        if (x > -1.0 / beta_) {
            return 0.5 * std::pow(y0 / y, nu) * y / (tau - tau0) *
                   (modifiedBesselFunction_i_exponentiallyWeighted(
                        -nu, y0 * y / (tau - tau0)) +
                    modifiedBesselFunction_i_exponentiallyWeighted(
                        nu, y0 * y / (tau - tau0))) *
                   exp(-(y - y0) * (y - y0) / (2.0 * (tau - tau0))) * dydx(y);
        } else {
            return std::sin(M_PI * nu) * M_1_PI * std::pow(y0 / y, nu) * y /
                   (tau - tau0) *
                   modifiedBesselFunction_k_exponentiallyWeighted(
                       nu, y0 * y / (tau - tau0)) *
                   exp(-(y - y0) * (y - y0) / (2.0 * (tau - tau0))) * dydx(y);
        }
    } else {
        if (close(eta_, 1.0)) {
            // TODO is it reaonsable to return 0.0 here ?
            if (x <= -1.0 / beta_ || x0 <= -1.0 / beta_)
                return 0.0;
            // if both x and x0 are > -1/beta, y and y0 are well defined
            return exp(-beta_ * y) / std::sqrt(2.0 * M_PI * (tau - tau0)) *
                   exp(-(y - y0 + beta_ * (tau - tau0) / 2.0) *
                       (y - y0 + beta_ * (tau - tau0) / 2.0) /
                       (2.0 * (tau - tau0)));
        }
        // TODO is it reasonable to return 0.0 here ?
        // if we only exclude x=-1/beta, we get a
        // bimodal density with integral 2
        if (x <= -1 / beta_ || x0 <= -1.0 / beta_)
            return 0.0; // the singularTerm_y_0 contributes to the integral
        return std::pow(y0 / y, nu) * y / (tau - tau0) *
               modifiedBesselFunction_i_exponentiallyWeighted(
                   nu, y0 * y / (tau - tau0)) *
               exp(-(y - y0) * (y - y0) / (2.0 * (tau - tau0))) * dydx(y);
    }
};

const Real BetaEta::singularTerm_y_0(const Time t0, const Real x0,
                                     const Time t) const {
    if (eta_ < 0.5 || close(eta_, 1.0))
        return 0.0;
    Real nu = 1.0 / (2.0 - 2.0 * eta_);
    Real y0 = this->y(x0);
    Real tau0 = this->tau(t0);
    Real tau = this->tau(t);
    return boost::math::gamma_q(nu, y0 * y0 / (2.0 * (tau - tau0)));
};

} // namespace QuantLib

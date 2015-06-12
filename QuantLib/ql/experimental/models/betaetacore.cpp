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

#include <ql/experimental/models/betaetacore.hpp>
#include <ql/errors.hpp>
#include <ql/math/modifiedbessel.hpp>
#include <ql/math/integrals/segmentintegral.hpp>
#include <ql/math/integrals/gausslobattointegral.hpp>
#include <ql/math/integrals/trapezoidintegral.hpp>

#include <boost/math/special_functions/gamma.hpp>
#include <boost/make_shared.hpp>

namespace QuantLib {

BetaEtaCore::BetaEtaCore(const Array &times, const Array &alpha,
                         const Array &kappa, const Real &beta, const Real &eta)
    : times_(times), alpha_(alpha), kappa_(kappa), beta_(beta), eta_(eta),
      integrateStdDevs_(8.0) {
    QL_REQUIRE(beta > 0.0, "beta (" << beta << ") must be positive");
    QL_REQUIRE(eta >= 0.0 && eta <= 1.0, " eta (" << eta
                                                  << ") must be in [0,1]");
    QL_REQUIRE(alpha.size() == times.size() + 1,
               "alpha size (" << alpha.size()
                              << ") must be equal to times size ("
                              << times.size() << ") plus one");
    QL_REQUIRE(kappa.size() == 1 || kappa.size() == times.size() + 1,
               "kappa size (" << kappa.size()
                              << ") must be equal to times size ("
                              << times.size() << ") plus one or equal to one");
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
    preIntegrator_ =
        boost::make_shared<GaussLobattoIntegral>(5000000, 1E-8, 1E-8);
    preIntegrator2_ = boost::make_shared<SegmentIntegral>(1000);
    // preIntegrator_ = boost::make_shared<SegmentIntegral>(1000);
    // integrator_ = boost::make_shared<SegmentIntegral>(1000);
    integrator_ = boost::make_shared<GaussLobattoIntegral>(100000, 1E-8, 1E-8);
    ghIntegrator_ = boost::make_shared<GaussHermiteIntegration>(16);
};

// integrand to compute M directly in terms of x
class BetaEtaCore::mIntegrand1 {
    const BetaEtaCore *model_;
    const Real t0_, x0_, t_;

  public:
    mIntegrand1(const BetaEtaCore *model, const Real t0, const Real x0,
                const Real t)
        : model_(model), t0_(t0), x0_(x0), t_(t) {}
    Real operator()(Real x) const {
        return model_->p(t0_, x0_, t_, x) *
               exp(-model_->lambda(t_) * (x - x0_));
    }
};

// integrand to precompute M, 0 < eta < 1, eta != 0.5
class BetaEtaCore::mIntegrand2 {
    const BetaEtaCore *model_;
    const Real v_, u0_;
    const bool onePlusBetaX0Pos_, onePlusBetaXPos_;

  public:
    mIntegrand2(const BetaEtaCore *model, const Real v, const Real u0,
                const bool onePlusBetaX0Pos, const bool onePlusBetaXPos)
        : model_(model), v_(v), u0_(u0), onePlusBetaX0Pos_(onePlusBetaX0Pos),
          onePlusBetaXPos_(onePlusBetaXPos) {}
    Real operator()(Real u) const {
        if (close(u, 0))
            return 0.0; // is that reasonable or shall we interpolate ?
        Real eta = model_->eta();
        Real s0 = 1.0; // onePlusBetaX0Pos_ ? 1.0 : -1.0;
        Real s = 1.0;  // onePlusBetaXPos_ ? 1.0 : -1.0;
        Real res;
        res = s *
              model_->p_y(v_, std::pow(u0_, 1.0 - eta) / (1.0 - eta),
                          std::pow(u, 1.0 - eta) / (1.0 - eta), true) *
              std::exp(-(s * u - s0 * u0_));
        return res;
    }
};

// integrand to compute M, eta = 1
class BetaEtaCore::mIntegrand3 {
    const BetaEtaCore *model_;
    const Real v_, y0_, lambda_;

  public:
    mIntegrand3(const BetaEtaCore *model, const Real v, const Real y0,
                const Real lambda)
        : model_(model), v_(v), y0_(y0), lambda_(lambda) {}
    Real operator()(Real z) const {
        Real beta = model_->beta();
        Real y = M_SQRT2 * std::sqrt(v_) * z + y0_ - beta * v_ / 2.0;
        return exp(-lambda_ * (exp(beta * y) - exp(beta * y0_)) / beta) *
               exp(-z * z);
    }
};

const Real BetaEtaCore::M(const Time t0, const Real x0, const Real t,
                          const bool usePrecomputedValues) const {
    if (x0 <= -1.0 / beta_)
        return 0.0;
    Real lambda = this->lambda(t);
    Real y0 = this->y(x0);
    Real v = this->tau(t0, t);
    if (close(v, 0.0))
        return 0.0;
    // if (close(eta_, 0.0)) {
    //     return 0.5 * lambda * lambda * v;
    // }
    if (close(eta_, 0.5)) {
        return (1.0 + beta_ * x0) * lambda * lambda * v /
               (2.0 + beta_ * lambda * v);
    }
    if (close(eta_, 1.0)) {
        if (x0 < -1.0 / beta_)
            return 0.0;
        Real result =
            M_1_SQRTPI *
            ghIntegrator_->operator()(mIntegrand3(this, v, y0, lambda));
        return std::log(result);
    }
    if (usePrecomputedValues) {
        return M_precomputed(t0, x0, t);
    }
    // determine a suitable integration domain
    Real s = std::sqrt(tau(t0, t));
    if (close(s, 0.0))
        return 0.0;
    Real a = std::max(x0 - integrateStdDevs_ * s, -1.0 / beta_);
    Real b = x0 + integrateStdDevs_ * s;
    Real result1 = integrator_->operator()(mIntegrand1(this, t0, x0, t), a, b);
    Real result2 = singularTerm_y_0(t0, x0, t);
    return std::log(result1 + result2);
};

const Real BetaEtaCore::M_precomputed(const Real t0, const Real x0,
                                      const Real t) const {
    Real v = this->tau(t0, t);
    Real lambda = this->lambda(t);
    if (/*close(eta_, 0.0) ||*/ close(eta_, 0.5) || close(eta_, 1.0)) {
        return M(t0, x0, t);
    }
    Real stddev = std::sqrt(v);
    Real xmax = x0 + integrateStdDevs_ * stddev;
    Real xmin = std::max(x0 - integrateStdDevs_ * stddev, -1.0 / beta_);
    Real smin = 1.0 + beta_ * xmin >= 0.0 ? 1.0 : -1.0;
    Real smax = 1.0 + beta_ * xmax >= 0.0 ? 1.0 : -1.0;
    Real u0 = lambda / beta_ * std::fabs(1.0 + beta_ * x0);
    Real umin = lambda / beta_ * std::fabs(1.0 + beta_ * xmin);
    Real umax = lambda / beta_ * std::fabs(1.0 + beta_ * xmax);
    Real vt =
        v * std::pow(lambda, 2.0 - 2.0 * eta_) * std::pow(beta_, 2.0 * eta_);
    // integration
    Real result = 0.0;
    Real a = std::min(umin, umax);
    Real b = std::max(umin, umax);
    Real i_type1_tt =
        preIntegrator_->operator()(mIntegrand2(this, vt, u0, true, true), a, b);
    // Real i_type1_ft = preIntegrator_->operator()(
    //     mIntegrand2(this, vt, u0, false, true), a, b);
    // Real i_type1_tf = preIntegrator_->operator()(
    //     mIntegrand2(this, vt, u0, true, false), a, b);
    // Real i_type1_ff = preIntegrator_->operator()(
    //     mIntegrand2(this, vt, u0, false, false), a, b);
    // Real i_type2_t = preIntegrator_->operator()(
    //                      mIntegrand2(this, vt, u0, true, true), 0.0, b) +
    //                  preIntegrator_->operator()(
    //                      mIntegrand2(this, vt, u0, true, false), 0.0, a);
    // Real i_type2_f = preIntegrator_->operator()(
    //                      mIntegrand2(this, vt, u0, false, true), 0.0, b) +
    //                  preIntegrator_->operator()(
    //                      mIntegrand2(this, vt, u0, false, false), 0.0, a);
    // std::cout << "smin=" << smin << " smax=" << smax
    //           << " 1+bx=" << (1.0 + beta_ * x0 >= 0.0) << " i1tt=" <<
    //           i_type1_tt
    //           << " i1ft=" << i_type1_ft << " i1tf=" << i_type1_tf
    //           << " i1ff=" << i_type1_ff << " i2t=" << i_type2_t
    //           << " i2f=" << i_type2_f << std::endl;
    mIntegrand2 ig(this, vt, u0, 1.0 + beta_ * x0 >= 0.0, true);
    // todo here we can use the values above
    if (smin > 0.0 && smax > 0.0) {
        result += preIntegrator_->operator()(ig, a, b); // type 1 limits
    }
    // if (smin < 0.0 && smax > 0.0) {
    //     result += preIntegrator_->operator()(
    //         mIntegrand2(this, vt, u0, 1.0 + beta_ * x0 >= 0.0, true), 0.0,
    //         b); // type 2 limits
    //     result += preIntegrator_->operator()(
    //         mIntegrand2(this, vt, u0, 1.0 + beta_ * x0 >= 0.0, false), 0.0,
    //         a);
    // }
    // if (smin < 0.0 && smax < 0.0) {
    //     result += preIntegrator_->operator()(
    //         mIntegrand2(this, vt, u0, 1.0 + beta_ * x0 >= 0.0, false), a,
    //         b); // type 1 limits
    // }
    Real resultm = std::pow(beta_, -eta_ / (eta_ - 1.0));
    Real results = singularTerm_y_0(t0, x0, t);
    // std::cerr << std::setprecision(16) << "vt=" << vt << " u0=" << u0
    //           << " a=" << a << " b=" << b << " result=" << result
    //           << " resultm=" << resultm << std::endl;
    return std::log(result * resultm + results);
}

const void BetaEtaCore::precompute(const Real u0_min, const Real u0_max,
                                   const Real vt_min, const Real vt_max) const {
    Real m = 1.3;
    Real h = 0.01;
    for (Size i = 0; i < 100; ++i) {
        for (Size j = 0; j < 100; ++j) {
            Real u0 = u0_min + ((double)i) * (u0_max - u0_min) / 100.0;
            Real u0h = u0 + h;
            Real u0h2 = u0 + 2.0 * h;
            Real vt = vt_min + ((double)j) * (vt_max - vt_min) / 100.0;
            Real vth = vt + h;
            Real vth2 = vt + 2.0 * h;
            mIntegrand2 ig(this, vt, u0, true, true);
            mIntegrand2 iguh(this, vt, u0h, true, true);
            mIntegrand2 iguh2(this, vt, u0h2, true, true);
            mIntegrand2 igvth(this, vth, u0, true, true);
            mIntegrand2 igvth2(this, vth2, u0, true, true);
            Real a = u0;
            Real b = u0;
            while (ig(b) > 1E-10) {
                b *= m;
            }
            while (ig(a) > 1E-10 && a > 1E-10) {
                a /= m;
            }
            Real res = 0.0;
            Real res_u = 0.0;
            Real res_vt = 0.0;
            Real res_u2 = 0.0;
            Real res_vt2 = 0.0;
            try {
                res += preIntegrator_->operator()(ig, a, b);
                res_u += preIntegrator_->operator()(iguh, a, b);
                res_vt += preIntegrator_->operator()(igvth, a, b);
                res_u2 += preIntegrator_->operator()(iguh2, a, b);
                res_vt2 += preIntegrator_->operator()(igvth2, a, b);
            } catch (...) {
            }
            if (res < 1e-10) {
                res += preIntegrator2_->operator()(ig, a, b);
                res_u += preIntegrator2_->operator()(iguh, a, b);
                res_vt += preIntegrator2_->operator()(igvth, a, b);
                res_u2 += preIntegrator2_->operator()(iguh2, a, b);
                res_vt2 += preIntegrator2_->operator()(igvth2, a, b);
            }
            Real lres = std::log(res);
            Real lres_u =
                (std::log(res_u2) - 2.0 * std::log(res_u) + std::log(res)) /
                (h * h);
            Real lres_vt =
                (std::log(res_vt2) - 2.0 * std::log(res_vt) + std::log(res)) /
                (h * h);
            // std::cout << u0 << " " << vt << " " << res << " " << lres << " "
            //           << lres_u << " " << lres_vt << std::endl;
        }
        //std::cout << std::endl;
    }
}

const Real BetaEtaCore::p_y(const Real v, const Real y0, const Real y,
                            const bool onePlusBetaXPos) const {
    Real nu = 1.0 / (2.0 - 2.0 * eta_);
    if (eta_ < 0.5) {
        // 0.0 < eta < 0.5
        if (close(y, 0.0)) // i.e. x, x0 = -1/beta
            return 0.0;
        return std::pow(y0 / y, nu) * y / v *
               modifiedBesselFunction_i_exponentiallyWeighted(-nu, y0 * y / v) *
               exp(-(y - y0) * (y - y0) / (2.0 * v)) * dydx(y);
        // if (onePlusBetaXPos) {
        //     return 0.5 * std::pow(y0 / y, nu) * y / v *
        //            (modifiedBesselFunction_i_exponentiallyWeighted(-nu,
        //                                                            y0 * y /
        //                                                            v) +
        //             modifiedBesselFunction_i_exponentiallyWeighted(nu, y0 * y
        //             /
        //                                                                    v))
        //                                                                    *
        //            exp(-(y - y0) * (y - y0) / (2.0 * v)) * dydx(y);
        // } else {
        //     return std::sin(M_PI * nu) * M_1_PI * std::pow(y0 / y, nu) * y /
        //     v *
        //            modifiedBesselFunction_k_exponentiallyWeighted(nu,
        //                                                           y0 * y / v)
        //                                                           *
        //            exp(-(y - y0) * (y - y0) / (2.0 * v)) * dydx(y);
        // }
    }
    // eta = 1.0
    if (close(eta_, 1.0)) {
        return exp(-beta_ * y) / std::sqrt(2.0 * M_PI * v) *
               exp(-0.5 * (y - y0 + 0.5 * beta_ * v) *
                   (y - y0 + 0.5 * beta_ * v) / v);
    }
    // 0.5 <= eta < 1.0
    if (!onePlusBetaXPos || close(y, 0.0) || close(y0, 0.0))
        return 0.0; // the singularTerm_y_0 contributes to the integral
    return std::pow(y0 / y, nu) * y / v *
           modifiedBesselFunction_i_exponentiallyWeighted(nu, y0 * y / v) *
           exp(-(y - y0) * (y - y0) / (2.0 * v)) * dydx(y);
}

const Real BetaEtaCore::p(const Time t0, const Real x0, const Real t,
                          const Real x) const {
    if (x < -1.0 / beta_)
        return 0.0;
    Real v = this->tau(t0, t);
    // if (close(eta_, 0.0)) {
    //     return exp(-(x - x0) * (x - x0) / (2.0 * v)) /
    //            std::sqrt(2.0 * M_PI * v);
    // }
    Real y0 = this->y(x0);
    Real y = this->y(x);
    return p_y(v, y0, y, 1.0 + beta_ * x > 0);
};

const Real BetaEtaCore::singularTerm_y_0(const Time t0, const Real x0,
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

/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2015 Peter Caspers
 Copyright (C) 2015 Roland Lichters

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
#include <ql/math/integrals/gausslobattointegral.hpp>
#include <ql/math/integrals/segmentintegral.hpp>
#include <ql/math/interpolations/bilinearinterpolation.hpp>
#include <ql/math/interpolations/bicubicsplineinterpolation.hpp>
#include <ql/methods/finitedifferences/meshers/concentrating1dmesher.hpp>

#include <boost/math/special_functions/gamma.hpp>
#include <boost/make_shared.hpp>

#include <ql/experimental/models/betaetatabulation.cpp>

#include <iostream>

namespace QuantLib {

BetaEtaCore::BetaEtaCore(const Array &times, const Array &alpha,
                         const Array &kappa, const Real &beta, const Real &eta)
    : times_(times), alpha_(alpha), kappa_(kappa), beta_(beta), eta_(eta),
      integrateStdDevs_(8.0), ghPoints_(16), multiplier_(1.00001) {

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

    // integrator and fallback to compute M directly in terms of x
    integrator_ = boost::make_shared<GaussLobattoIntegral>(10000, 1E-8, 1E-8);
    integrator2_ = boost::make_shared<SegmentIntegral>(250);
    // integrator to compute M for case eta = 1
    ghIntegrator_ = boost::make_shared<GaussHermiteIntegration>(ghPoints_);
    // integrator and fallback to tabulate M
    preIntegrator_ =
        boost::make_shared<GaussLobattoIntegral>(10000, 1E-8, 1E-8);
    preIntegrator2_ = boost::make_shared<SegmentIntegral>(250);

    // tabulation data
    etaSize_ = sizeof(detail::eta_pre) / sizeof(detail::eta_pre[0]);
    uSize_ = sizeof(detail::u_pre) / sizeof(detail::u_pre[0]);
    vSize_ = sizeof(detail::v_pre) / sizeof(detail::v_pre[0]);
    eta_pre_ = std::vector<Real>(detail::eta_pre, detail::eta_pre + etaSize_);
    u_pre_ = std::vector<Real>(detail::u_pre, detail::u_pre + uSize_);
    v_pre_ = std::vector<Real>(detail::v_pre, detail::v_pre + vSize_);
    // spline interpolation
    for (Size i = 0; i < etaSize_; ++i) {
        boost::shared_ptr<Matrix> zTmp =
            boost::make_shared<Matrix>(v_pre_.size(), u_pre_.size());
        for (Size uu = 0; uu < uSize_; ++uu)
            for (Size vv = 0; vv < vSize_; ++vv)
                (*zTmp)[vv][uu] = detail::M_pre[i][uu][vv];
        M_datasets_.push_back(zTmp);
        boost::shared_ptr<BilinearInterpolation> tmp =
            boost::make_shared<BilinearInterpolation>(
                u_pre_.begin(), u_pre_.end(), v_pre_.begin(), v_pre_.end(),
                *(M_datasets_[i]));
        tmp->enableExtrapolation();
        M_surfaces_.push_back(tmp);
    }
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

// integrand to precompute M, 0 < eta < 1, eta != 0.5, first approach
class BetaEtaCore::mIntegrand2 {
    const BetaEtaCore *model_;
    const Real v_, u0_;

  public:
    mIntegrand2(const BetaEtaCore *model, const Real v, const Real u0)
        : model_(model), v_(v), u0_(u0) {}
    Real operator()(Real u) const {
        if (close(u, 0))
            return 0.0;
        Real eta = model_->eta();
        return model_->p_y_core(v_, 1.0 / (1.0 - eta),
                                std::pow(u / u0_, 1.0 - eta) / (1.0 - eta)) *
               std::exp(-(u - u0_));
    }
};

// alternative implementation in v*,w
class BetaEtaCore::mIntegrand2a {
    const BetaEtaCore *model_;
    const Real v_, w0_;

  public:
    mIntegrand2a(const BetaEtaCore *model, const Real v, const Real w0)
        : model_(model), v_(v), w0_(w0) {}
    Real operator()(Real w) const {
        if (close(w, 0))
            return 0.0;
        Real eta = model_->eta();
        return std::pow(w, eta / (1.0 - eta)) * model_->p_y_core(1.0, w0_, w) *
               std::exp(-std::pow(v_, 1.0 / (2.0 - 2.0 * eta)) * (1.0 - eta) *
                        (std::pow(w, 1.0 / (1.0 - eta)) -
                         std::pow(w0_, 1.0 / (1.0 - eta))));
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
                          const bool useTabulation) const {

    // since we are assuming a reflecting barrier we can
    // return M = 0 for all eta when y is negative or zero
    if (x0 <= -1.0 / beta_)
        return 0.0;

    Real lambda = this->lambda(t);
    Real v = this->tau(t0, t);

    // for zero volatility we obviously have M = 0
    if (close(v, 0.0))
        return 0.0;

    // without the reflecting barrier at y=0 we could write
    // M = 0.5 *lambda * lambda * v for eta = 0
    // with the reflecting barrier assumed here, we do not
    // have a closed form solution

    if (close(eta_, 0.5)) {
        return M_eta_05(t0, x0, t);
    }
    if (close(eta_, 1.0)) {
        return M_eta_1(t0, x0, t);
    }

    Real result;
    if (useTabulation) {
        result = M_tabulated(t0, x0, t);
    } else {
        // determine a suitable integration domain
        Real s = std::sqrt(tau(t0, t));
        if (close(s, 0.0))
            return 0.0;
        Real a = std::max(x0 - integrateStdDevs_ * s, -1.0 / beta_);
        Real b = x0 + integrateStdDevs_ * s;
        try {
            result = std::log(
                integrator_->operator()(mIntegrand1(this, t0, x0, t), a, b));
        } catch (...) {
            try {
                result = std::log(integrator2_->operator()(
                    mIntegrand1(this, t0, x0, t), a, b));
            } catch (...) {
                QL_FAIL("could not compute M(" << t0 << "," << x0 << "," << t
                                               << "), tried integration over "
                                               << a << "..." << b);
            }
        }
    }

    std::cout << t0 << " " << x0 << " " << t << " " << result << " ";
    // tabulated=" << useTabulation << "\n";
    M_tabulated(t0, x0, t);

    Real singularTerm =
        singularTerm_y_0(t0, x0, t) * exp(-lambda * (-1.0 / beta_ - x0));
    // only take the singular term into account if numerically significant
    if (singularTerm > std::exp(result) * QL_EPSILON) {
        result = std::log(std::exp(result) + singularTerm);
    }
    return result;
};

const Real BetaEtaCore::M_eta_1(const Real t0, const Real x0,
                                const Real t) const {
    if (x0 < -1.0 / beta_)
        return 0.0;
    Real lambda = this->lambda(t);

    // eta_ may be not one when this function is called, so we
    // can not use the member function y() to compute y0 here
    Real y0 = std::log(1.0 + beta_ * x0) / beta_;
    Real v = this->tau(t0, t);
    Real result = M_1_SQRTPI *
                  ghIntegrator_->operator()(mIntegrand3(this, v, y0, lambda));
    return std::log(result);
}

const Real BetaEtaCore::M_eta_05(const Real t0, const Real x0,
                                 const Real t) const {
    if (x0 < -1.0 / beta_)
        return 0.0;
    Real lambda = this->lambda(t);
    Real v = this->tau(t0, t);
    return (1.0 + beta_ * x0) * lambda * lambda * v /
           (2.0 + beta_ * lambda * v);
}

const Real BetaEtaCore::M_tabulated(const Real t0, const Real x0,
                                    const Real t) const {

    Real vraw = this->tau(t0, t);
    Real lambda = this->lambda(t);

    if (close(eta_, 0.5) || close(eta_, 1.0)) {
        return M(t0, x0, t);
    }

    Real u0 = lambda / beta_ * std::fabs(1.0 + beta_ * x0);
    Real v =
        vraw * beta_ * beta_ / std::pow(1.0 + beta_ * x0, 2.0 - 2.0 * eta_);

    int etaIdx = std::upper_bound(eta_pre_.begin(), eta_pre_.end(), eta_) -
                 eta_pre_.begin();

    Real eta_weight_1 =
        (etaIdx < static_cast<int>(eta_pre_.size()) ? eta_pre_[etaIdx] - eta_
                                                    : 1.0 - eta_) /
        (etaIdx < static_cast<int>(eta_pre_.size())
             ? (eta_pre_[etaIdx] - eta_pre_[etaIdx - 1])
             : (1.0 - eta_pre_[etaIdx - 1]));

    Real eta_weight_2 = (eta_ - eta_pre_[etaIdx - 1]) /
                        (etaIdx < static_cast<int>(eta_pre_.size())
                             ? (eta_pre_[etaIdx] - eta_pre_[etaIdx - 1])
                             : (1.0 - eta_pre_[etaIdx - 1]));

    Real result_eta_lower = M_surfaces_[etaIdx - 1]->operator()(u0, v);

    Real result_eta_higher;
    if (etaIdx < static_cast<int>(eta_pre_.size())) {
        result_eta_higher = M_surfaces_[etaIdx]->operator()(u0, v);
    } else {
        result_eta_higher = M_eta_1(t0, x0, t);
    }

    Real add = eta_ / (eta_ - 1.0) * std::log(1.0 - eta_) - std::log(u0);

    Real result = add + (result_eta_lower * eta_weight_1 +
                         result_eta_higher * eta_weight_2);

    std::cout << u0 << " " << v << " " << result << " " << M(u0, v) + add
              << "\n";

    return result;
}

// for debug
const Real BetaEtaCore::M_tabulated(const Real u0, const Real v) const {

    QL_REQUIRE(eta_ <= eta_pre_.back(), "M_tabulated(u,S) only defined for eta("
                                            << eta_
                                            << ") <= " << eta_pre_.back());
    int etaIdx = std::upper_bound(eta_pre_.begin(), eta_pre_.end(), eta_) -
                 eta_pre_.begin();

    Real eta_weight_1 =
        (etaIdx < static_cast<int>(eta_pre_.size()) ? eta_pre_[etaIdx] - eta_
                                                    : 1.0 - eta_) /
        (etaIdx < static_cast<int>(eta_pre_.size())
             ? (eta_pre_[etaIdx] - eta_pre_[etaIdx - 1])
             : (1.0 - eta_pre_[etaIdx - 1]));

    Real eta_weight_2 = (eta_ - eta_pre_[etaIdx - 1]) /
                        (etaIdx < static_cast<int>(eta_pre_.size())
                             ? (eta_pre_[etaIdx] - eta_pre_[etaIdx - 1])
                             : (1.0 - eta_pre_[etaIdx - 1]));

    Real result_eta_lower = M_surfaces_[etaIdx - 1]->operator()(u0, v);

    Real result_eta_higher;
    if (etaIdx < static_cast<int>(eta_pre_.size())) {
        result_eta_higher = M_surfaces_[etaIdx]->operator()(u0, v);
    } else {
        QL_FAIL("we should never end up here ...");
    }

    Real result =
        (result_eta_lower * eta_weight_1 + result_eta_higher * eta_weight_2);

    return result;
}

const Real BetaEtaCore::M(const Real u0, const Real S) const {
    Real res;
    if (close(S, 0.0)) {
        res = 1.0;
    } else {
        QL_REQUIRE(!close(eta_, 1.0), "M(u,S) is only defined for eta < 1");
        mIntegrand2 ig(this, S, u0);
        // bisection to determine the integration domain
        Real t = 1E-10, t2 = 1E-12;
        Real la = u0;
        Real lb = u0;
        while (ig(la) > t && la > 1E-8)
            la /= multiplier_;
        while (ig(lb) > t)
            lb *= multiplier_;
        Real tmpa = la;
        Real tmpb = u0;
        Real m = tmpa;
        if ((ig(tmpa) - t) * (ig(tmpb) - t) < 0.0) {
            while (std::fabs(tmpa - tmpb) > 1E-6 && ig(tmpa) < t2) {
                m = (tmpa + tmpb) / 2.0;
                if ((ig(tmpa) - t) * (ig(m) - t) < 0.0)
                    tmpb = m;
                else
                    tmpa = m;
            }
        }
        Real a = m;
        tmpa = u0;
        tmpb = lb;
        m = tmpb;
        if ((ig(tmpa) - t) * (ig(tmpb) - t) < 0.0) {
            while (std::fabs(tmpa - tmpb) > 1E-6 && ig(tmpb) < t2) {
                m = (tmpa + tmpb) / 2.0;
                if ((ig(tmpa) - t) * (ig(m) - t) < 0.0)
                    tmpb = m;
                else
                    tmpa = m;
            }
        }
        Real b = m;
        // while (ig(b) > 1E-20) {
        //     b *= multiplier_;
        // }
        // while (ig(a) > 1E-20 && a > 1E-6) {
        //     a /= multiplier_;
        // }
        // std::cout << "u0=" << u0 << " S=" << S << " a=" << a << " b=" << b
        //           << std::endl;
        // Real tmp = a;
        // while (tmp <= b) {
        //     std::cout << tmp << " " << ig(tmp) << "\n";
        //     tmp += (b - a) / 100.0;
        // }
        try {
            res = preIntegrator_->operator()(ig, a, b);
            // std::cout << "result(gl)=" << res << "\n";
        } catch (...) {
            try {
                res = preIntegrator2_->operator()(ig, a, b);
                // std::cout << "result(sg)=" << res << "\n";
            } catch (...) {
                QL_FAIL("could not compute M("
                        << u0 << "," << S << "), tried integration over " << a
                        << "..." << b);
            }
        }
    }
    return std::log(res);
}

// alternative implementaiton in w,v*
// const Real BetaEtaCore::M_tabulated(const Real t0, const Real x0,
//                                     const Real t) const {

//     Real vraw = this->tau(t0, t);
//     Real lambda = this->lambda(t);

//     if (close(eta_, 0.5) || close(eta_, 1.0)) {
//         return M(t0, x0, t);
//     }

//     Real v = vraw * std::pow(lambda, 2.0 - 2.0 * eta_) *
//              std::pow(beta_, 2.0 * eta_) * std::pow(1.0 - eta_, 2.0 *
//              eta_);
//     Real u0 = lambda / beta_ * std::fabs(1.0 + beta_ * x0);
//     u0 = std::pow(u0, 1.0 - eta_) * std::pow(1.0 - eta_, eta_ - 1.0) /
//          std::sqrt(v);

//     int etaIdx = std::upper_bound(eta_pre_.begin(), eta_pre_.end(), eta_)
//     -
//                  eta_pre_.begin();
//     int uIdx =
//         std::upper_bound(u_pre_.begin(), u_pre_.end(), u0) -
//         u_pre_.begin();

//     int vIdx =
//         std::upper_bound(v_pre_.begin(), v_pre_.end(), v) -
//         v_pre_.begin();

//     // QL_REQUIRE(vIdx < static_cast<int>(v_pre_.size()),
//     //            "M_tabulated can not extrapolate in v direction, v is "
//     //                << v << ", bound is " << v_pre_.back());

//     int uIdx_low, uIdx_high;
//     if (uIdx == 0) {
//         uIdx_low = 0;
//         uIdx_high = 1;
//     } else {
//         uIdx_high = std::min(uIdx, static_cast<int>(u_pre_.size()) - 1);
//         uIdx_low = uIdx_high - 1;
//     }

//     int vIdx_low = std::max(vIdx - 1, 0);
//     int vIdx_high = vIdx_low + 1;

//     Real u_weight_1 =
//         (u_pre_[uIdx_high] - u0) / (u_pre_[uIdx_high] -
//         u_pre_[uIdx_low]);
//     Real u_weight_2 =
//         (u0 - u_pre_[uIdx_low]) / (u_pre_[uIdx_high] - u_pre_[uIdx_low]);

//     Real v_weight_1 =
//         (v_pre_[vIdx_high] - v) / (v_pre_[vIdx_high] - v_pre_[vIdx_low]);
//     Real v_weight_2 =
//         (v - v_pre_[vIdx_low]) / (v_pre_[vIdx_high] - v_pre_[vIdx_low]);

//     Real eta_weight_1 =
//         (etaIdx < static_cast<int>(eta_pre_.size()) ? eta_pre_[etaIdx] -
//         eta_
//                                                     : 1.0 - eta_) /
//         (etaIdx < static_cast<int>(eta_pre_.size())
//              ? (eta_pre_[etaIdx] - eta_pre_[etaIdx - 1])
//              : (1.0 - eta_pre_[etaIdx - 1]));

//     Real eta_weight_2 = (eta_ - eta_pre_[etaIdx - 1]) /
//                         (etaIdx < static_cast<int>(eta_pre_.size())
//                              ? (eta_pre_[etaIdx] - eta_pre_[etaIdx - 1])
//                              : (1.0 - eta_pre_[etaIdx - 1]));

//     Real result_eta_lower = detail::M_pre[etaIdx - 1][uIdx_low][vIdx_low]
//     *
//                                 u_weight_1 * v_weight_1 +
//                             detail::M_pre[etaIdx -
//                             1][uIdx_low][vIdx_high] *
//                                 u_weight_1 * v_weight_2 +
//                             detail::M_pre[etaIdx -
//                             1][uIdx_high][vIdx_low] *
//                                 u_weight_2 * v_weight_1 +
//                             detail::M_pre[etaIdx -
//                             1][uIdx_high][vIdx_high] *
//                                 u_weight_2 * v_weight_2;

//     // std::cout << "eta lower extrapolation\n";
//     // std::cout << "        " << v_pre_[vIdx_low] << " " <<
//     // v_pre_[vIdx_high]
//     //           << std::endl;
//     // std::cout << u_pre_[uIdx_low] << " "
//     //           << detail::M_pre[etaIdx - 1][uIdx_low][vIdx_low] << " "
//     //           << detail::M_pre[etaIdx - 1][uIdx_low][vIdx_high] <<
//     "\n";
//     // std::cout << u_pre_[uIdx_high] << " "
//     //           << detail::M_pre[etaIdx - 1][uIdx_high][vIdx_low] << " "
//     //           << detail::M_pre[etaIdx - 1][uIdx_high][vIdx_high] <<
//     "\n";

//     Real result_eta_higher;
//     if (etaIdx < static_cast<int>(eta_pre_.size())) {
//         result_eta_higher = detail::M_pre[etaIdx][uIdx_low][vIdx_low] *
//                                 u_weight_1 * v_weight_1 +
//                             detail::M_pre[etaIdx][uIdx_low][vIdx_high] *
//                                 u_weight_1 * v_weight_2 +
//                             detail::M_pre[etaIdx][uIdx_high][vIdx_low] *
//                                 u_weight_2 * v_weight_1 +
//                             detail::M_pre[etaIdx][uIdx_high][vIdx_high] *
//                                 u_weight_2 * v_weight_2;
//     } else {
//         result_eta_higher = M_eta_1(t0, x0, t);
//     }

//     Real i = result_eta_lower * eta_weight_1 + result_eta_higher *
//     eta_weight_2;

//     // debug: we return not an interpolated, but the full precomputable
//     value
//     std::cout << u0 << " " << v << " " << M(u0, v) << "\n";
//     //
//     return i;
// }

// // alternative implementation in w, v*
// const Real BetaEtaCore::M(const Real w0, const Real v) const {
//     const Real m = 1.3; // multiplier to determine integration domain
//     Real res = 0.0;
//     if (close(v, 0.0)) {
//         res = 1.0;
//     } else {
//         QL_REQUIRE(!close(eta_, 1.0), "M(w,v) is only defined for eta <
//         1");
//         mIntegrand2a ig(this, v, w0);
//         Real a = w0;
//         Real b = w0;
//         while (ig(b) > 1E-10) {
//             b *= m;
//         }
//         while (ig(a) > 1E-10 && a > 1E-10) {
//             a /= m;
//         }
//         try {
//             res += preIntegrator_->operator()(ig, a, b);
//         } catch (...) {
//             res += preIntegrator2_->operator()(ig, a, b);
//         }
//     }
//     return std::log(res);
// }

const Real BetaEtaCore::p_y_core(const Real v, const Real y0,
                                 const Real y) const {
    QL_REQUIRE(!close(eta_, 1.0), "eta must not be one in p_y_core");
    if (close(y, 0.0) || close(y0, 0.0)) // i.e. x, x0 = -1/beta
        return 0.0;
    Real nu = 1.0 / (2.0 - 2.0 * eta_);
    // 0.0 <= eta < 0.5
    if (eta_ < 0.5) {
        return std::pow(y0 / y, nu) * y / v *
               modifiedBesselFunction_i_exponentiallyWeighted(-nu, y0 * y / v) *
               exp(-(y - y0) * (y - y0) / (2.0 * v)) *
               std::pow(y, eta_ / (eta_ - 1.0));
    }
    // 0.5 <= eta < 1.0
    return std::pow(y0 / y, nu) * y / v *
           modifiedBesselFunction_i_exponentiallyWeighted(nu, y0 * y / v) *
           exp(-(y - y0) * (y - y0) / (2.0 * v)) *
           std::pow(y, eta_ / (eta_ - 1.0));
}

const Real BetaEtaCore::p_y(const Real v, const Real y0, const Real y) const {
    // eta = 1.0
    if (close(eta_, 1.0)) {
        return exp(-beta_ * y) / std::sqrt(2.0 * M_PI * v) *
               exp(-0.5 * (y - y0 + 0.5 * beta_ * v) *
                   (y - y0 + 0.5 * beta_ * v) / v);
    }
    // eta < 1.0
    return p_y_core(v, y0, y) * std::pow(1.0 - eta_, eta_ / (eta_ - 1.0)) *
           std::pow(beta_, eta_ / (eta_ - 1.0));
}

const Real BetaEtaCore::p(const Time t0, const Real x0, const Real t,
                          const Real x) const {
    if (x <= -1.0 / beta_)
        return 0.0;
    Real v = this->tau(t0, t);
    Real y0 = this->y(x0);
    Real y = this->y(x);
    return p_y(v, y0, y);
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

namespace detail {

const void
betaeta_tabulate(betaeta_tabulation_type type, std::ostream &out,
                 const Real eta_min, const Real eta_max, const Real u0_min,
                 const Real u0_max, const Real v_min, const Real v_max,
                 const Size usize, const Size vsize, const Size etasteps,
                 const Real cu, const Real densityu, const Real cv,
                 const Real densityv, const Real ce, const Real densitye) {

    Concentrating1dMesher um(u0_min, u0_max, usize,
                             std::make_pair(cu, densityu), true);
    Concentrating1dMesher vm(v_min, v_max, vsize, std::make_pair(cv, densityv),
                             true);
    Concentrating1dMesher em(eta_min, eta_max, etasteps,
                             std::make_pair(ce, densitye), true);

    out.precision(8);
    if (type == Cpp) {

        out << "/* -*- mode: c++; tab-width: 4; indent-tabs-mode:"
            << "nil; c-basic-offset: 4 -*- */\n"
            << " \n "
            << "/*\n"
            << " Copyright (C) 2015 Peter Caspers\n"
            << " Copyright (C) 2015 Roland Lichters\n"
            << "\n"
            << " This file is part of QuantLib, a "
               "free-software/open-source "
               "library\n"
            << " for financial quantitative analysts and developers - "
               "http://quantlib.org/\n"
            << "\n"
            << " QuantLib is free software: you can redistribute it and/or "
               "modify it\n"
            << " under the terms of the QuantLib license.  You should have "
               "received a\n"
            << " copy of the license along with this program; if not, "
               "please "
               "email\n"
            << " <quantlib-dev@lists.sf.net>. The license is also "
               "available "
               "online at\n"
            << " <http://quantlib.org/license.shtml>.\n"
            << "\n"
            << " This program is distributed in the hope that it will be "
               "useful, but WITHOUT\n"
            << " ANY WARRANTY; without even the implied warranty of "
               "MERCHANTABILITY or FITNESS\n"
            << " FOR A PARTICULAR PURPOSE.  See the license for more "
               "details.\n"
            << "*/\n\n";

        out << "// this file was generated by "
               "QuantLib::detail::betaeta_tabulate\n"
               "// using the following parameters:\n";
        out << "// u0_min = " << u0_min << " u0_max = " << u0_max << "\n";
        out << "// v_min = " << v_min << " v_max = " << v_max << "\n";
        out << "// usize = " << usize << " vsize = " << vsize
            << " etaSteps = " << etasteps << "\n";
        out << "// cu = " << cu << " densityu = " << densityu << "\n";
        out << "// cv = " << cv << " densityv = " << densityv << "\n";
        out << "// ce = " << ce << " densitye = " << densitye << "\n\n";
        out << "namespace QuantLib {\n"
            << "namespace detail {\n\n";
        out << "const Real eta_pre[] = {";
        for (Size i = 0; i < em.size() - 1; ++i)
            out << em.location(i) << (i < em.size() - 2 ? "," : "};\n\n");
        out << "const Real u_pre[] = {";
        for (Size i = 0; i < um.size(); ++i)
            out << um.location(i) << (i < um.size() - 1 ? "," : "};\n\n");
        out << "const Real v_pre[] = {";
        for (int i = 0; i < static_cast<int>(vm.size()); ++i)
            out << (i == -1 ? 0.0 : vm.location(i))
                << (i < static_cast<int>(vm.size()) - 1 ? "," : "};\n\n");

        out << "const Real M_pre[][" << um.size() << "][" << (vm.size())
            << "] = {\n";
    }

    Array times;
    Array alpha(1, 0.01);
    Array kappa(1, 0.01);

    Real h = 1E-4; // step to compute finite difference second derivatives in
                   // the plots

    if (type == Cpp) {
        for (Size e = 0; e < etasteps - 1; ++e) {
            Real eta = em.location(e);
            BetaEtaCore core(times, alpha, kappa, 1.0, eta);
            out << "// ========================  eta=" << eta << "\n";
            out << "{ ";
            for (Size i = 0; i < um.size(); ++i) {
                Real u0 = um.location(i);
                out << "// eta=" << eta << " u=" << u0 << "\n";
                out << "{";
                for (int j = 0; j < static_cast<int>(vm.size()); ++j) {
                    Real v = j == -1 ? 0.0 : vm.location(j);
                    Real lres = core.M(u0, v);
                    out << lres
                        << (j < static_cast<int>(vm.size()) - 1 ? "," : "}");
                }
                out << (i < um.size() - 1 ? ",\n" : "}");
            }
            out << (e < etasteps - 2 ? ",\n" : "};\n");
        }
        out << "} // namespace detail\n"
            << "} // namespace QuantLib\n";
    }

    if (type == GnuplotEUV) {
        for (Size e = 0; e < etasteps - 1; ++e) {
            Real eta = em.location(e);
            BetaEtaCore core(times, alpha, kappa, 1.0, eta);
            for (Size i = 0; i < um.size(); ++i) {
                Real u0 = um.location(i);
                for (int j = 0; j < static_cast<int>(vm.size()); ++j) {
                    Real v = j == -1 ? 0.0 : vm.location(j);
                    Real lres = core.M(u0, v);
                    // Real lresTab = core.M_tabulated(u0, v); // for debug
                    // output currently tabulated values
                    // Real lres_u_h = core.M(u0 + h, v);
                    // Real lres_u_2h = core.M(u0 + 2.0 * h, v);
                    // Real lres_v_h = core.M(u0, v + h);
                    // Real lres_v_2h = core.M(u0, v + 2.0 * h);
                    // Real lres_u = (lres_u_2h - 2.0 * lres_u_h + lres) / (h *
                    // h);
                    // Real lres_v = (lres_v_2h - 2.0 * lres_v_h + lres) / (h *
                    // h);
                    // out << eta << " " << u0 << " " << v << " " << lres << " "
                    //     << lres_u << " " << lres_v << "\n";
                    out << eta << " " << u0 << " " << v << " " << lres
                        << " " /*<< lresTab*/ << "\n";
                }
                out << "\n";
            }
        }
    }

    if (type == GnuplotUEV) {
        for (Size i = 0; i < um.size(); ++i) {
            Real u0 = um.location(i);
            for (Size e = 0; e < etasteps - 1; ++e) {
                Real eta = em.location(e);
                BetaEtaCore core(times, alpha, kappa, 1.0, eta);
                for (int j = 0; j < static_cast<int>(vm.size()); ++j) {
                    Real v = j == -1 ? 0.0 : vm.location(j);
                    Real lres = core.M(u0, v);
                    out << u0 << " " << eta << " " << v << " " << lres << "\n";
                }
                out << "\n";
            }
        }
    }

    if (type == GnuplotVEU) {
        for (int j = -1; j < static_cast<int>(vm.size()); ++j) {
            Real v = j == -1 ? 0.0 : vm.location(j);
            for (Size e = 0; e < etasteps - 1; ++e) {
                Real eta = em.location(e);
                BetaEtaCore core(times, alpha, kappa, 1.0, eta);
                for (Size i = 0; i < um.size(); ++i) {
                    Real u0 = um.location(i);
                    Real lres = core.M(u0, v);
                    out << v << " " << eta << " " << u0 << " " << lres << "\n";
                }
                out << "\n";
            }
        }
    }
}

} // namespace detail

} // namespace QuantLib

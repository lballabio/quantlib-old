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

#include <iostream>

namespace QuantLib {

BetaEtaCore::BetaEtaCore(const Array &times, const Array &alpha,
                         const Array &kappa, const Real &beta, const Real &eta)
    : times_(times), alpha_(alpha), kappa_(kappa), beta_(beta), eta_(eta),
      ghPoints_(8), prob_y_0_cutoff(1E-6), kappa_cutoff(1E-6) {

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
    etaSize_ = detail::eta_pre_size;
    uSize_ = detail::u_pre_size;
    SuSize_ = detail::Su_pre_size;
    vSize_ = detail::v_pre_size;
    y0Size_ = detail::y0_pre_size;

    eta_pre_ = std::vector<Real>(detail::eta_pre, detail::eta_pre + etaSize_);
    u_pre_ = std::vector<Real>(detail::u_pre, detail::u_pre + uSize_);
    Su_pre_ = std::vector<Real>(detail::Su_pre, detail::Su_pre + SuSize_);
    v_pre_ = std::vector<Real>(detail::v_pre, detail::v_pre + vSize_);
    y0_pre_ = std::vector<Real>(detail::y0_pre, detail::y0_pre + y0Size_);

    // interpolation of tabulated data
    for (Size i = 0; i < etaSize_; ++i) {
        boost::shared_ptr<Matrix> zTmp =
            boost::make_shared<Matrix>(Su_pre_.size(), u_pre_.size());
        boost::shared_ptr<Matrix> z2Tmp =
            boost::make_shared<Matrix>(v_pre_.size(), y0_pre_.size());
        for (Size uu = 0; uu < uSize_; ++uu)
            for (Size vv = 0; vv < SuSize_; ++vv)
                (*zTmp)[vv][uu] = detail::M_pre[i][uu][vv];
        for (Size vv = 0; vv < vSize_; ++vv)
            for (Size yy = 0; yy < y0Size_; ++yy)
                (*z2Tmp)[yy][vv] = detail::p_pre[i][yy][vv];
        M_datasets_.push_back(zTmp);
        p_datasets_.push_back(z2Tmp);
        boost::shared_ptr<BilinearInterpolation> tmp =
            boost::make_shared<BilinearInterpolation>(
                u_pre_.begin(), u_pre_.end(), Su_pre_.begin(), Su_pre_.end(),
                *(M_datasets_[i]));
        boost::shared_ptr<BilinearInterpolation> tmp2 =
            boost::make_shared<BilinearInterpolation>(
                v_pre_.begin(), v_pre_.end(), y0_pre_.begin(), y0_pre_.end(),
                *(p_datasets_[i]));
        tmp->enableExtrapolation();
        tmp2->enableExtrapolation();
        M_surfaces_.push_back(tmp);
        p_surfaces_.push_back(tmp2);
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

// integrand to compute prob_y_0 (directly in terms of x, eta < 0.5)
class BetaEtaCore::pIntegrand1 {
    const BetaEtaCore *model_;
    const Real t0_, x0_, t_;

  public:
    pIntegrand1(const BetaEtaCore *model, const Real t0, const Real x0,
                const Real t)
        : model_(model), t0_(t0), x0_(x0), t_(t) {}
    Real operator()(Real x) const { return model_->p(t0_, x0_, t_, x); }
};

// integrand to precompute M, 0 < eta < 1, eta != 0.5
class BetaEtaCore::mIntegrand2 {
    const BetaEtaCore *model_;
    const Real S_, u0_;

  public:
    mIntegrand2(const BetaEtaCore *model, const Real S, const Real u0)
        : model_(model), S_(S), u0_(u0) {}
    Real operator()(Real u) const {
        if (close(u, 0))
            return 0.0;
        Real eta = model_->eta();
        return model_->p_y_core1(
                   S_ * std::pow(1.0 - eta, 2.0 * eta) *
                       std::pow(u0_, 2.0 - 2.0 * eta),
                   std::pow(u0_, 1.0 - eta) * std::pow(1.0 - eta, eta - 1.0),
                   std::pow(u, 1.0 - eta) * std::pow(1.0 - eta, eta - 1.0),
                   eta) *
               exp(-(u - u0_));
    }
};

// integrand to precompute prob_y_0 (eta < 0.5)
class BetaEtaCore::pIntegrand2 {
    const BetaEtaCore *model_;
    const Real v_, y0_;

  public:
    pIntegrand2(const BetaEtaCore *model, const Real v, const Real y0)
        : model_(model), v_(v), y0_(y0) {}
    Real operator()(Real y) const {
        return model_->p_y_core0(v_, y0_, y, model_->eta());
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
        Real s = std::sqrt(tau(t0, t));
        if (close(s, 0.0))
            return 0.0;
        mIntegrand1 in(this, t0, x0, t);
        std::pair<Real, Real> d = detail::domain(
            in, x0, 1E-10, 1E-12, 1E-6, 1.1, -1.0 / beta_, QL_MAX_REAL);
        Real a = d.first;
        Real b = d.second;
        try {
            result = std::log(integrator_->operator()(in, a, b));
        } catch (...) {
            try {
                result = std::log(integrator2_->operator()(in, a, b));
            } catch (...) {
                QL_FAIL("could not compute M(" << t0 << "," << x0 << "," << t
                                               << "), tried integration over "
                                               << a << "..." << b);
            }
        }
    }

    Real singularProb = prob_y_0(t0, x0, t, useTabulation);
    Real singularTerm = 0.0;
    singularTerm = singularProb * exp(-lambda * (-1.0 / beta_ - x0));

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

    Real y0 = y(x0, 1.0);
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
    Real Su = vraw * beta_ * beta_ /
              std::pow(1.0 + beta_ * x0, 2.0 - 2.0 * eta_) *
              std::pow(u0, 2.0 - 0.5 * eta_);

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

    Real result_eta_lower = M_surfaces_[etaIdx - 1]->operator()(u0, Su);

    Real result_eta_higher;
    if (etaIdx < static_cast<int>(eta_pre_.size())) {
        result_eta_higher = M_surfaces_[etaIdx]->operator()(u0, Su);
    } else {
        result_eta_higher = M_eta_1(t0, x0, t);
    }

    Real result =
        (result_eta_lower * eta_weight_1 + result_eta_higher * eta_weight_2);

    if (u0 > u_pre_.back() || Su > Su_pre_.back())
        QL_FAIL("tabulated value lookup ("
                << u0 << "," << Su
                << ") would require extrapolation, bounds are u0_max="
                << u_pre_.back() << " and Su_max=" << Su_pre_.back());

    return result;
}

const Real BetaEtaCore::M(const Real u0, const Real Su) const {
    Real res;
    if (close(Su, 0.0)) {
        res = 1.0;
    } else {
        QL_REQUIRE(!close(eta_, 1.0), "M(u0,Su) is only defined for eta < 1");
        mIntegrand2 ig(this, Su / std::pow(u0, 2.0 - 0.5 * eta_), u0);
        std::pair<Real, Real> d =
            detail::domain(ig, u0, 1E-10, 1E-12, 1E-6, 1.1, 1E-10, QL_MAX_REAL);
        try {
            res = preIntegrator_->operator()(ig, d.first, d.second);
        } catch (...) {
            try {
                res = preIntegrator2_->operator()(ig, d.first, d.second);
            } catch (...) {
                QL_FAIL("could not compute M(" << u0 << "," << Su
                                               << "), tried integration over "
                                               << d.first << "..." << d.second);
            }
        }
    }
    Real a = close(res, 0.0) ? -50.0 : std::log(res);
    return a;
}

const Real BetaEtaCore::p_y_core0(const Real v, const Real y0, const Real y,
                                  const Real eta) const {
    QL_REQUIRE(!close(eta, 1.0), "eta must not be one in p_y_core0");
    if (close(y, 0.0) || close(y0, 0.0)) // i.e. x, x0 = -1/beta
        return 0.0;
    Real nu = 1.0 / (2.0 - 2.0 * eta);
    // 0.0 <= eta < 0.5
    if (eta < 0.5) {
        return std::pow(y0 / y, nu) * y / v *
               modifiedBesselFunction_i_exponentiallyWeighted(-nu, y0 * y / v) *
               exp(-(y - y0) * (y - y0) / (2.0 * v));
    }
    // 0.5 <= eta < 1.0
    return std::pow(y0 / y, nu) * y / v *
           modifiedBesselFunction_i_exponentiallyWeighted(nu, y0 * y / v) *
           exp(-(y - y0) * (y - y0) / (2.0 * v));
}

const Real BetaEtaCore::p_y_core1(const Real v, const Real y0, const Real y,
                                  const Real eta) const {
    QL_REQUIRE(!close(eta, 1.0), "eta must not be one in p_y_core1");
    return p_y_core0(v, y0, y, eta) * std::pow(y, eta / (eta - 1.0));
}

const Real BetaEtaCore::p_y(const Real v, const Real y0, const Real y,
                            const Real eta) const {
    // eta = 1.0
    if (close(eta, 1.0)) {
        return exp(-beta_ * y) / std::sqrt(2.0 * M_PI * v) *
               exp(-0.5 * (y - y0 + 0.5 * beta_ * v) *
                   (y - y0 + 0.5 * beta_ * v) / v);
    }
    // eta < 1.0
    return p_y_core1(v, y0, y, eta) * std::pow(1.0 - eta, eta / (eta - 1.0)) *
           std::pow(beta_, eta / (eta - 1.0));
}

const Real BetaEtaCore::p(const Time t0, const Real x0, const Real t,
                          const Real x) const {
    if (x <= -1.0 / beta_)
        return 0.0;
    // to avoid numerical instabilities when eta is close to
    // but not equal to one we interpolate the density between the largest
    // stable value for eta and one. Since the tabulation should be
    // stable w.r.t. the spanned eta grid, we use the largest grid value
    // from there as the cutoff value (which would typically be something
    // close to 0.99).
    Real v = this->tau(t0, t);
    if (eta_ <= eta_pre_.back()) {
        Real y0 = this->y(x0, eta_);
        Real y = this->y(x, eta_);
        return p_y(v, y0, y, eta_);
    } else {
        Real y0a = this->y(x0, eta_pre_.back());
        Real ya = this->y(x, eta_pre_.back());
        Real y0b = this->y(x0, 1.0);
        Real yb = this->y(x, 1.0);
        return 0.5 * (p_y(v, y0a, ya, eta_pre_.back()) + p_y(v, y0b, yb, 1.0));
    }
};

const Real BetaEtaCore::prob_y_0(const Real v, const Real y0) const {
    if (close(v, 0.0) || eta_ > eta_pre_.back())
        return 0.0;
    if (eta_ >= 0.5) {
        Real nu = 1.0 / (2.0 - 2.0 * eta_);
        Real res = boost::math::gamma_q(nu, y0 * y0 / (2.0 * v));
        return res;
    }
    // eta < 0.5
    pIntegrand2 inC(this, v, y0);
    std::pair<Real, Real> d =
        detail::domain(inC, y0, 1E-10, 1E-12, 1E-6, 1.1, 0.0, QL_MAX_REAL);
    Real a = d.first;
    Real b = d.second;
    Real result;
    try {
        result = 1.0 - integrator_->operator()(inC, a, b);
    } catch (...) {
        try {
            result = 1.0 - integrator2_->operator()(inC, a, b);
        } catch (...) {
            QL_FAIL("could not compute prob_y_0("
                    << v << "," << y0 << "), tried integration over " << a
                    << "..." << b);
        }
    }
    return result;
}

const Real BetaEtaCore::prob_y_0_tabulated(const Real v, const Real y0) const {
    if (close(v, 0.0) || eta_ > eta_pre_.back())
        return 0.0;
    int etaIdx = std::upper_bound(eta_pre_.begin(), eta_pre_.end(), eta_) -
                 eta_pre_.begin();

    // weight formulas are more general than needed because
    // etaIdx < eta_pre_.size() by the condition above
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

    Real result_eta_lower = p_surfaces_[etaIdx - 1]->operator()(v, y0);
    Real result_eta_higher = p_surfaces_[etaIdx]->operator()(v, y0);
    Real result =
        (result_eta_lower * eta_weight_1 + result_eta_higher * eta_weight_2);

    // if (v > v_pre_.back() || y0 > y0_pre_.back())
    //     QL_FAIL("tabulated value lookup ("
    //             << v << "," << y0
    //             << ") would require extrapolation, bounds are v_max="
    //             << v_pre_.back() << " and y0_max=" << y0_pre_.back());

    return result;
}

const Real BetaEtaCore::prob_y_0(const Time t0, const Real x0, const Time t,
                                 bool useTabulation) const {
    Real v = tau(t0, t);
    Real y0 = y(x0, eta_);
    Real result;
    if (useTabulation) {
        result = prob_y_0_tabulated(v, y0);
    } else {
        result = prob_y_0(v, y0);
    }
    // TODO ...
    if(result < prob_y_0_cutoff)
        result = 0.0;
    return result;
};

namespace detail {

const void
betaeta_tabulate(betaeta_tabulation_type type, std::ostream &out,
                 const Real eta_min, const Real eta_max, const Real u0_min,
                 const Real u0_max, const Real Su_min, const Real Su_max,
                 const Size u_size, const Size Su_size, const Size eta_size,
                 const Real c_u, const Real density_u, const Real c_Su,
                 const Real density_Su, const Real c_e, const Real density_e) {

    Concentrating1dMesher um(u0_min, u0_max, u_size,
                             std::make_pair(c_u, density_u), true);
    Concentrating1dMesher sum(Su_min, Su_max, Su_size,
                              std::make_pair(c_Su, density_Su), true);
    Concentrating1dMesher em(eta_min, eta_max, eta_size,
                             std::make_pair(c_e, density_e), true);

    out.precision(8);
    if (type == Cpp_M || type == Cpp_p) {

        out << "/* -*- mode: c++; tab-width: 4; indent-tabs-mode:"
            << "nil; c-basic-offset: 4 -*- */\n"
            << "\n "
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
        if (type == Cpp_M) {
            out << "// u0_min = " << u0_min << " u0_max = " << u0_max << "\n";
            out << "// Su_min = " << Su_min << " Su_max = " << Su_max << "\n";
            out << "// u_size = " << u_size << " Su_size = " << Su_size
                << " eta_size = " << eta_size << "\n";
            out << "// c_u = " << c_u << " density_u = " << density_u << "\n";
            out << "// c_Su = " << c_Su << " density_Su = " << density_Su
                << "\n";
            out << "// c_e = " << c_e << " density_e = " << density_e << "\n\n";
            out << "namespace QuantLib {\n"
                << "namespace detail {\n\n";
            out << "extern \"C\" const double eta_pre[] = {";
            for (Size i = 0; i < em.size() - 1; ++i)
                out << em.location(i) << (i < em.size() - 2 ? "," : "};\n\n");
            out << "extern \"C\" const double u_pre[] = {";
            for (Size i = 0; i < um.size(); ++i)
                out << um.location(i) << (i < um.size() - 1 ? "," : "};\n\n");
            out << "extern \"C\" const double Su_pre[] = {";
            for (int i = 0; i < static_cast<int>(sum.size()); ++i)
                out << (i == -1 ? 0.0 : sum.location(i))
                    << (i < static_cast<int>(sum.size()) - 1 ? "," : "};\n\n");
            out << "extern \"C\" const double M_pre[][" << um.size() << "]["
                << (sum.size()) << "] = {\n";
        } else {
            out << "// y0_min = " << u0_min << " y0_max = " << u0_max << "\n";
            out << "// v_min = " << Su_min << " v_max = " << Su_max << "\n";
            out << "// y0_size = " << u_size << " v_size = " << Su_size
                << " eta_size = " << eta_size << "\n";
            out << "// c_y0 = " << c_u << " density_y0 = " << density_u << "\n";
            out << "// c_v = " << c_Su << " density_v = " << density_Su
                << "\n";
            out << "// c_e = " << c_e << " density_e = " << density_e << "\n\n";
            out << "// note that the eta grid is taken from "
                   "betaetatabulation.cpp\n\n";
            out << "namespace QuantLib {\n"
                << "namespace detail {\n\n";
            out << "extern \"C\" const double y0_pre[] = {";
            for (Size i = 0; i < um.size(); ++i)
                out << um.location(i) << (i < um.size() - 1 ? "," : "};\n\n");
            out << "extern \"C\" const double v_pre[] = {";
            for (int i = 0; i < static_cast<int>(sum.size()); ++i)
                out << (i == -1 ? 0.0 : sum.location(i))
                    << (i < static_cast<int>(sum.size()) - 1 ? "," : "};\n\n");
            out << "extern \"C\" const double p_pre[][" << um.size() << "]["
                << (sum.size()) << "] = {\n";
        }
    }

    Array times;
    Array alpha(1, 0.01);
    Array kappa(1, 0.01);

    if (type == Cpp_M || type == Cpp_p) {
        for (Size e = 0; e < eta_size - 1; ++e) {
            Real eta = em.location(e);
            BetaEtaCore core(times, alpha, kappa, 1.0, eta);
            out << "// ========================  eta=" << eta << "\n";
            out << "{ ";
            for (Size i = 0; i < um.size(); ++i) {
                Real u0 = um.location(i);
                if (type == Cpp_M) {
                    out << "// eta=" << eta << " u0=" << u0 << "\n";
                } else {
                    out << "// eta=" << eta << " y0=" << u0 << "\n";
                }
                out << "{";
                for (int j = 0; j < static_cast<int>(sum.size()); ++j) {
                    Real v = j == -1 ? 0.0 : sum.location(j);
                    Real lres;
                    if (type == Cpp_M) {
                        lres = core.M(u0, v);
                    } else {
                        lres = core.prob_y_0(v, u0);
                    }
                    out << lres
                        << (j < static_cast<int>(sum.size()) - 1 ? "," : "}");
                }
                out << (i < um.size() - 1 ? ",\n" : "}");
            }
            out << (e < eta_size - 2 ? ",\n" : "};\n");
        }
        out << "} // namespace detail\n"
            << "} // namespace QuantLib\n";
    }

    if (type == GnuplotEUV) {
        for (Size e = 0; e < eta_size - 1; ++e) {
            Real eta = em.location(e);
            BetaEtaCore core(times, alpha, kappa, 1.0, eta);
            for (Size i = 0; i < um.size(); ++i) {
                Real u0 = um.location(i);
                for (int j = 0; j < static_cast<int>(sum.size()); ++j) {
                    Real v = j == -1 ? 0.0 : sum.location(j);
                    Real lres = core.M(u0, v);
                    out << eta << " " << u0 << " " << v << " " << lres << "\n";
                }
                out << "\n";
            }
        }
    }

    if (type == GnuplotUEV) {
        for (Size i = 0; i < um.size(); ++i) {
            Real u0 = um.location(i);
            for (Size e = 0; e < eta_size - 1; ++e) {
                Real eta = em.location(e);
                BetaEtaCore core(times, alpha, kappa, 1.0, eta);
                for (int j = 0; j < static_cast<int>(sum.size()); ++j) {
                    Real v = j == -1 ? 0.0 : sum.location(j);
                    Real lres = core.M(u0, v);
                    out << u0 << " " << eta << " " << v << " " << lres << "\n";
                }
                out << "\n";
            }
        }
    }

    if (type == GnuplotVEU) {
        for (int j = 0; j < static_cast<int>(sum.size()); ++j) {
            Real v = j == -1 ? 0.0 : sum.location(j);
            for (Size e = 0; e < eta_size - 1; ++e) {
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

    if (type == GnuplotP) {
        for (Size e = 0; e < eta_size - 1; ++e) {
            Real eta = em.location(e);
            for (Size i = 0; i < um.size(); ++i) {
                Real u0 = um.location(i);
                for (int j = 0; j < static_cast<int>(sum.size()); ++j) {
                    Real v = j == -1 ? 0.0 : sum.location(j);
                    BetaEtaCore core(times, alpha, kappa, 1.0, eta);
                    Real lres = core.prob_y_0(v, u0);
                    out << eta << " " << v << " " << u0 << " " << lres << "\n";
                }
                out << "\n";
            }
        }
    }
}

} // namespace detail

} // namespace QuantLib

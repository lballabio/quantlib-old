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

#include <ql/experimental/volatility/noarbsabr.hpp>

#include <ql/math/solvers1d/brent.hpp>
#include <ql/math/solvers1d/finitedifferencenewtonsafe.hpp>

#include <boost/make_shared.hpp>
#include <boost/math/special_functions/gamma.hpp>
#include <boost/math/special_functions/bessel.hpp>
#include <boost/lambda/lambda.hpp>
#include <boost/lambda/bind.hpp>
#include <boost/assign/std/vector.hpp>

#include <iostream>

namespace QuantLib {

#define QL_NOARBSABR_PHI_ACCURACY 1E-6
#define QL_NOARBSABR_BETA_CUTOFF 0.99
#define QL_NOARBSABR_PHITAU_CUTOFF 150.0 // phi(d0)/tau
#define QL_NOARBSABR_NSIM 2500000.0
#define QL_NOARBSABR_TINY_PROB 1E-5
#define QL_NOARBSABR_MINSTRIKE 0.00001
#define QL_NOARBSABR_GL_ACCURACY 1E-6
#define QL_NOARBSABR_GL_MAX_ITERATIONS 10000
#define QL_NOARBSABR_FORWARD_ACCURACY 0.00001
#define QL_NOARBSABR_FORWARD_SEARCH_STEP 0.0010

NoArbSabr::NoArbSabr(const Real expiryTime, const Real forward,
                     const Real alpha, const Real beta, const Real nu,
                     const Real rho)
    : expiryTime_(expiryTime), externalForward_(forward), alpha_(alpha),
      beta_(beta), nu_(nu), rho_(rho), forward_(forward) {

    // determine a region which is numerically valid or at least manageable

    if( boost::math::isinf( p(forward_, true) ) )
        QL_FAIL("p is infinite at forward, giving up");

    fmin_ = fmax_ = forward_;
    bool back = false;
    for (Real tmp = p(fmax_, false);
         (tmp > 1.0e-10 && !back) || boost::math::isnan(tmp); tmp=p(fmax_,false)) {
        if (boost::math::isinf(tmp) || boost::math::isnan(tmp)) {
            fmax_ *= 0.75;
            back = true;
        } else {
            fmax_ *= 2.0;
        }
    }
    back = false;
    for (Real tmp = p(fmin_, false);
         (tmp > 1.0e-10 && !back) || boost::math::isnan(tmp); tmp=p(fmin_,false)) {
        if (boost::math::isinf(tmp) || boost::math::isnan(tmp)) {
            fmin_ *= 1.5;
            back = true;
        } else {
            fmin_ *= 0.5;
            if (fmin_ < QL_NOARBSABR_MINSTRIKE) {
                fmin_ = QL_NOARBSABR_MINSTRIKE;
                back = true;
            }
        }
    }

    std::cout << "fmin =" << fmin_ << " p =" << p(fmin_,false) << std::endl;
    std::cout << "fmax =" << fmax_ << " p =" << p(fmax_,false) << std::endl;

    integrator_ = boost::make_shared<GaussLobattoIntegral>(
        QL_NOARBSABR_GL_MAX_ITERATIONS, QL_NOARBSABR_GL_ACCURACY);

    detail::D0Interpolator d0(forward_, expiryTime_, alpha_, beta_, nu_, rho_);
    absProb_ = d0();

    std::cout << "absprob is " << absProb_ << std::endl;

    FiniteDifferenceNewtonSafe n;
    forward_ = n.solve(
        boost::lambda::bind(&NoArbSabr::forwardError, this, boost::lambda::_1),
        QL_NOARBSABR_FORWARD_ACCURACY, externalForward_, QL_NOARBSABR_FORWARD_SEARCH_STEP);

    forwardError(forward_); // make sure that the numericalIntegralOverP_ is consistent with found forward_

    std::cout << "found forward" << forward_ << " absprob is " << absProb_ << std::endl;

}

Real NoArbSabr::optionPrice(const Real strike) const {
    return (1.0 - absProb_) *
           integrator_->operator()(boost::lambda::bind(&NoArbSabr::integrand,
                                                       this, strike,
                                                       boost::lambda::_1),
                                   fmin_, fmax_) /
           numericalIntegralOverP_;
}

Real NoArbSabr::digitalOptionPrice(const Real strike) const {
    
    return strike < QL_EPSILON ? 1.0 : 
        (1.0 - absProb_) *
           integrator_->operator()(boost::lambda::bind(&NoArbSabr::p,
                                                       this, boost::lambda::_1, true),
                                   strike, fmax_) /
           numericalIntegralOverP_;
}

Real NoArbSabr::forwardError(const Real forward) const {
    forward_ = std::min(std::max(forward,fmin_),fmax_);
    numericalIntegralOverP_ = integrator_->operator()(
        boost::lambda::bind(&NoArbSabr::p, this, boost::lambda::_1, false),
        fmin_, fmax_);
    Real numericalForward = optionPrice(0.0);
    std::cout << "*** Iteration, trying forward" << forward_ << std::endl;
    std::cout << "numerical integral over p =" << numericalIntegralOverP_
              << std::endl;
    std::cout << "numerical forward = " << numericalForward << std::endl;
    return numericalForward - externalForward_;
}

Real NoArbSabr::integrand(const Real strike, const Real f) const {
    return std::max(f - strike, 0.0) * p(f);
}

Real NoArbSabr::p(const Real f, const bool checkNumericalLimits) const {

    if (checkNumericalLimits && (f < fmin_ || f > fmax_))
        return 0.0;

    Real fOmB = std::pow(f, 1.0 - beta_);
    Real FOmB = std::pow(forward_, 1.0 - beta_);

    Real zf = fOmB / (alpha_ * (1.0 - beta_));
    Real zF = FOmB / (alpha_ * (1.0 - beta_));
    Real z = zF - zf;

    // Real JzF = std::sqrt(1.0 - 2.0 * rho_ * nu_ * zF + nu_ * nu_ * zF * zF);
    Real Jmzf = std::sqrt(1.0 + 2.0 * rho_ * nu_ * zf + nu_ * nu_ * zf * zf);
    Real Jz = std::sqrt(1.0 - 2.0 * rho_ * nu_ * z + nu_ * nu_ * z * z);

    Real xz = std::log((Jz - rho_ + nu_ * z) / (1.0 - rho_)) / nu_;
    Real Bp_B = beta_ / FOmB;
    // Real Bpp_B = beta_ * (2.0 * beta_ - 1.0) / (FOmB * FOmB);
    Real kappa1 = 0.125 * nu_ * nu_ * (2.0 - 3.0 * rho_ * rho_) -
                  0.25 * rho_ * nu_ * alpha_ * Bp_B;
    // Real kappa2 = alpha_ * alpha_ * (0.25 * Bpp_B - 0.375 * Bp_B * Bp_B);
    Real gamma = 1.0 / (2.0 * (1.0 - beta_));
    Real sqrtOmR = std::sqrt(1.0 - rho_ * rho_);
    Real h = 0.5 * beta_ * rho_ / ((1.0 - beta_) * Jmzf * Jmzf) *
             (nu_ * zf * std::log(zf * Jz / zF) +
              (1 + rho_ * nu_ * zf) / sqrtOmR *
                  (std::atan((nu_ * z - rho_) / sqrtOmR) +
                   std::atan(rho_ / sqrtOmR)));

    Real bes = boost::math::cyl_bessel_i<Real, Real>(
        gamma, zF * zf / expiryTime_,
        boost::math::policies::make_policy(
            boost::math::policies::overflow_error<
            boost::math::policies::ignore_error>()/*, // this works, but may be very slow
            boost::math::policies::evaluation_error<
            boost::math::policies::ignore_error>()*/));

    Real res = std::pow(Jz, -1.5) /
               (alpha_ * std::pow(f, beta_) * expiryTime_) *
               std::pow(zf, 1.0 - gamma) * std::pow(zF, gamma) *
               std::exp(-(xz * xz + 2.0 * zF * zf) / (2.0 * expiryTime_) +
                        (h + kappa1 * expiryTime_)) *
               bes;
    return res;
}

namespace detail {

#include "noarbsabrabsprobs.dat"

using namespace boost::assign;

D0Interpolator::D0Interpolator(const Real forward, const Real expiryTime,
                               const Real alpha, const Real beta, const Real nu,
                               const Real rho)
    : forward_(forward), expiryTime_(expiryTime), alpha_(alpha), beta_(beta),
      nu_(nu), rho_(rho), gamma_(1.0 / (2.0 * (1.0 - beta_))) {

    QL_REQUIRE(beta_ <= QL_NOARBSABR_BETA_CUTOFF && beta_ > 0.0,
               "beta must be leq 0.99 and gt 0.0");
    sigmaI_ = alpha_ * std::pow(forward_, beta_ - 1.0);

    tauG_ += 0.25, 0.5, 0.75, 1.0, 1.25, 1.5, 1.75, 2.0, 2.25, 2.5, 2.75, 3.0,
        3.25, 3.5, 3.75, 4.0, 4.25, 4.5, 4.75, 5.0, 5.25, 5.5, 5.75, 6.0, 6.25,
        6.5, 6.75, 7.0, 7.25, 7.5, 7.75, 8.0, 8.25, 8.5, 8.75, 9.0, 9.25, 9.5,
        9.75, 10.0, 10.25, 10.5, 10.75, 11.0, 11.25, 11.5, 11.75, 12.0, 12.25,
        12.5, 12.75, 13.0, 13.25, 13.5, 13.75, 14.0, 14.25, 14.5, 14.75, 15.0,
        15.25, 15.5, 15.75, 16.0, 16.25, 16.5, 16.75, 17.0, 17.25, 17.5, 17.75,
        18.0, 18.25, 18.5, 18.75, 19.0, 19.25, 19.5, 19.75, 20.0, 20.25, 20.5,
        20.75, 21.0, 21.25, 21.5, 21.75, 22.0, 22.25, 22.5, 22.75, 23.0, 23.25,
        23.5, 23.75, 24.0, 24.25, 24.5, 24.75, 25.0, 25.25, 25.5, 25.75, 26.0,
        26.25, 26.5, 26.75, 27.0, 27.25, 27.5, 27.75, 28.0, 28.25, 28.5, 28.75,
        29.0, 29.25, 29.5, 29.75, 30.0;

    sigmaIG_ += 1, 0.8, 0.7, 0.6, 0.5, 0.45, 0.4, 0.35, 0.3, 0.27, 0.24, 0.21,
        0.18, 0.15, 0.125, 0.1, 0.075, 0.05;

    rhoG_ += 0.75, 0.50, 0.25, 0.00, -0.25, -0.50, -0.75;

    nuG_ += 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8;

    betaG_ += 0.01, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9;
}

Real D0Interpolator::operator()() const {

    Size tauInd = std::upper_bound(tauG_.begin(), tauG_.end(), expiryTime_) -
                  tauG_.begin();
    Real expiryTimeTmp = expiryTime_;
    if (tauInd == 0) {
        ++tauInd;
        expiryTimeTmp = tauG_.front();
    }
    QL_REQUIRE(tauInd < tauG_.size(), "tau must be leq " << tauG_.back());
    Real tauL = (expiryTimeTmp - tauG_[tauInd - 1]) /
                (tauG_[tauInd] - tauG_[tauInd - 1]);

    Size sigmaIInd =
        sigmaIG_.size() -
        (std::upper_bound(sigmaIG_.rbegin(), sigmaIG_.rend(), sigmaI_) -
         sigmaIG_.rbegin());
    QL_REQUIRE(sigmaIInd > 0 && sigmaIInd < sigmaIG_.size(),
               "sigmaI = alpha*F^(beta-1) (" << sigmaI_ << ") must be in ["
                                             << sigmaIG_.back() << ","
                                             << sigmaIG_.front() << "]");
    Real sigmaIL = (sigmaI_ - sigmaIG_[sigmaIInd - 1]) /
                   (sigmaIG_[sigmaIInd] - sigmaIG_[sigmaIInd - 1]);

    Size rhoInd =
        rhoG_.size() -
        (std::upper_bound(rhoG_.rbegin(), rhoG_.rend(), rho_) - rhoG_.rbegin());
    if (rhoInd == 0) {
        rhoInd++;
    }
    if (rhoInd == rhoG_.size()) {
        rhoInd--;
    }
    Real rhoL =
        (rho_ - rhoG_[rhoInd - 1]) / (rhoG_[rhoInd] - rhoG_[rhoInd - 1]);

    // for nu = 0 we know phi = 0.5*z_F^2
    Size nuInd = std::upper_bound(nuG_.begin(), nuG_.end(), nu_) - nuG_.begin();
    Real tmpNuG = nuInd > 0 ? nuG_[nuInd - 1] : 0.0;
    QL_REQUIRE(nuInd < nuG_.size(), "nu (" << nu_ << " must be leq "
                                           << nuG_.back());
    Real nuL = (nu_ - tmpNuG) / (nuG_[nuInd] - tmpNuG);

    // for beta = 1 we know phi = 0.0
    Size betaInd =
        std::upper_bound(betaG_.begin(), betaG_.end(), beta_) - betaG_.begin();
    QL_REQUIRE(betaInd > 0, "beta (" << beta_ << " must be geq "
                                     << betaG_.front());
    Real tmpBetaG;
    if (betaInd == betaG_.size())
        tmpBetaG = 1.0;
    else
        tmpBetaG = betaG_[betaInd];
    Real betaL =
        (beta_ - betaG_[betaInd - 1]) / (tmpBetaG - betaG_[betaInd - 1]);

    Real phiRes = 0.0;
    for (int iTau = -1; iTau <= 0; ++iTau) {
        for (int iSigma = -1; iSigma <= 0; ++iSigma) {
            for (int iRho = -1; iRho <= 0; ++iRho) {
                for (int iNu = -1; iNu <= 0; ++iNu) {
                    for (int iBeta = -1; iBeta <= 0; ++iBeta) {
                        Real phiTmp;
                        if (iNu == -1 && nuInd == 0) {
                            phiTmp =
                                0.5 /
                                (sigmaI_ * sigmaI_ * (1.0 - beta_) *
                                 (1.0 - beta_)); // this is 0.5*z_F^2, see above
                        } else {
                            if (iBeta == 0 && betaInd == betaG_.size()) {
                                phiTmp =
                                    phi(QL_NOARBSABR_TINY_PROB); // to get a
                                                                 // reasonable
                                // extrapolation
                            } else {
                                phiTmp = phi(
                                    (Real)sabrabsprob[tauInd + iTau +
                                                      (sigmaIInd + iSigma +
                                                       (rhoInd + iRho +
                                                        (nuInd + iNu +
                                                         ((betaInd + iBeta) *
                                                          nuG_.size())) *
                                                            rhoG_.size()) *
                                                           sigmaIG_.size()) *
                                                          tauG_.size()] /
                                    QL_NOARBSABR_NSIM);
                            }
                        }
                        phiRes += phiTmp * (iTau == -1 ? (1.0 - tauL) : tauL) *
                                  (iSigma == -1 ? (1.0 - sigmaIL) : sigmaIL) *
                                  (iRho == -1 ? (1.0 - rhoL) : rhoL) *
                                  (iNu == -1 ? (1.0 - nuL) : nuL) *
                                  (iBeta == -1 ? (1.0 - betaL) : betaL);
                    }
                }
            }
        }
    }
    Real d0Tmp = d0(phiRes);
    return d0Tmp;
}

Real D0Interpolator::phi(const Real d0) const {
    if (d0 < 1e-12)
        return QL_NOARBSABR_PHITAU_CUTOFF * expiryTime_;
    if (d0 > 1.0 - 1e-12)
        return 0.0;
    Brent b;
    // for \beta = BETA_CUTOFF = 0.99 (the upper bound we assume here) the
    // integrand is
    // below 1e-19 if y is bigger than PHITAU_CUTOFF = 150.0
    Real z = b.solve(
        boost::lambda::bind(&D0Interpolator::d0, this, boost::lambda::_1, d0),
        QL_NOARBSABR_PHI_ACCURACY, 1.0 * expiryTime_, 0.0,
        QL_NOARBSABR_PHITAU_CUTOFF * expiryTime_);
    return z;
}

Real D0Interpolator::d0(const Real phi, const Real target) const {
    Real res = boost::math::gamma_q(gamma_, phi / expiryTime_);
    return res - target;
}

} // namespace detail
} // namespace QuantLib

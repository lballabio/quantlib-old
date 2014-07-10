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

// parameter bounds
// beta [0.01, 0.99]
// expiryTime (0.0, 30.0]
// sigmaI [0.05, 1.0]
// nu (0.0, 0.8]
// rho (-1.0, 1.0)

#include <ql/math/solvers1d/brent.hpp>
#include <ql/experimental/volatility/noarbsabr.hpp>

#include <boost/make_shared.hpp>
#include <boost/math/special_functions/gamma.hpp>
#include <boost/lambda/lambda.hpp>
#include <boost/lambda/bind.hpp>
#include <boost/assign/std/vector.hpp>

#include <iostream>

namespace QuantLib {

namespace detail {

#include "noarbsabrabsprobs.dat"

using namespace boost::assign;

#define QL_NOARBSABR_D0_ACCURACY 1E-6
#define QL_NOARBSABR_PHI_ACCURACY 1E-6
#define QL_NOARBSABR_GL_MAX_ITERATIONS 10000
#define QL_NOARBSABR_BETA_CUTOFF 0.99
#define QL_NOARBSABR_PHITAU_CUTOFF 150.0 // phi(d0)/tau
#define QL_NOARBSABR_NSIM                                                      \
  2500000.0 // number of simulations in tabulated absorption probabilities

D0Interpolator::D0Interpolator(const Real forward, const Real expiryTime,
                               const Real alpha, const Real beta, const Real nu,
                               const Real rho)
    : forward_(forward), expiryTime_(expiryTime), alpha_(alpha), beta_(beta),
      nu_(nu), rho_(rho), gamma_(1.0 / (2.0 * (1.0 - beta_))) {

  QL_REQUIRE(beta_ <= QL_NOARBSABR_BETA_CUTOFF && beta_ > 0.0,
             "beta must be leq 0.99 and gt 0.0");
  sigmaI_ = alpha_ * std::pow(forward_, beta_ - 1.0);
  integrator_ = boost::make_shared<GaussLobattoIntegral>(
      QL_NOARBSABR_GL_MAX_ITERATIONS, QL_NOARBSABR_D0_ACCURACY);

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

  // extrapolate tau flat to the left
  Size tauInd =
      std::upper_bound(tauG_.begin(), tauG_.end(), expiryTime_) - tauG_.begin();
  Real expiryTimeTmp = expiryTime_;
  if (tauInd == 0) {
    ++tauInd;
    expiryTimeTmp = tauG_.front();
  }
  QL_REQUIRE(tauInd < tauG_.size(), "tau must be leq " << tauG_.back());
  Real tauL =
      (expiryTimeTmp - tauG_[tauInd - 1]) / (tauG_[tauInd] - tauG_[tauInd - 1]);

  //std::cout << "tau: " << tauG_[tauInd - 1] << " < " << expiryTimeTmp << " < "
  //          << tauG_[tauInd] << std::endl;
  //std::cout << "tauL: " << tauL << std::endl;

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

  //std::cout << "sigmaI: " << sigmaIG_[sigmaIInd - 1] << " < " << sigmaI_
  //          << " < " << sigmaIG_[sigmaIInd] << std::endl;
  //std::cout << "sigmaIL: " << sigmaIL << std::endl;

  // extrapolate rho flat to both sides ( is that how it is meant to be done ? )
  Size rhoInd =
      rhoG_.size() -
      (std::upper_bound(rhoG_.rbegin(), rhoG_.rend(), rho_) - rhoG_.rbegin());
  Real rhoTmp = rho_;
  if (rhoInd == 0) {
    ++rhoInd;
    rhoTmp = rhoG_.front();
  }
  if (rhoInd == rhoG_.size()) {
    --rhoInd;
    rhoTmp = rhoG_.back();
  }
  Real rhoL =
      (rhoTmp - rhoG_[rhoInd - 1]) / (rhoG_[rhoInd] - rhoG_[rhoInd - 1]);

  //std::cout << "rho: " << rhoG_[rhoInd - 1] << " < " << rhoTmp << " < "
  //          << rhoG_[rhoInd] << std::endl;
  //std::cout << "rhoL: " << rhoL << std::endl;

  // for nu = 0 we know phi = 0.5*z_F^2
  Size nuInd = std::upper_bound(nuG_.begin(), nuG_.end(), nu_) - nuG_.begin();
  Real tmpNuG = nuInd > 0 ? nuG_[nuInd - 1] : 0.0;
  QL_REQUIRE(nuInd < nuG_.size(), "nu (" << nu_ << " must be leq "
                                         << nuG_.back());
  Real nuL = (nu_ - tmpNuG) / (nuG_[nuInd] - tmpNuG);

  //std::cout << "nu: " << tmpNuG << " < " << nu_ << " < " << nuG_[nuInd]
  //          << std::endl;
  //std::cout << "nuL: " << nuL << std::endl;

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
  Real betaL = (beta_ - betaG_[betaInd - 1]) / (tmpBetaG - betaG_[betaInd - 1]);
  //std::cout << "beta: " << betaG_[betaInd - 1] << " < " << beta_ << " < "
  //          << tmpBetaG << std::endl;
  //std::cout << "betaL: " << betaL << std::endl;

  Real phiRes = 0.0;

  for (int iTau = -1; iTau <= 0; ++iTau) {
    for (int iSigma = -1; iSigma <= 0; ++iSigma) {
      for (int iRho = -1; iRho <= 0; ++iRho) {
        for (int iNu = -1; iNu <= 0; ++iNu) {
          for (int iBeta = -1; iBeta <= 0; ++iBeta) {

            Real phiTmp;

            if (iNu == -1 && nuInd == 0) {
              phiTmp = 0.5 / (sigmaI_ * sigmaI_ * (1.0 - beta_) *
                              (1.0 - beta_)); // this is 0.5*z_F^2, see above
            } else {
              if (iBeta == 0 && betaInd == betaG_.size()) {
                phiTmp = phi(0.00001); // to get a reasonable extrapolation
                //std::cout << "ibeta=0, beta2=1 => phitmp = " << phiTmp;
              } else {
                //std::cout << "tau=" << iTau << " sigma=" << iSigma
                //           << " rho=" << iRho << " nu=" << iNu
                //           << " beta=" << iBeta << std::endl;
                // std::cout << "Indices: tau=" << tauInd << " sigmaI "
                //           << sigmaIInd << " rho " << rhoInd << " nu " << nuInd
                //           << " beta " << betaInd << " total "
                //           << tauInd + iTau +
                //          (sigmaIInd + iSigma +
                //           (rhoInd + iRho +
                //            (nuInd + iNu +
                //             ((betaInd + iBeta) * nuG_.size())) 
                //            * rhoG_.size())
                //           * sigmaIG_.size()) 
                //          * tauG_.size()
                //           << std::endl;
                // std::cout << "retrieving absN="
                //           << sabrabsprob[tauInd + iTau +
                //          (sigmaIInd + iSigma +
                //           (rhoInd + iRho +
                //            (nuInd + iNu +
                //             ((betaInd + iBeta) * nuG_.size())) 
                //            * rhoG_.size())
                //           * sigmaIG_.size()) 
                //          * tauG_.size()] << std::endl;
                phiTmp = phi(
                    (Real)sabrabsprob
                        [tauInd + iTau +
                         (sigmaIInd + iSigma +
                          (rhoInd + iRho +
                           (nuInd + iNu +
                            ((betaInd + iBeta) * nuG_.size())) 
                           * rhoG_.size())
                          * sigmaIG_.size()) 
                         * tauG_.size()] /
                    QL_NOARBSABR_NSIM);
                //std::cout << "phitmp=" << phiTmp << std::endl;
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

  // transform back to d0 space
  Real d0Tmp = d0(phiRes);

  return d0Tmp;
}

Real D0Interpolator::integrand(const Real y) const {
  QL_FAIL("this is not needed ...");
  return std::exp(-y) * std::pow(y, gamma_ - 1.0) / boost::math::tgamma(gamma_);
}

Real D0Interpolator::phi(const Real d0) const {
  if (d0 < 1e-12)
    return QL_NOARBSABR_PHITAU_CUTOFF * expiryTime_;
  if (d0 > 1.0 - 1e-12)
    return 0.0;
  // std::cout << "*** solving for d0=" << d0 << std::endl;
  Brent b;
  Real z = b.solve(
      boost::lambda::bind(&D0Interpolator::d0, this, boost::lambda::_1, d0),
      QL_NOARBSABR_PHI_ACCURACY, 1.0, 0.0, QL_NOARBSABR_PHITAU_CUTOFF * expiryTime_);
  return z;
}

Real D0Interpolator::d0(const Real phi, const Real target) const {
  // for \beta = BETA_CUTOFF = 0.99 (the upper bound we assume here) the
  // integrand is
  // below 1e-19 if y is bigger than PHITAU_CUTOFF = 150.0
  // Real res =
  //     phi < 1e-12 ? 1.0 : integrator_->operator()(
  //                             boost::lambda::bind(&D0Interpolator::integrand,
  //                                                 this, boost::lambda::_1),
  //                             phi / expiryTime_, QL_NOARBSABR_PHITAU_CUTOFF);
  //std::cout << "--- d0(" << phi << ") = " << res << std::endl;
  Real res = boost::math::gamma_q(gamma_, phi / expiryTime_);
  return res - target;
}

} // namespace detail

} // namespace QuantLib

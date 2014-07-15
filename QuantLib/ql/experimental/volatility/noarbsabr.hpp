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

/*! \file noarbsabr.hpp
    \brief No-arbitrage SABR

    Reference: Paul Doust, No-arbitrage SABR,
               The Journal of Computational Finance (3–31)
               Volume 15/Number 3, Spring 2012

    The parameters are bounded as follows

    beta [0.01, 0.99] // beta > 0.8 may be already a numerical problem ... to be clarified ...
    expiryTime (0.0, 30.0]
    sigmaI = alpha*forward^(beta-1) [0.05, 1.0]
    nu (0.0, 0.8] // bound nu away from zero  !! to make analytical formulas safe here ...
    rho [-1.0, 1.0] // bound away from -1, 1 !!

    As suggested in the paper, d0 is interpolated (linearly)
    in phi space. For beta > 0.9 phi is extrapolated to a
    value corresponding to d0 = TINY_PROB = 1E-5 at beta = 1.
    For tau < 0.25 phi is extrapolated flat.
    For rho outside [-0.75, 0.75] phi is extrapolated linearly.
*/

#ifndef quantlib_noarb_sabr
#define quantlib_noarb_sabr

#include <ql/qldefines.hpp>
#include <ql/types.hpp>
#include <ql/math/integrals/gausslobattointegral.hpp>

namespace QuantLib {

class NoArbSabr {

  public:
    NoArbSabr(const Real expiryTime, const Real forward, const Real alpha,
              const Real beta, const Real nu, const Real rho);

    Real optionPrice(const Real strike) const;
    Real digitalOptionPrice(const Real strike) const; 
    Real density(const Real strike) const { return p(strike,true) * (1-absProb_) / numericalIntegralOverP_; }

    Real forward() const { return forward_; }
    Real expiryTime() const { return expiryTime_; }
    Real alpha() const { return alpha_; }
    Real beta() const { return beta_; }
    Real nu() const { return nu_; }
    Real rho() const { return rho_; }

    Real absorptionProbability() const { return absProb_; }

    //private:
    Real p(const Real f, const bool checkNumericalLimits = true) const;
    Real forwardError(const Real forward) const;
    Real integrand(const Real strike, const Real f) const;
    const Real expiryTime_, externalForward_;
    const Real alpha_, beta_, nu_, rho_;
    Real absProb_, fmin_, fmax_;
    mutable Real forward_, numericalIntegralOverP_;
    boost::shared_ptr<GaussLobattoIntegral> integrator_;
};

namespace detail {

class D0Interpolator {
  public:
    D0Interpolator(const Real forward, const Real expiryTime, const Real alpha,
                   const Real beta, const Real nu, const Real rho);
    Real operator()() const;

  private:
    Real phi(const Real d0) const;
    Real d0(const Real phi, const Real target = 0.0) const;
    const Real forward_, expiryTime_, alpha_, beta_, nu_, rho_, gamma_;
    Real sigmaI_;
    std::vector<Real> tauG_, sigmaIG_, rhoG_, nuG_, betaG_;
};

} // namespace detail
} // namespace QuantLib

#endif

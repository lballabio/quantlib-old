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
               The Journal of Computational Finance (3–31) Volume 15/Number 3,
   Spring 2012
*/

#ifndef quantlib_noarb_sabr
#define quantlib_noarb_sabr

#include <ql/qldefines.hpp>
#include <ql/types.hpp>
#include <ql/math/integrals/gausslobattointegral.hpp>

namespace QuantLib {

namespace detail {

class D0Interpolator {
public:
  D0Interpolator(const Real forward, const Real expiryTime, const Real alpha,
                 const Real beta, const Real nu, const Real rho);
  Real operator()() const;

    //private:
  Real integrand(const Real y) const;
  Real phi(const Real d0) const;
  Real d0(const Real phi, const Real target = 0.0) const;
  boost::shared_ptr<GaussLobattoIntegral> integrator_;
  const Real forward_, expiryTime_, alpha_, beta_, nu_, rho_, gamma_;
  Real sigmaI_;
  std::vector<Real> tauG_, sigmaIG_, rhoG_, nuG_, betaG_;
};

} // namespace detail
} // namespace QuantLib

#endif

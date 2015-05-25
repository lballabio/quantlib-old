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

/*! \file betaeta.hpp
    \brief beta eta model core computations
*/

#ifndef quantlib_betaeta_hpp
#define quantlib_betaeta_hpp

#include <ql/types.hpp>
#include <ql/utilities/null.hpp>

#include <vector>

namespace QuantLib {

/*! cf. Hagan, Woodward: Markov interest rate models,
    Applied Mathematical Finance 6, 233–260 (1999)
*/

class BetaEta {
  public:
    BetaEta(const std::vector<Real> &times, const std::vector<Real> &alpha,
            const std::vector<Real> &lambda, const Real &beta, const Real &eta);

    // transition density
    const Real p(const Time t0, const Real x0, const Real t, const Real x);
    // singular term for y0=0 (x0=-1/beta) 
    // and 1 > eta >= 0.5, otherwise 0 is returned
    const Real singularTerm_y0_0(const Time t0, const Real x0, const Time t);

  private:
    const Real tau(const Time t) const;
    const Real y(const Real x) const;
    const Real dydx(const Real y) const;

    const int lowerIndex(const Time t) const;
    const int upperIndex(const Time t) const;

    const Real time2(const Size index) const;
    const Real cappedTime(const Size index,
                          const Real cap = Null<Real>()) const;
    const Real flooredTime(const Size index,
                           const Real floor = Null<Real>()) const;

    const Real alpha(const Size index) const;
    const Real lambda(const Size index) const;

    const std::vector<Real> &times_, &alpha_, &lambda_;
    const Real &beta_, &eta_;
};

} // namespace QuantLib

#endif

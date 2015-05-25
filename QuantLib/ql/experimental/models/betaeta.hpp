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
#include <ql/math/comparison.hpp>
#include <ql/math/integrals/gausslobattointegral.hpp>

#include <vector>

namespace QuantLib {

/*! cf. Hagan, Woodward: Markov interest rate models,
    Applied Mathematical Finance 6, 233–260 (1999)
*/

class BetaEta {
  public:
    BetaEta(const std::vector<Real> &times, const std::vector<Real> &alpha,
            const std::vector<Real> &lambda, const Real &beta, const Real &eta);

    // M(t0,x0;t)
    const Real M(const Real t0, const Real x0, const Real t) const;

    // transition density
    const Real p(const Time t0, const Real x0, const Real t,
                 const Real x) const;

    // singular term for y=0 (x=-1/beta)
    // and 1 > eta >= 0.5, otherwise 0 is returned
    const Real singularTerm_y_0(const Time t0, const Real x0,
                                const Time t) const;

  private:
    const Real tau(const Time t) const;
    const Real tau(const Time t0, const Time t) const;
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

    boost::shared_ptr<GaussLobattoIntegral> integrator_;

    class mIntegrand;
    friend class mIntegrand;
};

// implementation

inline const Real BetaEta::tau(const Real t) const { return tau(0.0, t); }

inline const Real BetaEta::tau(const Real t0, const Real t) const {
    Real res = 0.0;
    for (int i = lowerIndex(t0); i < upperIndex(t); ++i) {
        res +=
            alpha_[i] * alpha_[i] * (cappedTime(i + 1, t) - flooredTime(i, t0));
    }
    return res;
}

inline const Real BetaEta::y(const Real x) const {
    // for eta = 1 y is only well defined if x > -1/beta
    // it is not the responsibility of the code here to ensure this
    return close(eta_, 1.0) ? std::log(1.0 + beta_ * x) / beta_
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

#endif

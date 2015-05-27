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

/*! \file betaetacore.hpp
    \brief beta eta model core computations
*/

#ifndef quantlib_betaeta_hpp
#define quantlib_betaeta_hpp

#include <ql/types.hpp>
#include <ql/math/array.hpp>
#include <ql/utilities/null.hpp>
#include <ql/math/comparison.hpp>
#include <ql/math/integrals/gausslobattointegral.hpp>

#include <vector>

namespace QuantLib {

/*! cf. Hagan, Woodward: Markov interest rate models,
    Applied Mathematical Finance 6, 233–260 (1999)
*/

class BetaEtaCore {
  public:
    /*! We assume a piecewise constant reversion \kappa and
        set \lambda(t) := (1-exp(-\kappa*t))/\kappa */
    BetaEtaCore(const Array &times, const Array &alpha, const Array &kappa,
                const Real &beta, const Real &eta);

    // M(t0,x0;t)
    const Real M(const Real t0, const Real x0, const Real t) const;

    // transition density
    const Real p(const Time t0, const Real x0, const Real t,
                 const Real x) const;

    // singular term for y=0 (x=-1/beta)
    // and 1 > eta >= 0.5, otherwise 0 is returned
    const Real singularTerm_y_0(const Time t0, const Real x0,
                                const Time t) const;

    // lambda(t)
    const Real lambda(const Time t) const;

    // tau(0,t) and tau(t0,t)
    const Real tau(const Time t) const;
    const Real tau(const Time t0, const Time t) const;

    // integrator
    boost::shared_ptr<Integrator> integrator() const { return integrator_; }

  private:
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
    const Real kappa(const Size index) const;

    const Array &times_, &alpha_, &kappa_;
    const Real &beta_, &eta_;

    boost::shared_ptr<GaussLobattoIntegral> integrator_;

    class mIntegrand;
    friend class mIntegrand;
};

// implementation

inline const Real BetaEtaCore::tau(const Real t) const { return tau(0.0, t); }

inline const Real BetaEtaCore::tau(const Real t0, const Real t) const {
    Real res = 0.0;
    for (int i = lowerIndex(t0); i < upperIndex(t); ++i) {
        res +=
            alpha_[i] * alpha_[i] * (cappedTime(i + 1, t) - flooredTime(i, t0));
    }
    return res;
}

inline const Real BetaEtaCore::y(const Real x) const {
    // for eta = 1 y is only well defined if x > -1/beta
    // it is not the responsibility of the code here to ensure this
    return close(eta_, 1.0) ? std::log(1.0 + beta_ * x) / beta_
                            : std::pow(std::fabs(1 + beta_ * x), 1.0 - eta_) /
                                  (beta_ * (1.0 - eta_));
}

inline const Real BetaEtaCore::dydx(const Real y) const {
    return close(y, 1.0)
               ? std::exp(-beta_ * y)
               : std::pow((1.0 - eta_) * beta_ * y, -eta_ / (1.0 - eta_));
}

inline const int BetaEtaCore::lowerIndex(const Time t) const {
    return static_cast<int>(std::upper_bound(times_.begin(), times_.end(), t) -
                            times_.begin());
}

inline const int BetaEtaCore::upperIndex(const Time t) const {
    if (t < QL_MIN_POSITIVE_REAL)
        return 0;
    return static_cast<int>(std::upper_bound(times_.begin(), times_.end(),
                                             t - QL_MIN_POSITIVE_REAL) -
                            times_.begin()) +
           1;
}

inline const Real BetaEtaCore::cappedTime(const Size index,
                                          const Real cap) const {
    return cap != Null<Real>() ? std::min(cap, time2(index)) : time2(index);
}

inline const Real BetaEtaCore::flooredTime(const Size index,
                                           const Real floor) const {
    return floor != Null<Real>() ? std::max(floor, time2(index)) : time2(index);
}

inline const Real BetaEtaCore::time2(const Size index) const {
    if (index == 0)
        return 0.0;
    if (index > times_.size())
        return QL_MAX_REAL;
    return times_[index - 1];
}

inline const Real BetaEtaCore::alpha(const Size index) const {
    if (index >= alpha_.size())
        return alpha_.back();
    return alpha_[index];
}

inline const Real BetaEtaCore::kappa(const Size index) const {
    if (index >= kappa_.size())
        return kappa_.back();
    return kappa_[index];
}

inline const Real BetaEtaCore::lambda(const Time t) const {
    Real kappa = this->kappa(lowerIndex(t));
    return (1.0 - exp(-kappa * t)) / kappa;
}

} // namespace QuantLib

#endif

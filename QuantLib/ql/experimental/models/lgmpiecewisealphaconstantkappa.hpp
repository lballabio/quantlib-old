/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2015 Peter Caspers

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

/*! \file lgmpiecewisealphaconstantkappa.hpp
    \brief piecewise alpha, constant kappa
*/

#ifndef quantlib_lgm_piecewisealphaconstantkappa_hpp
#define quantlib_lgm_piecewisealphaconstantkappa_hpp

#include <ql/experimental/models/lgmparametrization.hpp>
#include <vector>

namespace QuantLib {

class LgmPiecewiseAlphaConstantKappa
    : public LgmParametrization<LgmPiecewiseAlphaConstantKappa> {
  public:
    LgmPiecewiseAlphaConstantKappa(const std::vector<Real> &times,
                                   const std::vector<Real> &alphas,
                                   const Real &kappa);

    const Real zetaImpl(const Time t) const;
    const Real alphaImpl(const Time t) const;
    const Real HImpl(const Time t) const;
    const Real HprimeImpl(const Time t) const;
    const Real Hprime2Impl(const Time t) const;
    const void updateImpl() const;

  private:
    const std::vector<Real> &times_, &alphas_;
    const Real &kappa_;
    mutable std::vector<Real> zetas_;
};

// inline definitions

inline const void LgmPiecewiseAlphaConstantKappa::updateImpl() const {
    Real sum = 0.0;
    for (Size i = 0; i < times_.size(); ++i) {
        sum += alphas_[i] * alphas_[i] *
               (times_[i] - (i == 0 ? 0.0 : times_[i - 1]));
        zetas_[i] = sum;
    }
}

inline const Real
LgmPiecewiseAlphaConstantKappa::alphaImpl(const Time t) const {
    if (t < 0.0)
        return 0.0;
    return alphas_[std::min<Size>(
        std::upper_bound(times_.begin(), times_.end(), t) - times_.begin(),
        alphas_.size() - 1)];
}

inline const Real LgmPiecewiseAlphaConstantKappa::zetaImpl(const Time t) const {
    if (t < 0.0)
        return 0.0;
    Size i = std::min<Size>(std::upper_bound(times_.begin(), times_.end(), t) -
                                times_.begin(),
                            times_.size() - 1);
    Real res = 0.0;
    if (i >= 1)
        res += zetas_[std::min(i - 1, zetas_.size() - 1)];
    res += alphas_[i] * alphas_[i] * (t - (i == 0 ? 0.0 : times_[i - 1]));
    return res;
}

inline const Real LgmPiecewiseAlphaConstantKappa::HImpl(const Time t) const {
    // we avoid a kappa near zero
    Real tmp;
    if (std::fabs(kappa_) < 1E-4) {
        if (kappa_ > 0.0) {
            tmp = 1E-4;
        } else {
            tmp = -1E-4;
        }
    } else {
        tmp = kappa_;
    }

    return (1.0 - std::exp(-tmp * t)) / kappa_;
}

inline const Real
LgmPiecewiseAlphaConstantKappa::HprimeImpl(const Time t) const {
    return std::exp(-kappa_ * t);
}

inline const Real
LgmPiecewiseAlphaConstantKappa::Hprime2Impl(const Time t) const {
    return -kappa_ * std::exp(-kappa_ * t);
}

} // namespace QuantLib

#endif

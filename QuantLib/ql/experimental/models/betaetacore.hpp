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

/*! \file betaetacore.hpp
    \brief beta eta model core computations
*/

#ifndef quantlib_betaeta_hpp
#define quantlib_betaeta_hpp

#include <ql/types.hpp>
#include <ql/math/array.hpp>
#include <ql/utilities/null.hpp>
#include <ql/math/comparison.hpp>
#include <ql/math/integrals/integral.hpp>
#include <ql/math/integrals/gaussianquadratures.hpp>
#include <ql/math/interpolations/interpolation2d.hpp>

#include <ostream>

#include <vector>

namespace QuantLib {

/* see betaeta.hpp for references and more comments */

/*! warning: the parameters in the constructor are linked via references
    to their sources to be able to reflect changes immediately, so you
    should not initialize an instance of this class with numerical constants
    or variables with a lifetime shorter than that of this instance. */

/*! TODO very small probabilities for y=0 sometimes cause numerical
    instabilities when multiplied by huge terms (e.g. to compute M),
    therefore we set them to zero if smaller than 1E-6 for the time
    being; to be revisited */

class BetaEtaCore {
  public:
    BetaEtaCore(const Array &times, const Array &alpha, const Array &kappa,
                const Real &beta, const Real &eta);

    // normally tabulation should be used for efficiency reasons
    // it can be switched off for validation purposes though
    const Real M(const Real t0, const Real x0, const Real t,
                 const bool useTabulation = true) const;

    const Real p(const Time t0, const Real x0, const Real t,
                 const Real x) const;

    // probability for y being zero (or x = -1 / beta)
    const Real prob_y_0(const Time t0, const Real x0, const Time t,
                        const bool useTabulation = true) const;

    const Real lambda(const Time t) const;

    const Real tau(const Time t) const;
    const Real tau(const Time t0, const Time t) const;

    // parameter inspectors
    const Real beta() const { return beta_; }
    const Real eta() const { return eta_; }

    // M in transformed variables, mainly there for tabulation purposes
    const Real M(const Real u0, const Real Su) const;

    // prob_y_0 in transformed variables (warning: no cutoff is applied)
    const Real prob_y_0(const Real v, const Real y0) const;

  private:
    const Real M_eta_1(const Real t0, const Real x0, const Real t) const;
    const Real M_eta_05(const Real t0, const Real x0, const Real t) const;
    const Real M_tabulated(const Real t0, const Real x0, const Real t) const;

    // density (without singular term)
    const Real p_y(const Real v, const Real y0, const Real y,
                   const Real eta) const;
    // density (without singular term, without dy/dx)
    const Real p_y_core0(const Real v, const Real y0, const Real y,
                         const Real eta) const;
    // density (without singular term, without dy/dx, but including y^{-2\nu+1})
    const Real p_y_core1(const Real v, const Real y0, const Real y,
                         const Real eta) const;

    // warning: no cutoff is applied
    const Real prob_y_0_tabulated(const Real v, const Real y0) const;

    const Real y(const Real x, const Real eta) const;
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

    boost::shared_ptr<Integrator> integrator_, integrator2_;
    boost::shared_ptr<GaussianQuadrature> ghIntegrator_;
    boost::shared_ptr<Integrator> preIntegrator_, preIntegrator2_;

    // tabulation data
    Size etaSize_, uSize_, SuSize_, vSize_, y0Size_;
    std::vector<Real> eta_pre_, u_pre_, Su_pre_, v_pre_, y0_pre_;
    std::vector<boost::shared_ptr<Matrix> > M_datasets_, p_datasets_;
    std::vector<boost::shared_ptr<Interpolation2D> > M_surfaces_, p_surfaces_;

    // avoid lambda expressions for compiler compatibility
    class mIntegrand1;
    friend class mIntegrand1;
    class pIntegrand1;
    friend class pIntegrand1;
    class mIntegrand2;
    friend class mIntegrand2;
    class pIntegrand2;
    friend class pIntegrand2;
    class mIntegrand3;
    friend class mIntegrand3;

    // constants
    const Size ghPoints_;
    const Real prob_y_0_cutoff;
    const Real kappa_cutoff;
};

namespace detail {

// declarations for tabulated data

const unsigned int eta_pre_size = 99, u_pre_size = 100, Su_pre_size = 100;
const unsigned int v_pre_size = 50, y0_pre_size = 50;
extern "C" const double eta_pre[];
extern "C" const double u_pre[];
extern "C" const double Su_pre[];
extern "C" const double v_pre[];
extern "C" const double y0_pre[];
extern "C" const double M_pre[eta_pre_size][u_pre_size][Su_pre_size];
extern "C" const double p_pre[eta_pre_size][v_pre_size][y0_pre_size];

// tabulate values M(eta, u, Su) or prob_y_0(v,y0)
// note that the parameters for v and y0 and taken
// from Su and u; and the eta parameters
// (min, max, size, concentrating point, density)
// must be identical for M and p tabulation

enum betaeta_tabulation_type {
    Cpp_M,      // cpp tabulation of M
    Cpp_p,      // cpp tabulation of prob_y_0
    GnuplotEUV, // gnuplot file of M, order eta-u0-Su
    GnuplotUEV, // gnuplot file of M, order u0-eta-Su
    GnuplotVEU, // gnuplot file of M, order Su-eta-u0
    GnuplotP    // gnuplot file of prob_y_0
};

const void
betaeta_tabulate(betaeta_tabulation_type type, std::ostream &out,
                 const Real eta_min, const Real eta_max, const Real u0_min,
                 const Real u0_max, const Real Su_min, const Real Su_max,
                 const Size u_size, const Size Su_size, const Size eta_size,
                 const Real c_u, const Real density_u, const Real c_Su,
                 const Real density_Su, const Real c_e, const Real density_e);

// heuristic to determine reasonable integration domains;
// given a function f with f(c) > t, f continuous and with
// limit zero for x to plus and minus infinity we look for
// a0 < a < c < b < b0 with t2 < f(a) < t, t2 < f(b) < t

template <class F>
std::pair<Real, Real>
domain(const F &f, const Real c, const Real t, const Real t2,
       const Real accuracy = 1E-6, const Real step = 1.1,
       const Real a0 = -QL_MAX_REAL, const Real b0 = QL_MAX_REAL) {

    Real la, lb;
    la = c;
    lb = c;
    while (f(la) > t && la > a0) {
        if (fabs(la) < 1.0)
            la -= step - 1.0;
        else
            la = la > 0 ? la / step : la * step;
        la = std::max(std::min(la, b0), a0);
    }
    while (f(lb) > t && lb < b0) {
        if (fabs(lb) < 1.0)
            lb += step - 1.0;
        else
            lb = lb > 0 ? lb * step : lb / step;
        lb = std::max(std::min(lb, b0), a0);
    }
    Real tmpa = la;
    Real tmpb = c;
    Real m = tmpa;
    if ((f(tmpa) - t) * (f(tmpb) - t) < 0.0) {
        while (std::fabs(tmpa - tmpb) > accuracy && f(tmpa) < t2) {
            m = (tmpa + tmpb) / 2.0;
            if ((f(tmpa) - t) * (f(m) - t) < 0.0)
                tmpb = m;
            else
                tmpa = m;
        }
    }
    Real a = m;
    tmpa = c;
    tmpb = lb;
    m = tmpb;
    if ((f(tmpa) - t) * (f(tmpb) - t) < 0.0) {
        while (std::fabs(tmpa - tmpb) > accuracy && f(tmpb) < t2) {
            m = (tmpa + tmpb) / 2.0;
            if ((f(tmpa) - t) * (f(m) - t) < 0.0)
                tmpb = m;
            else
                tmpa = m;
        }
    }
    Real b = m;
    return std::make_pair(a, b);
}

} // namespace detail

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

inline const Real BetaEtaCore::y(const Real x, const Real eta) const {
    QL_REQUIRE(eta < 1.0 || x > -1.0 / beta_,
               "for eta = 1, x must be greater than - 1 / beta");
    return close(eta, 1.0) ? std::log(1.0 + beta_ * x) / beta_
                           : std::pow(std::fabs(1 + beta_ * x), 1.0 - eta) /
                                 (beta_ * (1.0 - eta));
}

inline const Real BetaEtaCore::dydx(const Real y) const {
    return close(eta_, 1.0)
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
    // the model collapses with kappa near zero,
    // so we just keep it away a bit - this is
    // not a numerical issue, but inherent in the
    // model construction
    if (std::fabs(kappa) < kappa_cutoff)
        kappa = kappa > 0.0 ? kappa_cutoff : -kappa_cutoff;
    return (1.0 - exp(-kappa * t)) / kappa;
}

} // namespace QuantLib

#endif

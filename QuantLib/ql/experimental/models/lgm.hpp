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

/*! \file lgm.hpp
    \brief LGM model base class
           Reference: Patrick S. Hagan, Evaluating and hedging exotic swap
                      instruments via LGM
*/

#ifndef quantlib_lgm_hpp
#define quantlib_lgm_hpp

#include <ql/models/shortrate/onefactormodels/gaussian1dmodel.hpp>
#include <ql/experimental/models/lgmparametrization.hpp>

namespace QuantLib {

template <class Impl> class Lgm : public Gaussian1dModel {
  public:
    const LgmParametrization<Impl> &parametrization() {
        return parametrization_;
    }

  protected:
    Lgm(const Handle<YieldTermStructure> &yts,
        const LgmParametrization<Impl> &parametrization);
    const Real numeraireImpl(const Time t, const Real y,
                             const Handle<YieldTermStructure> &yts) const;
    const Real zerobondImpl(const Time T, const Time t, const Real y,
                            const Handle<YieldTermStructure> &yts,
                            const bool adjusted) const;

  private:
    const LgmParametrization<Impl> &parametrization_;
};

// inline

template <class Impl>
inline const Real
Lgm<Impl>::numeraireImpl(const Time t, const Real y,
                         const Handle<YieldTermStructure> &yts) const {
    calculate();
    Handle<YieldTermStructure> tmp = yts.empty() ? this->termStructure() : yts;
    Real x = y * stateProcess()->stdDeviation(0.0, 0.0, t) +
             stateProcess()->expectation(0.0, 0.0, t);
    Real h = parametrization_.H(t);
    Real z = parametrization_.zeta(t);
    return 1.0 / tmp->discount(t) * std::exp(h * x + 0.5 * h * h * z);
}

template <class Impl>
inline const Real Lgm<Impl>::zerobondImpl(const Time T, const Time t,
                                          const Real y,
                                          const Handle<YieldTermStructure> &yts,
                                          const bool) const {
    calculate();
    // for reduced zero bonds this could be optimized
    Handle<YieldTermStructure> tmp = yts.empty() ? termStructure() : yts;
    Real x = y * stateProcess()->stdDeviation(0.0, 0.0, t) +
             stateProcess()->expectation(0.0, 0.0, t);
    Real ht = parametrization_.H(t);
    Real hT = parametrization_.H(T);
    Real z = parametrization_.zeta(t);
    return tmp->discount(T) / tmp->discount(t) *
           std::exp((ht - hT) * x + 0.5 * (ht * ht - hT * hT) * z);
}

// implementation

template <class Impl>
Lgm<Impl>::Lgm(const Handle<YieldTermStructure> &yts,
               const LgmParametrization<Impl> &parametrization)
    : Gaussian1dModel(yts), parametrization_(parametrization) {}

} // namespace QuantLib

#endif

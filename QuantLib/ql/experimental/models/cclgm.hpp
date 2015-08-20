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

/*! \file cclgm.hpp
    \brief multicurrency lgm model with generic parametrization
*/

#ifndef quantlib_multicurrency_lgm_hpp
#define quantlib_multicurrency_lgm_hpp

#include <ql/experimental/models/lgm.hpp>

namespace QuantLib {

template <class Impl, class ImplFx, class ImplLgm>
class CcLgm : public CalibratedModel {
  public:
    boost::shared_ptr<detail::CcLgmParametrization<Impl, ImplFx, ImplLgm> >
    parametrization() {
        return parametrization_;
    }

    const boost::shared_ptr<Lgm<ImplLgm> > model(Size i) const {
        return models_[i];
    }
    const Size n() const { return n_; }

    // calibration constraints
    Disposable<std::vector<bool> > MoveFXVolatility(Size i, Size step) {
        QL_REQUIRE(i < n_, "fx index (" << i << ") out of range (0..." << n_ - 1
                                        << ")");
        QL_REQUIRE(step <= fxVolStepTimes_.size(),
                   "fx volatility step (" << step << ") out of range (0..."
                                          << fxVolStepTimes_.size() << ")");
        std::vector<bool> res(n_ * (fxVolStepTimes_.size() + 1), true);
        res[n_ * i + step] = false;
        return res;
    }

    // calibrate the stepwise fx volatilities dom - currency(i)
    void calibrateFxVolatilitiesIterative(
        Size i,
        const std::vector<boost::shared_ptr<CalibrationHelper> > &helpers,
        OptimizationMethod &method, const EndCriteria &endCriteria,
        const Constraint &constraint = Constraint(),
        const std::vector<Real> &weights = std::vector<Real>()) {
        for (Size j = 0; j < helpers.size(); ++i) {
            std::vector<boost::shared_ptr<CalibrationHelper> > h(1, helpers[i]);
            calibrate(h, method, endCriteria, constraint, weights,
                      MoveCurrencyFXVolatility(i, j));
        }
    }

    boost::shared_ptr<StochasticProcess> stateProcess() const {
        return process_;
    }

  protected:
    CcLgm(const std::vector<boost::shared_ptr<Lgm<ImplLgm> > > &models);

    void generateArguments() {
        boost::static_pointer_cast<CcLgmProcess<Impl, ImplFx, ImplLgm> >(
            stateProcess()->flushCache());
        notifyObservers();
    }

  protected:
    setParametrization(const bost::shared_ptr<detail::CcLgmParametrization<
                           Impl, ImplFx, ImplLgm> > parametrization) {
        parametrization_ = parametrization;
        QL_REQUIRE(parametrization.n() == n_,
                   "parametrization's dimension (n="
                       << parametrization.n()
                       << ") is inconsistent to the number of models ("
                       << models_.size() << "=n+1)");
    }

  private:
    Size n_;
    const std::vector<boost::shared_ptr<Gsr> > models_;
    boost::shared_ptr<StochasticProcess> process_;
};

// implementation

template <class Impl, ImplFx, ImplLgm>
CcLgm<Impl, ImplFx, ImplLgm>::CcLgm(
    const std::vector<boost::shared_ptr<Lgm<ImplLgm> > > &models)
    : n_(models_.size() - 1) {
    QL_REQUIRE(models.size() >= 2, "at least two models ("
                                       << models.size() << ") must be given");
}

} // namespace QuantLib

#endif

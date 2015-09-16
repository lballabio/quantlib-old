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

#include <ql/experimental/models/qg1dlinearmodel.hpp>

namespace QuantLib {

#define PIECEWISEFCT(Y)                                                        \
    if (t < 0.0)                                                               \
        return 0.0;                                                            \
    return kappa_[std::min<Size>(                                              \
        std::upper_bound(volsteptimes_.begin(), volsteptimes_.end(), t) -      \
            volsteptimes_.begin(),                                             \
        Y.size() - 1)];

Qg1dLinearModel::Qg1dLinearModel(const Handle<YieldTermStructure> &yts,
                                 const std::vector<Date> stepDates,
                                 const std::vector<Real> &lambda,
                                 const std::vector<Real> &alpha,
                                 const std::vector<Real> &beta,
                                 const std::vector<Real> &kappa)
    : TermStructureConsistentModel(yts), stepDates_(stepDates), lambda_(lambda),
      alpha_(alpha), beta_(beta), kappa_(kappa) {
    QL_REQUIRE(!yts.empty(), "yield term structure handle is empty");
    initialize();
}

void Qg1dLinearModel::update() { updateTimes(); }

void Qg1dLinearModel::initialize() {
    registerWith(termStructure());
    volsteptimesArray_ = Array(stepDates_.size());
    updateTimes();
}

void Qg1dLinearModel::updateTimes() const {
    volsteptimes_.clear();
    int j = 0;
    for (std::vector<Date>::const_iterator i = stepDates_.begin();
         i != stepDates_.end(); ++i, ++j) {
        volsteptimes_.push_back(termStructure()->timeFromReference(*i));
        volsteptimesArray_[j] = volsteptimes_[j];
        if (j == 0)
            QL_REQUIRE(volsteptimes_[0] > 0.0, "volsteptimes must be positive ("
                                                   << volsteptimes_[0] << ")");
        else
            QL_REQUIRE(volsteptimes_[j] > volsteptimes_[j - 1],
                       "volsteptimes must be strictly increasing ("
                           << volsteptimes_[j - 1] << "@" << (j - 1) << ", "
                           << volsteptimes_[j] << "@" << j << ")");
    }
}

Real Qg1dLinearModel::g(const Real t, const Real x, const Real y) const {
    return lambda(t) * (alpha(t) + beta(t) * x) / h(t);
}

Real Qg1dLinearModel::kappa(const Real t) const { PIECEWISEFCT(kappa_); }

Real Qg1dLinearModel::lambda(const Real t) const { PIECEWISEFCT(lambda_); }

Real Qg1dLinearModel::alpha(const Real t) const { PIECEWISEFCT(alpha_); }

Real Qg1dLinearModel::beta(const Real t) const { PIECEWISEFCT(beta_); }

} // namespace QuantLib

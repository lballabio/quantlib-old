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
#include <ql/experimental/math/piecewiseintegral.hpp>
#include <ql/experimental/math/piecewisefunction.hpp>
#include <ql/math/integrals/segmentintegral.hpp>
#include <ql/math/integrals/gausslobattointegral.hpp>

namespace QuantLib {

Qg1dLinearModel::Qg1dLinearModel(const Handle<YieldTermStructure> &yts,
                                 const std::vector<Date> stepDates,
                                 const std::vector<Real> &lambda,
                                 const std::vector<Real> &alpha,
                                 const std::vector<Real> &beta,
                                 const std::vector<Real> &kappa)
    : Qg1dLocalVolModel(yts), stepDates_(stepDates), lambda_(lambda),
      alpha_(alpha), beta_(beta), kappa_(kappa) {
    QL_REQUIRE(!yts.empty(), "yield term structure handle is empty");
    initialize();
}

void Qg1dLinearModel::initialize() {
    registerWith(termStructure());
    volsteptimesArray_ = Array(stepDates_.size());
    updateTimes();
    updateIntKappa();
    boost::shared_ptr<Integrator> baseIntegrator =
        boost::make_shared<GaussLobattoIntegral>(100, 1E-8, 1E-8);
    integrator_ = boost::make_shared<PiecewiseIntegral>(baseIntegrator,
                                                        volsteptimes_, true);
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

void Qg1dLinearModel::updateIntKappa() const {
    Real sum = 0.0;
    intKappa_.resize(volsteptimes_.size());
    for (Size i = 0; i < volsteptimes_.size(); ++i) {
        sum += kappa_[i] *
               (volsteptimes_[i] - (i == 0 ? 0.0 : volsteptimes_[i - 1]));
        intKappa_[i] = sum;
    }
}

Real Qg1dLinearModel::kappa(const Real t) const {
    return QL_PIECEWISE_FUNCTION(volsteptimes_, kappa_, t);
}

Real Qg1dLinearModel::lambda(const Real t) const {
    return QL_PIECEWISE_FUNCTION(volsteptimes_, lambda_, t);
}

Real Qg1dLinearModel::alpha(const Real t) const {
    return QL_PIECEWISE_FUNCTION(volsteptimes_, alpha_, t);
}

Real Qg1dLinearModel::beta(const Real t) const {
    return QL_PIECEWISE_FUNCTION(volsteptimes_, beta_, t);
}

Real Qg1dLinearModel::h(const Real t) const {
    if (t < 0.0)
        return 0.0;
    Size i = std::upper_bound(volsteptimes_.begin(), volsteptimes_.end(), t) -
             volsteptimes_.begin();
    Real tmp = 0.0;
    if (i >= 1)
        tmp += intKappa_[std::min(i - 1, intKappa_.size() - 1)];
    tmp += kappa_[std::min(i, kappa_.size() - 1)] *
           (t - (i == 0 ? 0.0 : volsteptimes_[i - 1]));
    return std::exp(-tmp);
}

} // namespace QuantLib

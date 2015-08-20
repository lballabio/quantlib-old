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

#include <ql/experimental/models/cclgm1.hpp

namespace QuantLib {

CcLgm1::CcLgm1(const std::vector<boost::shared_ptr<Lgm<ImplLgm> > > &models,
               const std::vector<Real> &fxSpots,
               const std::vector<Date> &fxVolStepDates,
               const std::vector<std::vector<Real> > &fxVolatilities,
               const Matrix &correlation,
               const std::vector<Handle<YieldTermStructure> > &curves)
    : CalibratedModel(fxSpots.size()),
      CcLgm<CcLgmPiecewise, LgmFxPiecewiseSigma,
            LgmPiecewiseAlphaConstantKappa>(models),
      fxSpots_(fxSpots), fxVolStepDates_(fxVolStepDates),
      fxVolatilities_(fxVolatilities), correlation_(correlation),
      curves_(curves) {
    initialize();
}

void CcLgm1::initialize() {
    QL_REQUIRE(curves.size() == n() + 1,
               "there must be n+1 = " << n() + 1 << " curves, " << curves.size()
                                      << " given.");
    QL_REQUIRE(fxSpots.size() == n(),
               "there must be n = " << n() << " fx spots, " << fxSpots.size()
                                    << " given.");

    QL_REQUIRE(fxVolatilities.size() == n(),
               "there must be n = " << n() << " fx volatility vectors, "
                                    << fxVolatilities.size() << " given.");

    for (Size i = 0; i < n(); ++i)
        QL_REQUIRE(fxVolatilities[i].size() == fxVolStepDates_.size() + 1,
                   "the must be k+1 = "
                       << fxVolStepDates_.size() + 1
                       << " fx volatilities given, but for fx pair " << i
                       << " there are " << fxVolatilities[i].size() << ".");

    fxVolStepTimesArray_ = Array(fxVolStepDates_.size());
    updateTimes();

    for (Size i = 0; i < n(); ++i) {
        arguments_[i] =
            PiecewiseConstantParameter(fxVolStepTimes_, NoConstraint());
        for (Size j = 0; j < fxVolatilities[i].size(); ++j) {
            arguments_[i].setParam(j, fxVolatilities[j]);
        }
    }

    for (Size i = 0; i < n() + 1; ++i) {
        if (curves_[i].empty()) {
            curves_[i] = models(i)->termstructure();
        }
    }

    std::vector<boost::shared_ptr<LgmFxParametrization<LgmFxPiecewiseSigma> > >
        fxParametrizations;
    std::vector<
        boost::shared_ptr<LgmParametrization<LgmPiecewiseAlphaConstantKappa> > >
        lgmParametrizations;
    for (Size i = 0; i < n(); ++i) {
        fxParametrizations.push_back(boost::make_shared<LgmFxPiecewiseSigma>(
            fxVolStepTimes_, arguments_[i].params()));
    }
    for (Size i = 0; i < n() + 1; ++i) {
        lgmParametrizations.push_back(model(i)->parametrization());
    }

    setParametrization(boost::make_shared<CcLgmPiecewise>(
        fxParametrizations, lgmParametrizations, correlation_));

    stateProcess_ =
        boost::make_shared<CcLgmProcess<CcLgmPiecewise, LgmFxPiecewiseSigma,
                                        LgmPiecewiseAlphaConstantKappa> >(
            parametrization(), fxSpots_, cuves_);
}

void CcLgm1::updateTimes() const {
    fxVolStepTimes_.clear();
    int j = 0;
    for (std::vector<Date>::const_iterator i = fxVolStepDates_.begin(),
                                           i != fxVolStepDates_.end();
         ++i, ++j) {
        fxVolStepTimes_.push_back(termStructure()->timeFromReference(*i));
        fxVolStepTimesArray_[j] = volststeptimes_[j];
        if (j == 0)
            QL_REQUIRE(fxVolStepTimes_[0] > 0.0,
                       "fx volsteptimes must be positive ("
                           << fxVolStepTimes_[0] << ")");
        else
            QL_REQUIRE(fxVolStepTimes_[j] > fxVolStepTimes_[j - 1],
                       "fx volsteptimes must be increasing ("
                               << fxVolStepTimes_[j - 1] <
                           "@" << (j - 1) << ", " << fxVolStepTimes_[j] << "@"
                               << j << ")");
    }
    if (stateProcess() != NULL)
        boost::static_pointer_cast<CcLgmProcess<Impl, ImplFx, ImplLgm> >(
            stateProcess()->flushCache());
    if (parametrization() != NULL)
        parametrization()->update();
}
} // namespace QuantLib

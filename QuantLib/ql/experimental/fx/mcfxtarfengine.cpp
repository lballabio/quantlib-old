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

#include <ql/experimental/fx/mcfxtarfengine.hpp>

#include <iostream>

namespace QuantLib {

FxTarfPathPricer::FxTarfPathPricer(const std::vector<Real> &fixingTimes,
                                   const std::vector<Real> &discounts,
                                   const Real accumulatedAmount,
                                   const Real sourceNominal, const Real target,
                                   const FxTarf *instrument, ProxyData &data,
                                   const std::vector<Real> &accBucketLimits)
    : fixingTimes_(fixingTimes), discounts_(discounts),
      accumulatedAmount_(accumulatedAmount), sourceNominal_(sourceNominal),
      target_(target), instrument_(instrument), data_(data),
      accBucketLimits_(accBucketLimits) {
    QL_REQUIRE(instrument_ != NULL, "no instrument given");
}

Real FxTarfPathPricer::operator()(const Path &path) const {
    Real acc = accumulatedAmount_;
    Real pathNpv = 0.0;

    // we can precompute the fixing indices

    if (fixingIndices_.size() == 0) {
        for (Size i = 0, j = 0; i < path.length(); ++i) {
            if (close(fixingTimes_[j], path.time(i))) {
                fixingIndices_.push_back(i);
                ++j;
            }
        }
        QL_REQUIRE(fixingTimes_.size() == fixingIndices_.size(),
                   "not all fixing times found in grid");
    }

    // computation of the npv (only on grid points that represent a fixing)

    std::vector<Real> partialNpvs(fixingTimes_.size(), 0.0),
        underlying(fixingTimes_.size(), 0.0),
        accumulatedAmount(fixingTimes_.size(), acc);
    for (Size j = 0; j < fixingIndices_.size(); ++j) {
        underlying[j] = path[fixingIndices_[j]];
        partialNpvs[j] = instrument_->payout(path[fixingIndices_[j]], acc) *
                         discounts_[j] * sourceNominal_;
        accumulatedAmount[j] = acc;
        pathNpv += partialNpvs[j];
    }

    // collect data for regression analysis (on all grid points)

    Real pathNpvTmp = pathNpv;
    Size openFixingsTmp = fixingTimes_.size();
    Real accumulatedTmp = accumulatedAmount_;

    // fill the container

    for (Size i = 0, j = 0; i < path.length(); ++i) {
        if (i == fixingIndices_[j]) {
            pathNpvTmp -= partialNpvs[j];
            openFixingsTmp--;
            accumulatedTmp = accumulatedAmount[j];
            ++j;
        }
        // only collect data for paths that still have one open fixing and are
        // not knocked
        if (openFixingsTmp > 0 && accumulatedTmp < target_) {
            Size accInd =
                std::upper_bound(accBucketLimits_.begin(),
                                 accBucketLimits_.end(), accumulatedTmp) -
                accBucketLimits_.begin() - 1;
            std::vector<std::pair<Real, Real> > &spotVec =
                data_[openFixingsTmp - 1][accInd];
            std::pair<Real, Real> spotNpv = std::make_pair(path[i], pathNpvTmp);
            std::vector<std::pair<Real, Real> >::const_iterator spotInd =
                std::upper_bound(spotVec.begin(), spotVec.end(), spotNpv);
            spotVec.insert(spotInd, spotNpv);
            Real residualTime = fixingTimes_.back() - path.time(i);
            std::cout << openFixingsTmp << " " << path[i] << " " << residualTime
                      << " " << accumulatedTmp << " " << pathNpvTmp
                      << std::endl;
            // output raw data for testing
            // std::cout << path.time(i) << ";" << openFixingsTmp << ";"
            //           << accumulatedTmp << ";" << accInd << ";" << path[i]
            //           << ";" << pathNpvTmp << std::endl;
        }
    }

    return pathNpv;
}

} // namespace QuantLib

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

namespace QuantLib {

FxTarfPathPricer::FxTarfPathPricer(const std::vector<Real> &fixingTimes,
                                   const std::vector<Real> &discounts,
                                   const Real accumulatedAmount,
                                   const Real sourceNominal,
                                   const FxTarf *instrument)
    : fixingTimes_(fixingTimes), discounts_(discounts),
      accumulatedAmount_(accumulatedAmount), sourceNominal_(sourceNominal),
      instrument_(instrument) {
    QL_REQUIRE(instrument_ != NULL, "no instrument given");
}

Real FxTarfPathPricer::operator()(const Path &path) const {
    Real acc = accumulatedAmount_;
    Real pathNpv = 0.0;
    // we can precompute the fixing indices
    if (fixingIndices_.size() == 0) {
        for (Size i = 0; i < fixingTimes_.size(); ++i) {
            if (close(fixingTimes_[i], path.time(i))) {
                fixingIndices_.push_back(i);
            }
        }
        QL_REQUIRE(fixingTimes_.size() == fixingIndices_.size(),
                   "not all fixing times found in grid");
    }
    // regular computation
    for (Size i = 0; i < fixingIndices_.size(); ++i) {
        Real underlying = path[fixingIndices_[i]];
        Real payout = instrument_->payout(underlying, acc);
        acc += payout;
        pathNpv += payout * discounts_[i] * sourceNominal_;
    }
    return pathNpv;
}

} // namespace QuantLib

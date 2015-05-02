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
        for (Size i = 0, j = 0; i < path.length(); ++i) {
            if (close(fixingTimes_[j], path.time(i))) {
                fixingIndices_.push_back(i);
                ++j;
            }
        }
        QL_REQUIRE(fixingTimes_.size() == fixingIndices_.size(),
                   "not all fixing times found in grid");
    }
    // regular computation
    //    std::cout << "***path***\n";
    //    std::cout << "underlying;time;payout;acc" << std::endl;
    // for (Size i = 0; i < fixingIndices_.size(); ++i) {
    //     Real underlying = path[fixingIndices_[i]];
    //     Real payout = instrument_->payout(underlying, acc);
    //     pathNpv += payout * discounts_[i] * sourceNominal_;
    //        std::cout << underlying << ";" << path.time(fixingIndices_[i]) <<
    //        ";" << payout << ";" << acc << std::endl;
    //    }

    // computation writing out proxy information
    std::vector<Real> partialNpvs(fixingTimes_.size(), 0.0), underlying(fixingTimes_.size(),0.0),
        accumulatedAmount(fixingTimes_.size(), acc);
    for (Size i = 0, j = 0; i < path.length(); ++i) {
        if (i == fixingIndices_[j]) {
            underlying[j] = path[i];
            partialNpvs[j] = instrument_->payout(path[i], acc) * discounts_[j] *
                             sourceNominal_;
            accumulatedAmount[j] = acc;
            pathNpv += partialNpvs[j];
            ++j;
        }
    }
    Real pathNpvTmp = pathNpv;
    Size openFixingsTmp = fixingTimes_.size();
    Real accumulatedTmp = accumulatedAmount_;
    for (Size i = 0, j = 0; i < path.length(); ++i) {
        if (i == fixingIndices_[j]) {
            pathNpvTmp -= partialNpvs[j];
            openFixingsTmp--;
            accumulatedTmp = accumulatedAmount[j];
            ++j;
        }
        Real residualTime = fixingTimes_.back() - path.time(i);
        std::cout << openFixingsTmp << " " << path[i] << " " << residualTime
                  << " " << accumulatedTmp << " " << pathNpvTmp << std::endl;
    }
    return pathNpv;
}

} // namespace QuantLib

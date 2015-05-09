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

#include <ql/experimental/fx/proxyfxtarfengine.hpp>

#include <iostream> // just for debug

namespace QuantLib {

void ProxyFxTarfEngine::calculate() const {

    // handle the trivial cases
    FxTarfEngine::calculate();

    // determine the number of open fixings
    Date today = Settings::instance().evaluationDate();
    Size numberOpenFixings =
        std::distance(std::upper_bound(arguments_.openFixingDates.begin(),
                                       arguments_.openFixingDates.end(), today),
                      arguments_.openFixingDates.end());

    // determine the accumulated amount index
    Size accInd = std::upper_bound(proxy_->accBucketLimits.begin(),
                                   proxy_->accBucketLimits.end(),
                                   arguments_.accumulatedAmount) -
                  proxy_->accBucketLimits.begin() - 1;

    // sanity checks
    QL_REQUIRE(numberOpenFixings <= proxy_->maxNumberOpenFixings,
               "number of open fixings ("
                   << numberOpenFixings
                   << ") must be less or equal the number of open fixings "
                      "provided by the proxy object ("
                   << proxy_->maxNumberOpenFixings << ")");
    QL_REQUIRE(accInd >= 0 && accInd < proxy_->accBucketLimits.size(),
               "accumulated amount index ("
                   << accInd << ") out of range given by the proxy (0..."
                   << proxy_->accBucketLimits.size() << ")");

    // loggin
    std::cerr << "proxy engine: use function (openFixingsIndex, accIndex) = ("
              << (numberOpenFixings - 1) << "," << accInd << ")" << std::endl;

    // get the proxy function and return the npv, on forward basis
    results_.value =
        proxy_->functions[numberOpenFixings - 1][accInd]->operator()(
            exchangeRate_->value()) *
            discount_->discount(proxy_->lastPaymentDate) +
        unsettledAmountNpv_;
}

} // namespace QuantLib

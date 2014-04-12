/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2014 Peter Caspers

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

#include <ql/experimental/credit/cdolightengines.hpp>
#include <ql/math/statistics/incrementalstatistics.hpp>
#include <ql/math/distributions/normaldistribution.hpp>

namespace QuantLib {

CdoLightMcEngine::CdoLightMcEngine(
    const Handle<YieldTermStructure> &yieldCurve,
    const std::vector<Handle<DefaultProbabilityTermStructure>> &defaultCurves,
    const std::vector<Handle<Quote>> &recoveryRates,
    const Handle<Quote> &correlation, Size paths, BigNatural seed)
    : yieldCurve_(yieldCurve), defaultCurves_(defaultCurves),
      recoveryRates_(recoveryRates), correlation_(correlation), paths_(paths),
      seed_(seed), rsg_(PseudoRandom::make_sequence_generator(
                       defaultCurves_.size() + 1, seed_)) {

    QL_REQUIRE(defaultCurves_.size() == recoveryRates_.size(),
               "default curves size (" << defaultCurves_.size()
                                       << ") does not match "
                                          "recovery rates size ("
                                       << recoveryRates_.size() << ")");

    registerWith(yieldCurve);
    for (Size i = 0; i < defaultCurves_.size(); i++)
        registerWith(defaultCurves_[i]);
    for (Size i = 0; i < recoveryRates_.size(); i++)
        registerWith(recoveryRates_[i]);
    registerWith(correlation);

    defaultIndexes_.resize(defaultCurves_.size());
}

void CdoLightMcEngine::calculate() const {

    Date today = Settings::instance().evaluationDate();

    firstPeriod_ = std::upper_bound(arguments_.paymentDates.begin(),
                                    arguments_.paymentDates.end(), today) -
                   arguments_.paymentDates.begin();

    if (firstPeriod_ == arguments_.paymentDates.size()) {
        results_.value = 0.0;
        return;
    }

    periodTimes_.clear();
    for (Size i = firstPeriod_; i < arguments_.paymentDates.size(); ++i) {
        periodTimes_.push_back(
            yieldCurve_->timeFromReference(arguments_.paymentDates[i]));
    }

    for (Size i = 0; i < defaultCurves_.size(); ++i) {
        std::vector<Real> tmp(periodTimes_.size());
        for (Size j = firstPeriod_; j < arguments_.paymentDates.size(); ++i) {
            tmp[j] = defaultCurves_[i]->defaultProbability(
                arguments_.paymentDates[i]);
        }
        periodDefaultProbs_.push_back(tmp);
    }

    QL_REQUIRE(arguments_.nominals.size() == defaultCurves_.size(),
               "nominals size (" << arguments_.nominals.size()
                                 << ") does not match "
                                    "default curves size ("
                                 << defaultCurves_.size() << ")");

    rsg_ = PseudoRandom::make_sequence_generator(defaultCurves_.size() + 1,
                                                 seed_); // reset rsg

    IncrementalStatistics stat;

    for (Size i = 0; i < paths_; i++) {
        generateDefaultTimes();
        stat.add(pathPayoff());
    }

    results_.value =
        stat.mean() * (arguments_.side == Protection::Buyer ? 1.0 : (-1.0));
}

void CdoLightMcEngine::generateDefaultTimes() const {

    const std::vector<Real> &values = rsg_.nextSequence().value;
    Real r = correlation_->value();

    for (Size i = 0; i < defaultCurves_.size(); i++) {
        Real y = r * values[0] + std::sqrt(1 - r * r) * values[i + 1];
        Real p = CumulativeNormalDistribution()(y);
        defaultIndexes_[i] = std::upper_bound(periodDefaultProbs_[i].begin(),
                                              periodDefaultProbs_[i].end(), p) -
                             periodDefaultProbs_[i].begin();
    }
}

Real CdoLightMcEngine::pathPayoff() const {

    Real cumulativeCoupon = 0.0;
    Real cumulativeProtection = 0.0;

    for (Size i = 0; i < arguments_.paymentDates.size() - firstPeriod_; ++i) {

        Real coupon = 0.0;
        Real protection = 0.0;

        for (Size j = 0; j < defaultCurves_.size(); j++) {
            if (defaultIndexes_[j] <= i) {
                // we assume that the default happens on mid of period
                if (defaultIndexes_[j] == i) {
                    coupon += 0.5 * arguments_.nominals[j] *
                        (1.0 - recoveryRates_[j]->value());
                    protection +=
                        arguments_.nominals[j] * (1.0 - (recoveryRates_[j]->value()));
                }
                coupon += arguments_.nominals[j] * recoveryRates_[j]->value();
            } else {
                coupon += arguments_.nominals[j];
            }
        }

        cumulativeCoupon +=
            coupon * arguments_.spread *
            arguments_.accrualTimes[i + firstPeriod_] *
            yieldCurve_->discount(arguments_.paymentDates[i + firstPeriod_]);

        cumulativeProtection +=
            protection *
            yieldCurve_->discount(
                i == 0 ? periodTimes_[0] / 2.0
                       : (periodTimes_[i - 1] + periodTimes_[i]) / 2.0);
    }

    return cumulativeProtection - cumulativeCoupon;
}
}

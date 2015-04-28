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

#include <ql/experimental/fx/fxtarf.hpp>
#include <ql/event.hpp>

namespace QuantLib {

FxTarf::FxTarf(const Schedule schedule, const boost::shared_ptr<FxIndex> &index,
               const Real sourceNominal, const Real strike,
               const Option::Type longPositionType,
               const Real shortPositionFactor, const Real target,
               const Handle<Quote> accumulatedAmount)
    : schedule_(schedule), index_(index), sourceNominal_(sourceNominal),
      strike_(strike), longPositionType_(longPositionType),
      shortPositionFactor_(shortPositionFactor), target_(target),
      accumulatedAmount_(accumulatedAmount) {

    QL_REQUIRE(schedule.size() >= 2,
               "FXTarf requires at least 2 schedule dates (" << schedule.size()
                                                             << ")");
    registerWith(accumulatedAmount);
    registerWith(Settings::instance().evaluationDate());
}

bool FxTarf::isExpired() const {
    if (!accumulatedAmount_.empty())
        return accumulatedAmount_->value() >= target_;
    else {
        std::pair<Real, bool> acc = accumulatedAmountAndSettlement();
        return acc.first >= target_ && acc.second;
    }
}

std::pair<Real, bool> FxTarf::accumulatedAmountAndSettlement() const {
    Real acc = accumulatedAmount_.empty() ? 0.0 : accumulatedAmount_->value();
    int i = 1;
    while (detail::simple_event(index_->fixingDate(schedule_.date(i)))
               .hasOccurred()) {
        if (accumulatedAmount_.empty()) {
            acc += std::max(
                static_cast<Real>(longPositionType_) *
                    (index_->fixing(index_->fixingDate(schedule_.date(i))) -
                     strike_),
                0.);
        }
        ++i;
    }
    bool settled = detail::simple_event(schedule_.date(i - 1)).hasOccurred();
    return std::make_pair(acc, settled);
}

void FxTarf::setupExpired() const { Instrument::setupExpired(); }

void FxTarf::setupArguments(PricingEngine::arguments *args) const {
    FxTarf::arguments *arguments = dynamic_cast<FxTarf::arguments *>(args);
    QL_REQUIRE(arguments != 0, "wrong argument type");
    arguments->schedule = schedule_;
    arguments->index = index_;
    arguments->sourceNominal = sourceNominal_;
    arguments->strike = strike_;
    arguments->longPositionType = longPositionType_;
    arguments->shortPositionFactor = shortPositionFactor_;
    arguments->target = target_;
    arguments->accumulatedAmount = accumulatedAmountAndSettlement().first;
}

void FxTarf::fetchResults(const PricingEngine::results *r) const {
    Instrument::fetchResults(r);
    const FxTarf::results *results = dynamic_cast<const FxTarf::results *>(r);
    QL_REQUIRE(results != 0, "wrong results type");
    proxy_ = results->proxy;
}

Date FxTarf::startDate() const { return schedule_.dates().front(); }

Date FxTarf::maturityDate() const { return schedule_.dates().back(); }

boost::shared_ptr<ProxyDescription> FxTarf::proxy() const {
    calculate();
    QL_REQUIRE(proxy_ != NULL, "no valid proxy available");
    return proxy_;
}

void FxTarf::arguments::validate() const {}

void FxTarf::results::reset() { Instrument::results::reset(); }

} // namespace QuantLib

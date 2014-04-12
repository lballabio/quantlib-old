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

#include <ql/experimental/credit/cdolight.hpp>
#include <ql/event.hpp>
#include <ql/settings.hpp>

namespace QuantLib {

CdoLight::CdoLight(Protection::Side side, const Schedule &schedule, Real spread,
                   const DayCounter &dayCounter,
                   const std::vector<Real> &nominals, Real relativeAttachment,
                   Real relativeDetachment,
                   BusinessDayConvention paymentConvention)
    : side_(side), schedule_(schedule), spread_(spread),
      dayCounter_(dayCounter), nominals_(nominals),
      relativeAttachment_(relativeAttachment),
      relativeDetachment_(relativeDetachment),
      paymentConvention_(paymentConvention) {}

bool CdoLight::isExpired() const {
    Date lastPayment = schedule_.calendar().adjust(schedule_.dates().back(),
                                                   paymentConvention_);
    return Settings::instance().includeTodaysCashFlows()
               ? lastPayment > Settings::instance().evaluationDate()
               : lastPayment > Settings::instance().evaluationDate();
}

void CdoLight::setupExpired() const { Instrument::setupExpired(); }

void CdoLight::setupArguments(PricingEngine::arguments *args) const {
    CdoLight::arguments *arguments = dynamic_cast<CdoLight::arguments *>(args);
    QL_REQUIRE(arguments != NULL, "wrong argument type for CdoLight");
    arguments->side = side_;
    arguments->schedule = schedule_;
    arguments->spread = spread_;
    arguments->dayCounter = dayCounter_;
    arguments->relativeAttachment = relativeAttachment_;
    arguments->relativeDetachment = relativeDetachment_;
    arguments->paymentConvention = paymentConvention_;
    arguments->accrualTimes.clear();
    for (Size i = 1; i < schedule_.size(); i++) {
        arguments->accrualTimes.push_back(
            dayCounter_.yearFraction(schedule_.date(i - 1), schedule_.date(i)));
        arguments->paymentDates.push_back(
            schedule_.calendar().adjust(schedule_[i], paymentConvention_));
    }
}

void CdoLight::fetchResults(const PricingEngine::results *r) const {
    Instrument::fetchResults(r);

    const CdoLight::results *results =
        dynamic_cast<const CdoLight::results *>(r);
    QL_REQUIRE(results != NULL, "wrong result type for CdoLight");
}

void CdoLight::arguments::validate() const {
    QL_REQUIRE(side != Protection::Side(-1), "side not set");
    QL_REQUIRE(!dayCounter.empty(), "no day counter given");
}

void CdoLight::results::reset() { Instrument::results::reset(); }
}

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

/*! \file cdolight.hpp
    \brief Light version of cdo instrument, for validation purposes
*/

#ifndef quantlib_cdolight_hpp
#define quantlib_cdolight_hpp

#include <ql/default.hpp>
#include <ql/instrument.hpp>
#include <ql/time/schedule.hpp>
#include <ql/time/daycounter.hpp>

namespace QuantLib {

class CdoLight : public Instrument {

  public:
    class arguments;
    class results;
    class engine;

    CdoLight(Protection::Side side, const Schedule &schedule, Real spread,
             const DayCounter &dayCounter, const std::vector<Real> &nominals,
             Real relativeAttachment = 0.0, Real relativeDetachment = 1.0,
             BusinessDayConvention paymentConvention = Following);

    bool isExpired() const;
    void setupArguments(PricingEngine::arguments *) const;
    void fetchResults(const PricingEngine::results *) const;

  private:
    void setupExpired() const;
    Protection::Side side_;
    const Schedule schedule_;
    Real spread_;
    const DayCounter dayCounter_;
    const std::vector<Real> nominals_;
    Real relativeAttachment_, relativeDetachment_;
    BusinessDayConvention paymentConvention_;
};

class CdoLight::arguments : public virtual PricingEngine::arguments {

  public:
    arguments() {}
    void validate() const;

    Protection::Side side;
    Schedule schedule;
    std::vector<Date> paymentDates;
    std::vector<Real> accrualTimes;
    Real spread;
    DayCounter dayCounter;
    std::vector<Real> nominals;
    Real relativeAttachment, relativeDetachment;
    BusinessDayConvention paymentConvention;
};

class CdoLight::results : public Instrument::results {

  public:
    void reset();
};
}

#endif

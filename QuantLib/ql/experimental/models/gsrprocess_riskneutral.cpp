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

#include <ql/experimental/models/gsrprocess_riskneutral.hpp>

namespace QuantLib {

GsrProcessRiskNeutral::GsrProcessRiskNeutral(
    const Array &times, const Array &vols, const Array &reversions,
    const Array &adjusters, const Date &referenceDate, const DayCounter &dc)
    : core_(times, vols, reversions, adjusters), referenceDate_(referenceDate),
      dc_(dc) {
    flushCache();
}

Real GsrProcessRiskNeutral::time(const Date &d) const {
    QL_REQUIRE(
        referenceDate_ != Null<Date>() && dc_ != DayCounter(),
        "time can not be computed without reference date and day counter");
    return dc_.yearFraction(referenceDate_, d);
}

Real GsrProcessRiskNeutral::x0() const { return 0.0; }

Real GsrProcessRiskNeutral::drift(Time t, Real x) const {
    return core_.y(t) - reversion(t) * x;
}

Real GsrProcessRiskNeutral::diffusion(Time t, Real) const { return sigma(t); }

Real GsrProcessRiskNeutral::expectation(Time w, Real xw, Time dt) const {
    return core_.expectation_x0dep_part(w, xw, dt) +
           core_.expectation_rn_part(w, dt);
}

Real GsrProcessRiskNeutral::stdDeviation(Time t0, Real x0, Time dt) const {
    return sqrt(variance(t0, x0, dt));
}

Real GsrProcessRiskNeutral::variance(Time w, Real, Time dt) const {
    return core_.variance(w, dt);
}

Real GsrProcessRiskNeutral::sigma(Time t) const { return core_.sigma(t); }

Real GsrProcessRiskNeutral::reversion(Time t) const {
    return core_.reversion(t);
}

Real GsrProcessRiskNeutral::y(Time t) const { return core_.y(t); }

Real GsrProcessRiskNeutral::G(Time t, Time w, Real) const {
    return core_.G(t, w);
}
}

/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2012 Peter Caspers

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

#include <ql/cashflows/zcinflationcapfloor.hpp>
#include <ql/math/solvers1d/brent.hpp>
#include <ql/pricingengines/blackformula.hpp>

namespace QuantLib {

Real ZCInflationCapFloor::amount(Real totalVariance) const {

    Real firstCpi = index_->fixing(actualFirstFixingDate_);
    Real lastCpi = index_->fixing(actualLastFixingDate_);
    Real rate = lastCpi/firstCpi;
    Real dcf = dc_.yearFraction(firstFixingDate_, lastFixingDate_);
    Real effectiveStrike = std::pow(1.0 + strike_, dcf);
    Real stdDev = totalVariance == Null<Real>()
                      ? inflationVol_->blackVol(lastFixingDate_, strike_) *
                            std::sqrt(fixingTime_)
                      : std::sqrt(totalVariance);
    return notional_ * blackFormula(type_, effectiveStrike, rate, stdDev);
}

Real ZCInflationCapFloor::amount() const { return amount(Null<Real>()); }

Real ZCInflationCapFloor::impliedTotalVariance(Real undeflatedPrice) const {
    ImpliedVarianceHelper h(this, undeflatedPrice);
    Brent b;
    Real guess = 0.02 * 0.02 * fixingTime_;
    Real min = 0.0;
    Real max = 0.20 * 0.20 * fixingTime_;
    Real var = b.solve(h, 1E-4, guess, min, max);
    return var;
}

Real ZCInflationCapFloor::impliedVolatility(Real undeflatedPrice) const {
    return std::sqrt(impliedTotalVariance(undeflatedPrice) / fixingTime_);
}
}

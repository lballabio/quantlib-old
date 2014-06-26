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

namespace QuantLib {

Real ZCInflationCapFloor::amount(Real totalVariance) const {

    Real baseFixing = index_->fixing(baseDate_);
    Real ytsBaseFixing = index_->fixing(inflationYts_->baseDate());
    Real nominalRate = nominalYts_->zeroRate(fixingDate_, Actual365Fixed(),
                                             QuantLib::Continuous);
    Real inflationRate = inflationYts_->zeroRate(fixingDate_);
    Real realRate = nominalRate - log(1.0 + inflationRate);
    Real fwdDiscount = nominalYts_->discount(paymentDate_) /
                       nominalYts_->discount(fixingDate_);
    Real modStrike = baseFixing * std::pow(1.0 + strike_, fixingTime_);
    Real vol = totalVariance == Null<Real>()
                   ? inflationVol_->blackVol(fixingDate_, strike_)
                   : std::sqrt(totalVariance / fixingTime_);
    Real d1 = (log(ytsBaseFixing / modStrike) +
               (nominalRate - realRate + vol * vol / 2.0) * fixingTime_) /
              (vol * sqrt(fixingTime_));
    Real d2 = d1 - vol * sqrt(fixingTime_);
    CumulativeNormalDistribution cnd;
    Real value = notional_ * fwdDiscount / baseFixing *
                 (baseFixing * exp(-realRate * fixingTime_) * cnd(type_ * d1) -
                  modStrike * exp(-nominalRate * fixingTime_) * cnd(type_ * d2)) *
                 type_;
    return value * exp(nominalRate * fixingTime_);
}

Real ZCInflationCapFloor::amount() const { return amount(Null<Real>()); }

Real ZCInflationCapFloor::impliedTotalVariance(Real undeflatedPrice) const {

    ImpliedVarianceHelper h(this, undeflatedPrice);
    Brent b;
    Real sqrtt = std::sqrt(fixingTime_);
    Real guess = 0.02 * sqrtt;
    Real min = 0.0 * sqrtt;
    Real max = 0.20 * sqrtt;
    Real var = b.solve(h, 1E-4, guess, min, max );
    return var;

}



}

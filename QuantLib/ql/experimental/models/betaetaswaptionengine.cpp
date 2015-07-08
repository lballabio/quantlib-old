/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2015 Peter Caspers, Roland Lichters

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

#include <ql/experimental/models/betaetaswaptionengine.hpp>

#include <ql/math/interpolations/cubicinterpolation.hpp>

namespace QuantLib {

void BetaEtaSwaptionEngine::calculate() const {

    QL_REQUIRE(arguments_.settlementType == Settlement::Physical,
               "cash-settled swaptions not yet implemented");

    Date settlement = model_->termStructure()->referenceDate();

    if (arguments_.exercise->dates().back() <=
        settlement) { // swaption is expired, possibly generated swap is not
                      // valued
        results_.value = 0.0;
        return;
    }

    int idx = static_cast<int>(arguments_.exercise->dates().size()) - 1;
    int minIdxAlive = static_cast<int>(
        std::upper_bound(arguments_.exercise->dates().begin(),
                         arguments_.exercise->dates().end(), settlement) -
        arguments_.exercise->dates().begin());

    VanillaSwap swap = *arguments_.swap;
    Option::Type type =
        arguments_.type == VanillaSwap::Payer ? Option::Call : Option::Put;
    Schedule fixedSchedule = swap.fixedSchedule();
    Schedule floatSchedule = swap.floatingSchedule();

    Array npv0(2 * gridPoints_ + 1, 0.0), npv1(2 * gridPoints_ + 1, 0.0), xprev;

    Date expiry1 = Null<Date>(), expiry0;
    Time expiry1Time = Null<Real>(), expiry0Time;

    do {

        if (idx == minIdxAlive - 1)
            expiry0 = settlement;
        else
            expiry0 = arguments_.exercise->dates()[idx];

        expiry0Time =
            std::max(model_->termStructure()->timeFromReference(expiry0), 0.0);

        Size j1 = std::upper_bound(fixedSchedule.dates().begin(),
                                   fixedSchedule.dates().end(), expiry0 - 1) -
                  fixedSchedule.dates().begin();
        Size k1 = std::upper_bound(floatSchedule.dates().begin(),
                                   floatSchedule.dates().end(), expiry0 - 1) -
                  floatSchedule.dates().begin();

        Array x = model_->xGrid(stddevs_, gridPoints_, expiry0Time);

        // a lazy object is not thread safe, neither is the caching
        // in gsrprocess. therefore we trigger computations here such
        // that neither lazy object recalculation nor write access
        // during caching occurs in the parallized loop below.
        /*#ifdef _OPENMP
                if (expiry0 > settlement) {
                    for (Size l = k1; l < arguments_.floatingCoupons.size();
        l++) {
                        model_->forwardRate(arguments_.floatingFixingDates[l],
        expiry0,
                                            0.0, arguments_.swap->iborIndex());
                        model_->zerobond(arguments_.floatingPayDates[l],
        expiry0, 0.0,
                                         discountCurve_);
                    }
                    for (Size l = j1; l < arguments_.fixedCoupons.size(); l++) {
                        model_->zerobond(arguments_.fixedPayDates[l], expiry0,
        0.0,
                                         discountCurve_);
                    }
                    model_->numeraire(expiry0Time, 0.0, discountCurve_);
                }
        #endif*/

        /* #pragma omp parallel for default(shared) \
           firstprivate(p) if (expiry0 > settlement)*/
        for (Size k = 0; k < (expiry0 > settlement ? npv0.size() : 1); k++) {

            Real price = 0.0;
            if (expiry1Time != Null<Real>()) {
                CubicInterpolation payoff0(xprev.begin(), xprev.end(),
                                           npv1.begin(),
                                           CubicInterpolation::Spline, true,
                                           CubicInterpolation::Lagrange, 0.0,
                                           CubicInterpolation::Lagrange, 0.0);
                payoff0.enableExtrapolation(true);
                price = model_->integrate(stddevs_, payoff0, expiry0Time, x[k],
                                          expiry1Time);
            }

            npv0[k] = price;

            if (expiry0 > settlement) {
                Real floatingLegNpv = 0.0;
                for (Size l = k1; l < arguments_.floatingCoupons.size(); l++) {
                    floatingLegNpv +=
                        arguments_.nominal *
                        arguments_.floatingAccrualTimes[l] *
                        (arguments_.floatingSpreads[l] +
                         model_->forwardRate(arguments_.floatingFixingDates[l],
                                             expiry0, x[k],
                                             arguments_.swap->iborIndex())) *
                        model_->zerobond(arguments_.floatingPayDates[l],
                                         expiry0, x[k], discountCurve_);
                }
                Real fixedLegNpv = 0.0;
                for (Size l = j1; l < arguments_.fixedCoupons.size(); l++) {
                    fixedLegNpv +=
                        arguments_.fixedCoupons[l] *
                        model_->zerobond(arguments_.fixedPayDates[l], expiry0,
                                         x[k], discountCurve_);
                }
                Real exerciseValue = (type == Option::Call ? 1.0 : -1.0) *
                     (floatingLegNpv - fixedLegNpv) /
                     model_->numeraire(expiry0Time, x[k], discountCurve_);
                npv0[k] = std::max(npv0[k], exerciseValue);

            } // if expiry0 > settlement

        }     // for k

        npv1.swap(npv0);
        xprev = x;

        expiry1 = expiry0;
        expiry1Time = expiry0Time;

    } while (--idx >= minIdxAlive - 1);

    results_.value = npv1[0] * model_->numeraire(0.0, 0.0, discountCurve_);
};

} // namespace QuantLib

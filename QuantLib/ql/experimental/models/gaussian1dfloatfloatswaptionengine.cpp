/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2013 Peter Caspers

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

#include <ql/experimental/models/gaussian1dfloatfloatswaptionengine.hpp>

namespace QuantLib {

    void Gaussian1dFloatFloatSwaptionEngine::calculate() const {

        // pricing

        Date settlement = model_->termStructure()->referenceDate();

        if (arguments_.exercise->dates().back() <=
            settlement) { // swaption is expired, possibly generated swap is not
                     // valued
            results_.value = 0.0;
            return;
        }

        // event dates are coupon fixing dates and exercise dates
        // we explicitly estimate cms and also libor coupons although
        // the latter could be calculated analytically to make the code
        // simpler

        std::vector<Date> events;
        events.insert(events.end(), arguments_.exercise->dates().begin(),
                      arguments_.exercise->dates().end());
        events.insert(events.end(), arguments_.leg1FixingDates.begin(),
                      arguments_.leg1FixingDates.end());
        events.insert(events.end(), arguments_.leg2FixingDates.begin(),
                      arguments_.leg1FixingDates.end());
        std::sort(events.begin(), events.end());
        std::vector<Date>::iterator it =
            std::unique(events.begin(), events.end());
        events.resize(std::distance(events.begin(), it));

        // only events starting tommorow are of interest by definition of the
        // deal part that is exericsed into,

        std::vector<Date>::iterator filit =
            std::upper_bound(events.begin(), events.end(), settlement);
        while (events[0] <= settlement)
            events.erase(events.begin(), filit);

        int idx = events.size() - 1;

        FloatFloatSwap swap = *arguments_.swap;
        Option::Type type =
            arguments_.type == VanillaSwap::Payer ? Option::Call : Option::Put;

        Array npv0(2 * integrationPoints_ + 1, 0.0),
            npv1(2 * integrationPoints_ + 1, 0.0); // arrays for npvs of the
                                                   // option
        Array npv0a(2 * integrationPoints_ + 1, 0.0),
            npv1a(2 * integrationPoints_ + 1, 0.0); // arrays for npvs of the
                                                    // underlying
        Array z = model_->yGrid(stddevs_, integrationPoints_);
        Array p(z.size(), 0.0), pa(z.size(), 0.0);

        Date event1 = Null<Date>(), event0;
        Time event1Time = Null<Real>(), event0Time;

        bool isExercise, isLeg1Fixing, isLeg2Fixing;

        boost::shared_ptr<IborIndex> ibor1 =
            boost::dynamic_pointer_cast<IborIndex>(arguments_.index1);
        boost::shared_ptr<SwapIndex> cms1 =
            boost::dynamic_pointer_cast<SwapIndex>(arguments_.index1);
        boost::shared_ptr<IborIndex> ibor2 =
            boost::dynamic_pointer_cast<IborIndex>(arguments_.index2);
        boost::shared_ptr<SwapIndex> cms2 =
            boost::dynamic_pointer_cast<SwapIndex>(arguments_.index2);

        QL_REQUIRE(ibor1 != NULL || cms1 != NULL,
                   "index1 must be ibor or swap index");
        QL_REQUIRE(ibor2 != NULL || cms2 != NULL,
                   "index2 must be ibor or swap index");

        do {

            // we are at event0 date, which can be a structured coupon fixing
            // date or an exercise date or both.

            if (idx == -1)
                event0 = settlement;
            else
                event0 = events[idx];

            if (std::find(arguments_.exercise->dates().begin(),
                          arguments_.exercise->dates().end(),
                          event0) != arguments_.exercise->dates().end())
                isExercise = true;
            else
                isExercise = false;

            if (std::find(arguments_.leg1FixingDates.begin(),
                          arguments_.leg1FixingDates.end(),
                          event0) != arguments_.leg1FixingDates.end())
                isLeg1Fixing = true;
            else
                isLeg1Fixing = false;

            if (std::find(arguments_.leg2FixingDates.begin(),
                          arguments_.leg2FixingDates.end(),
                          event0) != arguments_.leg2FixingDates.end())
                isLeg2Fixing = true;
            else
                isLeg2Fixing = false;

            event0Time = std::max(
                model_->termStructure()->timeFromReference(event0), 0.0);

            for (Size k = 0; k < (event0 > settlement ? npv0.size() : 1); k++) {

                // roll back

                Real price = 0.0, pricea = 0.0;
                if (event1Time != Null<Real>()) {
                    Real zSpreadDf =
                        oas_.empty() ? 1.0 : std::exp(-oas_->value() *
                                                     (event1Time - event0Time));
                    Array yg =
                        model_->yGrid(stddevs_, integrationPoints_, event1Time,
                                      event0Time, event0Time > 0 ? z[k] : 0.0);
                    CubicInterpolation payoff0(
                        z.begin(), z.end(), npv1.begin(),
                        CubicInterpolation::Spline, true,
                        CubicInterpolation::Lagrange, 0.0,
                        CubicInterpolation::Lagrange, 0.0);
                    CubicInterpolation payoff0a(
                        z.begin(), z.end(), npv1a.begin(),
                        CubicInterpolation::Spline, true,
                        CubicInterpolation::Lagrange, 0.0,
                        CubicInterpolation::Lagrange, 0.0);
                    for (Size i = 0; i < yg.size(); i++) {
                        p[i] = payoff0(yg[i], true);
                        pa[i] = payoff0a(yg[i], true);
                    }
                    CubicInterpolation payoff1(
                        z.begin(), z.end(), p.begin(),
                        CubicInterpolation::Spline, true,
                        CubicInterpolation::Lagrange, 0.0,
                        CubicInterpolation::Lagrange, 0.0);
                    CubicInterpolation payoff1a(
                        z.begin(), z.end(), pa.begin(),
                        CubicInterpolation::Spline, true,
                        CubicInterpolation::Lagrange, 0.0,
                        CubicInterpolation::Lagrange, 0.0);
                    for (Size i = 0; i < z.size() - 1; i++) {
                        price += model_->gaussianShiftedPolynomialIntegral(
                                     0.0, payoff1.cCoefficients()[i],
                                     payoff1.bCoefficients()[i],
                                     payoff1.aCoefficients()[i], p[i], z[i],
                                     z[i], z[i + 1]) *
                                 zSpreadDf;
                        pricea += model_->gaussianShiftedPolynomialIntegral(
                                      0.0, payoff1a.cCoefficients()[i],
                                      payoff1a.bCoefficients()[i],
                                      payoff1a.aCoefficients()[i], pa[i], z[i],
                                      z[i], z[i + 1]) *
                                  zSpreadDf;
                    }
                    if (extrapolatePayoff_) {
                        if (flatPayoffExtrapolation_) {
                            price +=
                                model_->gaussianShiftedPolynomialIntegral(
                                    0.0, 0.0, 0.0, 0.0, p[z.size() - 2],
                                    z[z.size() - 2], z[z.size() - 1], 100.0) *
                                zSpreadDf;
                            price += model_->gaussianShiftedPolynomialIntegral(
                                         0.0, 0.0, 0.0, 0.0, p[0], z[0], -100.0,
                                         z[0]) *
                                     zSpreadDf;
                            pricea +=
                                model_->gaussianShiftedPolynomialIntegral(
                                    0.0, 0.0, 0.0, 0.0, pa[z.size() - 2],
                                    z[z.size() - 2], z[z.size() - 1], 100.0) *
                                zSpreadDf;
                            pricea += model_->gaussianShiftedPolynomialIntegral(
                                          0.0, 0.0, 0.0, 0.0, pa[0], z[0],
                                          -100.0, z[0]) *
                                      zSpreadDf;
                        } else {
                            if (type == Option::Call)
                                price +=
                                    model_->gaussianShiftedPolynomialIntegral(
                                        0.0,
                                        payoff1.cCoefficients()[z.size() - 2],
                                        payoff1.bCoefficients()[z.size() - 2],
                                        payoff1.aCoefficients()[z.size() - 2],
                                        p[z.size() - 2], z[z.size() - 2],
                                        z[z.size() - 1], 100.0) *
                                    zSpreadDf;
                            if (type == Option::Put)
                                price +=
                                    model_->gaussianShiftedPolynomialIntegral(
                                        0.0, payoff1.cCoefficients()[0],
                                        payoff1.bCoefficients()[0],
                                        payoff1.aCoefficients()[0], p[0], z[0],
                                        -100.0, z[0]) *
                                    zSpreadDf;
                            if (type == Option::Call)
                                pricea +=
                                    model_->gaussianShiftedPolynomialIntegral(
                                        0.0,
                                        payoff1a.cCoefficients()[z.size() - 2],
                                        payoff1a.bCoefficients()[z.size() - 2],
                                        payoff1a.aCoefficients()[z.size() - 2],
                                        pa[z.size() - 2], z[z.size() - 2],
                                        z[z.size() - 1], 100.0) *
                                    zSpreadDf;
                            if (type == Option::Put)
                                pricea +=
                                    model_->gaussianShiftedPolynomialIntegral(
                                        0.0, payoff1a.cCoefficients()[0],
                                        payoff1a.bCoefficients()[0],
                                        payoff1a.aCoefficients()[0], pa[0],
                                        z[0], -100.0, z[0]) *
                                    zSpreadDf;
                        }
                    }
                }

                npv0[k] = price;
                npv0a[k] = pricea;

                // event date calculations

                if (event0 > settlement) {
                    if (isLeg1Fixing) { // if event is a fixing date and
                                        // exercise date,
                        // the coupon is part of the exercise into right (by
                        // definition)
                        Size j = std::find(arguments_.leg1FixingDates.begin(),
                                           arguments_.leg1FixingDates.end(),
                                           event0) -
                                 arguments_.leg1FixingDates.begin();
                        Real zSpreadDf =
                            oas_.empty()
                                ? 1.0
                                : std::exp(
                                      -oas_->value() *
                                      (model_->termStructure()->dayCounter()
                                           .yearFraction(
                                                event0,
                                                arguments_.leg1PayDates[j])));
                        Real rate = arguments_.leg1Spreads[j] + (ibor1 != NULL
                                        ? model_->forwardRate(
                                              arguments_.leg1FixingDates[j],
                                              event0, z[k], ibor1)
                                        : model_->swapRate(
                                              arguments_.leg1FixingDates[j],
                                              cms1->tenor(), event0, z[k], cms1));
                        if (arguments_.leg1CappedRates[j] != Null<Real>())
                            rate =
                                std::min(arguments_.leg1CappedRates[j], rate);
                        if (arguments_.leg1FlooredRates[j] != Null<Real>())
                            rate =
                                std::max(arguments_.leg1FlooredRates[j], rate);
                        npv0a[k] -=
                            rate * arguments_.leg1AccrualTimes[j] *
                            model_->zerobond(arguments_.leg1PayDates[j], event0,
                                             z[k], discountCurve_) /
                            model_->numeraire(event0Time, z[k],
                                              discountCurve_) *
                            zSpreadDf;
                    }
                    if (isLeg2Fixing) { // if event is a fixing date and
                                        // exercise date,
                        // the coupon is part of the exercise into right (by
                        // definition)
                        Size j = std::find(arguments_.leg2FixingDates.begin(),
                                           arguments_.leg2FixingDates.end(),
                                           event0) -
                                 arguments_.leg2FixingDates.begin();
                        std::exp(
                            -oas_->value() *
                            (model_->termStructure()->dayCounter().yearFraction(
                                event0, arguments_.leg2PayDates[j])));
                        Real zSpreadDf =
                            oas_.empty()
                                ? 1.0
                                : std::exp(
                                      -oas_->value() *
                                      (model_->termStructure()->dayCounter()
                                           .yearFraction(
                                                event0,
                                                arguments_.leg2PayDates[j])));
                        Real rate = arguments_.leg2Spreads[j] + (ibor2 != NULL
                                        ? model_->forwardRate(
                                              arguments_.leg2FixingDates[j],
                                              event0, z[k], ibor2)
                                        : model_->swapRate(
                                              arguments_.leg2FixingDates[j],
                                              cms2->tenor(), event0, z[k], cms2));
                        if (arguments_.leg2CappedRates[j] != Null<Real>())
                            rate =
                                std::min(arguments_.leg2CappedRates[j], rate);
                        if (arguments_.leg2FlooredRates[j] != Null<Real>())
                            rate =
                                std::max(arguments_.leg2FlooredRates[j], rate);
                        npv0a[k] -=
                            rate * arguments_.leg2AccrualTimes[j] *
                            model_->zerobond(arguments_.leg2PayDates[j], event0,
                                             z[k], discountCurve_) /
                            model_->numeraire(event0Time, z[k],
                                              discountCurve_) *
                            zSpreadDf;
                    }
                    if (isExercise) {
                        Size j = std::find(arguments_.exercise->dates().begin(),
                                           arguments_.exercise->dates().end(),
                                           event0) -
                                 arguments_.exercise->dates().begin();
                        Real rebate = arguments_.exercise->rebate(j);
                        Date rebateDate =
                            arguments_.exercise->rebatePaymentDate(idx);
                        Real zSpreadDf =
                            oas_.empty()
                                ? 1.0
                                : std::exp(
                                      -oas_->value() *
                                      (model_->termStructure()->dayCounter()
                                           .yearFraction(event0, rebateDate)));
                        npv0[k] = std::max(
                            npv0[k],
                            (type == Option::Call ? 1.0 : -1.0) * npv0a[k] +
                                rebate * model_->zerobond(rebateDate, event0) *
                                    zSpreadDf /
                                    model_->numeraire(event0Time, z[k],
                                                      discountCurve_));
                    }
                }
            }

            npv1.swap(npv0);
            npv1a.swap(npv0a);
            event1 = event0;
            event1Time = event0Time;

        } while (--idx >= -1);

        results_.value = npv1[0] * model_->numeraire(0.0, 0.0, discountCurve_);
        results_.additionalResults["underlyingValue"] =
            npv1a[0] * model_->numeraire(0.0, 0.0, discountCurve_);
    }
}

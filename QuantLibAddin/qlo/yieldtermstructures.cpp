/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2005 Aurelien Chanudet
 Copyright (C) 2005 Plamen Neykov
 Copyright (C) 2005, 2006 Eric Ehlers
 Copyright (C) 2006, 2007 Ferdinando Ametrano

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

#ifdef HAVE_CONFIG_H
#include <qlo/config.hpp>
#endif

#include <qlo/yieldtermstructures.hpp>
#include <qlo/ratehelpers.hpp>

#include <ql/time/date.hpp>
#include <ql/termstructures/yield/discountcurve.hpp>
#include <ql/termstructures/yield/forwardcurve.hpp>
#include <ql/termstructures/yield/zerocurve.hpp>
#include <ql/termstructures/yield/impliedtermstructure.hpp>
#include <ql/termstructures/yield/flatforward.hpp>
#include <ql/termstructures/yield/forwardspreadedtermstructure.hpp>
#include <ql/math/interpolations/cubicinterpolation.hpp>
#include <ql/math/interpolations/forwardflatinterpolation.hpp>
#include <ql/math/interpolations/backwardflatinterpolation.hpp>

#include <boost/algorithm/string/case_conv.hpp>

using boost::algorithm::to_upper_copy;
using boost::shared_ptr;

using ObjectHandler::ValueObject;

using QuantLib::CubicInterpolation;
using QuantLib::InterpolatedDiscountCurve;
using QuantLib::InterpolatedZeroCurve;
using QuantLib::InterpolatedForwardCurve;

namespace QuantLibAddin {

    DiscountCurve::DiscountCurve(
        const shared_ptr<ValueObject>& prop,
        const std::vector<QuantLib::Date>& dates,
        const std::vector<QuantLib::DiscountFactor>& dfs,
        const QuantLib::DayCounter& dayCounter,
        bool perm) : YieldTermStructure(prop, perm)
    {
        QL_REQUIRE(!dates.empty(), "no input dates given");
        libraryObject_ = shared_ptr<QuantLib::Extrapolator>(new
            QuantLib::DiscountCurve(dates, dfs, dayCounter));
    }

    ZeroCurve::ZeroCurve(const shared_ptr<ValueObject>& prop,
                         const std::vector<QuantLib::Date>& dates,
                         const std::vector<QuantLib::Rate>& zeroRates,
                         const QuantLib::DayCounter& dayCounter,
                         bool perm) : YieldTermStructure(prop, perm)
    {
        QL_REQUIRE(!dates.empty(), "no input dates given");
        libraryObject_ = shared_ptr<QuantLib::Extrapolator>(new
            QuantLib::ZeroCurve(dates, zeroRates, dayCounter));
    }

    ForwardCurve::ForwardCurve(const shared_ptr<ValueObject>& prop,
                               const std::vector<QuantLib::Date>& dates,
                               const std::vector<QuantLib::Rate>& fwdRates,
                               const QuantLib::DayCounter& dayCounter,
                               bool perm) : YieldTermStructure(prop, perm)
    {
        QL_REQUIRE(!dates.empty(), "no input dates given");
        libraryObject_ = shared_ptr<QuantLib::Extrapolator>(new
            QuantLib::ForwardCurve(dates, fwdRates, dayCounter));
    }

    FlatForward::FlatForward(const shared_ptr<ValueObject>& prop,
                             QuantLib::Natural nDays,
                             const QuantLib::Calendar& calendar,
                             const QuantLib::Handle<QuantLib::Quote>& forward,
                             const QuantLib::DayCounter& dayCounter,
                             QuantLib::Compounding compounding,
                             QuantLib::Frequency frequency,
                             bool perm)
    : YieldTermStructure(prop, perm)
    {
        libraryObject_ = shared_ptr<QuantLib::Extrapolator>(new
            QuantLib::FlatForward(nDays, calendar, forward, dayCounter,
                                  compounding, frequency));
    }

    ForwardSpreadedTermStructure::ForwardSpreadedTermStructure(
            const shared_ptr<ValueObject>& prop,
            const QuantLib::Handle<QuantLib::YieldTermStructure>& hYTS,
            const QuantLib::Handle<QuantLib::Quote>& spread,
            bool perm) : YieldTermStructure(prop, perm) {

        libraryObject_ = shared_ptr<QuantLib::Extrapolator>(new
            QuantLib::ForwardSpreadedTermStructure(hYTS, spread));
    }


    ImpliedTermStructure::ImpliedTermStructure(
            const shared_ptr<ValueObject>& prop,
            const QuantLib::Handle<QuantLib::YieldTermStructure>& hYTS,
            const QuantLib::Date& referenceDate,
            bool perm)
    : YieldTermStructure(prop, perm)
    {
        libraryObject_ = shared_ptr<QuantLib::Extrapolator>(new
            QuantLib::ImpliedTermStructure(hYTS, referenceDate));
    }


    InterpolatedYieldCurve::InterpolatedYieldCurve(
            const shared_ptr<ValueObject>& prop,
            const std::vector<QuantLib::Date>& dates,
            const std::vector<QuantLib::Real>& data,
            const QuantLib::Calendar& calendar,
            const QuantLib::DayCounter& dayCounter,
            const std::vector<QuantLib::Handle<QuantLib::Quote> >& jumps,
            const std::vector<QuantLib::Date>& jumpDates,
            const std::string& traitsID,
            const std::string& interpolatorID,
            bool perm)
    : YieldTermStructure(prop, perm),
      traitsID_(to_upper_copy(traitsID)),
      interpolatorID_(to_upper_copy(interpolatorID))
    {
        if (traitsID_=="DISCOUNT") {
            if (interpolatorID_=="BACKWARDFLAT") {
                libraryObject_ = shared_ptr<QuantLib::Extrapolator>(new
                    InterpolatedDiscountCurve<QuantLib::BackwardFlat>(
                        dates, data, dayCounter, calendar, jumps, jumpDates));
            } else if (interpolatorID_=="FORWARDFLAT") {
                libraryObject_ = shared_ptr<QuantLib::Extrapolator>(new
                    InterpolatedDiscountCurve<QuantLib::ForwardFlat>(
                        dates, data, dayCounter, calendar, jumps, jumpDates));
            } else if (interpolatorID_=="LINEAR") {
                libraryObject_ = shared_ptr<QuantLib::Extrapolator>(new
                    InterpolatedDiscountCurve<QuantLib::Linear>(
                        dates, data, dayCounter, calendar, jumps, jumpDates));
            } else if (interpolatorID_=="LOGLINEAR") {
                libraryObject_ = shared_ptr<QuantLib::Extrapolator>(new
                    InterpolatedDiscountCurve<QuantLib::LogLinear>(
                        dates, data, dayCounter, calendar, jumps, jumpDates));
            } else if (interpolatorID_=="CUBICNATURALSPLINE") {
                libraryObject_ = shared_ptr<QuantLib::Extrapolator>(new
                    InterpolatedDiscountCurve<QuantLib::Cubic>(
                        dates, data, dayCounter, calendar, jumps, jumpDates,
                        QuantLib::Cubic(CubicInterpolation::Spline, false,
                                        CubicInterpolation::SecondDerivative, 0.0,
                                        CubicInterpolation::SecondDerivative, 0.0)));
            } else if (interpolatorID_=="LOGCUBICNATURALSPLINE") {
                libraryObject_ = shared_ptr<QuantLib::Extrapolator>(new
                    InterpolatedDiscountCurve<QuantLib::LogCubic>(
                        dates, data, dayCounter, calendar, jumps, jumpDates,
                        QuantLib::LogCubic(CubicInterpolation::Spline, false,
                                           CubicInterpolation::SecondDerivative, 0.0,
                                           CubicInterpolation::SecondDerivative, 0.0)));
            } else if (interpolatorID_=="MONOTONICCUBICNATURALSPLINE") {
                libraryObject_ = shared_ptr<QuantLib::Extrapolator>(new
                    InterpolatedDiscountCurve<QuantLib::Cubic>(
                        dates, data, dayCounter, calendar, jumps, jumpDates,
                        QuantLib::Cubic(CubicInterpolation::Spline, true,
                                        CubicInterpolation::SecondDerivative, 0.0,
                                        CubicInterpolation::SecondDerivative, 0.0)));
            } else if (interpolatorID_=="MONOTONICLOGCUBICNATURALSPLINE") {
                libraryObject_ = shared_ptr<QuantLib::Extrapolator>(new
                    InterpolatedDiscountCurve<QuantLib::LogCubic>(
                        dates, data, dayCounter, calendar, jumps, jumpDates,
                        QuantLib::LogCubic(CubicInterpolation::Spline, true,
                                           CubicInterpolation::SecondDerivative, 0.0,
                                           CubicInterpolation::SecondDerivative, 0.0)));
            } else if (interpolatorID_=="KRUGERCUBIC") {
                libraryObject_ = shared_ptr<QuantLib::Extrapolator>(new
                    InterpolatedDiscountCurve<QuantLib::Cubic>(
                        dates, data, dayCounter, calendar, jumps, jumpDates,
                        QuantLib::Cubic(CubicInterpolation::Kruger)));
            } else if (interpolatorID_=="KRUGERLOGCUBIC") {
                libraryObject_ = shared_ptr<QuantLib::Extrapolator>(new
                    InterpolatedDiscountCurve<QuantLib::LogCubic>(
                        dates, data, dayCounter, calendar, jumps, jumpDates,
                        QuantLib::LogCubic(CubicInterpolation::Kruger)));
            } else if (interpolatorID_=="FRITSCHBUTLANDCUBIC") {
                libraryObject_ = shared_ptr<QuantLib::Extrapolator>(new
                    InterpolatedDiscountCurve<QuantLib::Cubic>(
                        dates, data, dayCounter, calendar, jumps, jumpDates,
                        QuantLib::Cubic(CubicInterpolation::FritschButland)));
            } else if (interpolatorID_=="FRITSCHBUTLANDLOGCUBIC") {
                libraryObject_ = shared_ptr<QuantLib::Extrapolator>(new
                    InterpolatedDiscountCurve<QuantLib::LogCubic>(
                        dates, data, dayCounter, calendar, jumps, jumpDates,
                        QuantLib::LogCubic(CubicInterpolation::FritschButland)));
            } else if (interpolatorID_=="PARABOLIC") {
                libraryObject_ = shared_ptr<QuantLib::Extrapolator>(new
                    InterpolatedDiscountCurve<QuantLib::Cubic>(
                        dates, data, dayCounter, calendar, jumps, jumpDates,
                        QuantLib::Cubic(CubicInterpolation::Parabolic, false)));
            } else if (interpolatorID_=="LOGPARABOLIC") {
                libraryObject_ = shared_ptr<QuantLib::Extrapolator>(new
                    InterpolatedDiscountCurve<QuantLib::LogCubic>(
                        dates, data, dayCounter, calendar, jumps, jumpDates,
                        QuantLib::LogCubic(CubicInterpolation::Parabolic, false)));
            } else if (interpolatorID_=="MONOTONICPARABOLIC") {
                libraryObject_ = shared_ptr<QuantLib::Extrapolator>(new
                    InterpolatedDiscountCurve<QuantLib::Cubic>(
                        dates, data, dayCounter, calendar, jumps, jumpDates,
                        QuantLib::Cubic(CubicInterpolation::Parabolic, true)));
            } else if (interpolatorID_=="MONOTONICLOGPARABOLIC") {
                libraryObject_ = shared_ptr<QuantLib::Extrapolator>(new
                    InterpolatedDiscountCurve<QuantLib::LogCubic>(
                        dates, data, dayCounter, calendar, jumps, jumpDates,
                        QuantLib::LogCubic(CubicInterpolation::Parabolic, true)));
            } else
                QL_FAIL("unknown interpolatorID: " << interpolatorID_);
        } else if (traitsID_=="ZEROYIELD") {
            if (interpolatorID_=="BACKWARDFLAT") {
                libraryObject_ = shared_ptr<QuantLib::Extrapolator>(new
                    InterpolatedZeroCurve<QuantLib::BackwardFlat>(
                        dates, data, dayCounter, calendar, jumps, jumpDates));
            } else if (interpolatorID_=="FORWARDFLAT") {
                libraryObject_ = shared_ptr<QuantLib::Extrapolator>(new
                    InterpolatedZeroCurve<QuantLib::ForwardFlat>(
                        dates, data, dayCounter, calendar, jumps, jumpDates));
            } else if (interpolatorID_=="LINEAR") {
                libraryObject_ = shared_ptr<QuantLib::Extrapolator>(new
                    InterpolatedZeroCurve<QuantLib::Linear>(
                        dates, data, dayCounter, calendar, jumps, jumpDates));
            } else if (interpolatorID_=="LOGLINEAR") {
                libraryObject_ = shared_ptr<QuantLib::Extrapolator>(new
                    InterpolatedZeroCurve<QuantLib::LogLinear>(
                        dates, data, dayCounter, calendar, jumps, jumpDates));
            } else if (interpolatorID_=="CUBICNATURALSPLINE") {
                libraryObject_ = shared_ptr<QuantLib::Extrapolator>(new
                    InterpolatedZeroCurve<QuantLib::Cubic>(
                        dates, data, dayCounter, calendar, jumps, jumpDates,
                        QuantLib::Cubic(CubicInterpolation::Spline, false,
                                        CubicInterpolation::SecondDerivative, 0.0,
                                        CubicInterpolation::SecondDerivative, 0.0)));
            } else if (interpolatorID_=="LOGCUBICNATURALSPLINE") {
                libraryObject_ = shared_ptr<QuantLib::Extrapolator>(new
                    InterpolatedZeroCurve<QuantLib::LogCubic>(
                        dates, data, dayCounter, calendar, jumps, jumpDates,
                        QuantLib::LogCubic(CubicInterpolation::Spline, false,
                                           CubicInterpolation::SecondDerivative, 0.0,
                                           CubicInterpolation::SecondDerivative, 0.0)));
            } else if (interpolatorID_=="MONOTONICCUBICNATURALSPLINE") {
                libraryObject_ = shared_ptr<QuantLib::Extrapolator>(new
                    InterpolatedZeroCurve<QuantLib::Cubic>(
                        dates, data, dayCounter, calendar, jumps, jumpDates,
                        QuantLib::Cubic(CubicInterpolation::Spline, true,
                                        CubicInterpolation::SecondDerivative, 0.0,
                                        CubicInterpolation::SecondDerivative, 0.0)));
            } else if (interpolatorID_=="MONOTONICLOGCUBICNATURALSPLINE") {
                libraryObject_ = shared_ptr<QuantLib::Extrapolator>(new
                    InterpolatedZeroCurve<QuantLib::LogCubic>(
                        dates, data, dayCounter, calendar, jumps, jumpDates,
                        QuantLib::LogCubic(CubicInterpolation::Spline, true,
                                           CubicInterpolation::SecondDerivative, 0.0,
                                           CubicInterpolation::SecondDerivative, 0.0)));
            } else if (interpolatorID_=="KRUGERCUBIC") {
                libraryObject_ = shared_ptr<QuantLib::Extrapolator>(new
                    InterpolatedZeroCurve<QuantLib::Cubic>(
                        dates, data, dayCounter, calendar, jumps, jumpDates,
                        QuantLib::Cubic(CubicInterpolation::Kruger)));
            } else if (interpolatorID_=="KRUGERLOGCUBIC") {
                libraryObject_ = shared_ptr<QuantLib::Extrapolator>(new
                    InterpolatedZeroCurve<QuantLib::LogCubic>(
                        dates, data, dayCounter, calendar, jumps, jumpDates,
                        QuantLib::LogCubic(CubicInterpolation::Kruger)));
            } else if (interpolatorID_=="FRITSCHBUTLANDCUBIC") {
                libraryObject_ = shared_ptr<QuantLib::Extrapolator>(new
                    InterpolatedZeroCurve<QuantLib::Cubic>(
                        dates, data, dayCounter, calendar, jumps, jumpDates,
                        QuantLib::Cubic(CubicInterpolation::FritschButland)));
            } else if (interpolatorID_=="FRITSCHBUTLANDLOGCUBIC") {
                libraryObject_ = shared_ptr<QuantLib::Extrapolator>(new
                    InterpolatedZeroCurve<QuantLib::LogCubic>(
                        dates, data, dayCounter, calendar, jumps, jumpDates,
                        QuantLib::LogCubic(CubicInterpolation::FritschButland)));
            } else if (interpolatorID_=="PARABOLIC") {
                libraryObject_ = shared_ptr<QuantLib::Extrapolator>(new
                    InterpolatedZeroCurve<QuantLib::Cubic>(
                        dates, data, dayCounter, calendar, jumps, jumpDates,
                        QuantLib::Cubic(CubicInterpolation::Parabolic, false)));
            } else if (interpolatorID_=="LOGPARABOLIC") {
                libraryObject_ = shared_ptr<QuantLib::Extrapolator>(new
                    InterpolatedZeroCurve<QuantLib::LogCubic>(
                        dates, data, dayCounter, calendar, jumps, jumpDates,
                        QuantLib::LogCubic(CubicInterpolation::Parabolic, false)));
            } else if (interpolatorID_=="MONOTONICPARABOLIC") {
                libraryObject_ = shared_ptr<QuantLib::Extrapolator>(new
                    InterpolatedZeroCurve<QuantLib::Cubic>(
                        dates, data, dayCounter, calendar, jumps, jumpDates,
                        QuantLib::Cubic(CubicInterpolation::Parabolic, true)));
            } else if (interpolatorID_=="MONOTONICLOGPARABOLIC") {
                libraryObject_ = shared_ptr<QuantLib::Extrapolator>(new
                    InterpolatedZeroCurve<QuantLib::LogCubic>(
                        dates, data, dayCounter, calendar, jumps, jumpDates,
                        QuantLib::LogCubic(CubicInterpolation::Parabolic, true)));
            } else
                QL_FAIL("unknown interpolatorID: " << interpolatorID_);
        } else if (traitsID_=="FORWARDRATE") {
            if (interpolatorID_=="BACKWARDFLAT") {
                libraryObject_ = shared_ptr<QuantLib::Extrapolator>(new
                    InterpolatedForwardCurve<QuantLib::BackwardFlat>(
                        dates, data, dayCounter, calendar, jumps, jumpDates));
            } else if (interpolatorID_=="FORWARDFLAT") {
                libraryObject_ = shared_ptr<QuantLib::Extrapolator>(new
                    InterpolatedForwardCurve<QuantLib::ForwardFlat>(
                        dates, data, dayCounter, calendar, jumps, jumpDates));
            } else if (interpolatorID_=="LINEAR") {
                libraryObject_ = shared_ptr<QuantLib::Extrapolator>(new
                    InterpolatedForwardCurve<QuantLib::Linear>(
                        dates, data, dayCounter, calendar, jumps, jumpDates));
            } else if (interpolatorID_=="LOGLINEAR") {
                libraryObject_ = shared_ptr<QuantLib::Extrapolator>(new
                    InterpolatedForwardCurve<QuantLib::LogLinear>(
                        dates, data, dayCounter, calendar, jumps, jumpDates));
            } else if (interpolatorID_=="CUBICNATURALSPLINE") {
                libraryObject_ = shared_ptr<QuantLib::Extrapolator>(new
                    InterpolatedForwardCurve<QuantLib::Cubic>(
                        dates, data, dayCounter, calendar, jumps, jumpDates,
                        QuantLib::Cubic(CubicInterpolation::Spline, false,
                                        CubicInterpolation::SecondDerivative, 0.0,
                                        CubicInterpolation::SecondDerivative, 0.0)));
            } else if (interpolatorID_=="LOGCUBICNATURALSPLINE") {
                libraryObject_ = shared_ptr<QuantLib::Extrapolator>(new
                    InterpolatedForwardCurve<QuantLib::LogCubic>(
                        dates, data, dayCounter, calendar, jumps, jumpDates,
                        QuantLib::LogCubic(CubicInterpolation::Spline, false,
                                           CubicInterpolation::SecondDerivative, 0.0,
                                           CubicInterpolation::SecondDerivative, 0.0)));
            } else if (interpolatorID_=="MONOTONICCUBICNATURALSPLINE") {
                libraryObject_ = shared_ptr<QuantLib::Extrapolator>(new
                    InterpolatedForwardCurve<QuantLib::Cubic>(
                        dates, data, dayCounter, calendar, jumps, jumpDates,
                        QuantLib::Cubic(CubicInterpolation::Spline, true,
                                        CubicInterpolation::SecondDerivative, 0.0,
                                        CubicInterpolation::SecondDerivative, 0.0)));
            } else if (interpolatorID_=="MONOTONICLOGCUBICNATURALSPLINE") {
                libraryObject_ = shared_ptr<QuantLib::Extrapolator>(new
                    InterpolatedForwardCurve<QuantLib::LogCubic>(
                        dates, data, dayCounter, calendar, jumps, jumpDates,
                        QuantLib::LogCubic(CubicInterpolation::Spline, true,
                                           CubicInterpolation::SecondDerivative, 0.0,
                                           CubicInterpolation::SecondDerivative, 0.0)));
            } else if (interpolatorID_=="KRUGERCUBIC") {
                libraryObject_ = shared_ptr<QuantLib::Extrapolator>(new
                    InterpolatedForwardCurve<QuantLib::Cubic>(
                        dates, data, dayCounter, calendar, jumps, jumpDates,
                        QuantLib::Cubic(CubicInterpolation::Kruger)));
            } else if (interpolatorID_=="KRUGERLOGCUBIC") {
                libraryObject_ = shared_ptr<QuantLib::Extrapolator>(new
                    InterpolatedForwardCurve<QuantLib::LogCubic>(
                        dates, data, dayCounter, calendar, jumps, jumpDates,
                        QuantLib::LogCubic(CubicInterpolation::Kruger)));
            } else if (interpolatorID_=="FRITSCHBUTLANDCUBIC") {
                libraryObject_ = shared_ptr<QuantLib::Extrapolator>(new
                    InterpolatedForwardCurve<QuantLib::Cubic>(
                        dates, data, dayCounter, calendar, jumps, jumpDates,
                        QuantLib::Cubic(CubicInterpolation::FritschButland)));
            } else if (interpolatorID_=="FRITSCHBUTLANDLOGCUBIC") {
                libraryObject_ = shared_ptr<QuantLib::Extrapolator>(new
                    InterpolatedForwardCurve<QuantLib::LogCubic>(
                        dates, data, dayCounter, calendar, jumps, jumpDates,
                        QuantLib::LogCubic(CubicInterpolation::FritschButland)));
            } else if (interpolatorID_=="PARABOLIC") {
                libraryObject_ = shared_ptr<QuantLib::Extrapolator>(new
                    InterpolatedForwardCurve<QuantLib::Cubic>(
                        dates, data, dayCounter, calendar, jumps, jumpDates,
                        QuantLib::Cubic(CubicInterpolation::Parabolic)));
            } else if (interpolatorID_=="LOGPARABOLIC") {
                libraryObject_ = shared_ptr<QuantLib::Extrapolator>(new
                    InterpolatedForwardCurve<QuantLib::LogCubic>(
                        dates, data, dayCounter, calendar, jumps, jumpDates,
                        QuantLib::LogCubic(CubicInterpolation::Parabolic, false)));
            } else if (interpolatorID_=="MONOTONICPARABOLIC") {
                libraryObject_ = shared_ptr<QuantLib::Extrapolator>(new
                    InterpolatedForwardCurve<QuantLib::Cubic>(
                        dates, data, dayCounter, calendar, jumps, jumpDates,
                        QuantLib::Cubic(CubicInterpolation::Parabolic, true)));
            } else if (interpolatorID_=="MONOTONICLOGPARABOLIC") {
                libraryObject_ = shared_ptr<QuantLib::Extrapolator>(new
                    InterpolatedForwardCurve<QuantLib::LogCubic>(
                        dates, data, dayCounter, calendar, jumps, jumpDates,
                        QuantLib::LogCubic(CubicInterpolation::Parabolic, true)));
            } else
                QL_FAIL("unknown interpolatorID: " << interpolatorID_);
        } else
            QL_FAIL("unknown traitsID: " << traitsID_);
 
    }

    #define RESOLVE_TEMPLATE(NAME) \
        if (traitsID_=="DISCOUNT") { \
            if (interpolatorID_=="BACKWARDFLAT") { \
                return boost::dynamic_pointer_cast<InterpolatedDiscountCurve<QuantLib::BackwardFlat> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="FORWARDFLAT") { \
                return boost::dynamic_pointer_cast<InterpolatedDiscountCurve<QuantLib::ForwardFlat> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="LINEAR") { \
                return boost::dynamic_pointer_cast<InterpolatedDiscountCurve<QuantLib::Linear> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="LOGLINEAR") { \
                return boost::dynamic_pointer_cast<InterpolatedDiscountCurve<QuantLib::LogLinear> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="CUBICNATURALSPLINE") { \
                return boost::dynamic_pointer_cast<InterpolatedDiscountCurve<QuantLib::Cubic> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="LOGCUBICNATURALSPLINE") { \
                return boost::dynamic_pointer_cast<InterpolatedDiscountCurve<QuantLib::LogCubic> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="MONOTONICCUBICNATURALSPLINE") { \
                return boost::dynamic_pointer_cast<InterpolatedDiscountCurve<QuantLib::Cubic> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="MONOTONICLOGCUBICNATURALSPLINE") { \
                return boost::dynamic_pointer_cast<InterpolatedDiscountCurve<QuantLib::LogCubic> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="KRUGERCUBIC") { \
                return boost::dynamic_pointer_cast<InterpolatedDiscountCurve<QuantLib::Cubic> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="KRUGERLOGCUBIC") { \
                return boost::dynamic_pointer_cast<InterpolatedDiscountCurve<QuantLib::LogCubic> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="FRITSCHBUTLANDCUBIC") { \
                return boost::dynamic_pointer_cast<InterpolatedDiscountCurve<QuantLib::Cubic> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="FRITSCHBUTLANDLOGCUBIC") { \
                return boost::dynamic_pointer_cast<InterpolatedDiscountCurve<QuantLib::LogCubic> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="PARABOLIC") { \
                return boost::dynamic_pointer_cast<InterpolatedDiscountCurve<QuantLib::Cubic> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="LOGPARABOLIC") { \
                return boost::dynamic_pointer_cast<InterpolatedDiscountCurve<QuantLib::LogCubic> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="MONOTONICPARABOLIC") { \
                return boost::dynamic_pointer_cast<InterpolatedDiscountCurve<QuantLib::Cubic> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="MONOTONICLOGPARABOLIC") { \
                return boost::dynamic_pointer_cast<InterpolatedDiscountCurve<QuantLib::LogCubic> >(libraryObject_)->NAME(); \
            } else \
                QL_FAIL("unknown interpolatorID: " << interpolatorID_); \
        } else if (traitsID_=="ZEROYIELD") { \
            if (interpolatorID_=="BACKWARDFLAT") { \
                return boost::dynamic_pointer_cast<InterpolatedZeroCurve<QuantLib::BackwardFlat> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="FORWARDFLAT") { \
                return boost::dynamic_pointer_cast<InterpolatedZeroCurve<QuantLib::ForwardFlat> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="LINEAR") { \
                return boost::dynamic_pointer_cast<InterpolatedZeroCurve<QuantLib::Linear> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="LOGLINEAR") { \
                return boost::dynamic_pointer_cast<InterpolatedZeroCurve<QuantLib::LogLinear> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="CUBICNATURALSPLINE") { \
                return boost::dynamic_pointer_cast<InterpolatedZeroCurve<QuantLib::Cubic> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="LOGCUBICNATURALSPLINE") { \
                return boost::dynamic_pointer_cast<InterpolatedZeroCurve<QuantLib::LogCubic> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="MONOTONICCUBICNATURALSPLINE") { \
                return boost::dynamic_pointer_cast<InterpolatedZeroCurve<QuantLib::Cubic> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="MONOTONICLOGCUBICNATURALSPLINE") { \
                return boost::dynamic_pointer_cast<InterpolatedZeroCurve<QuantLib::LogCubic> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="KRUGERCUBIC") { \
                return boost::dynamic_pointer_cast<InterpolatedZeroCurve<QuantLib::Cubic> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="KRUGERLOGCUBIC") { \
                return boost::dynamic_pointer_cast<InterpolatedZeroCurve<QuantLib::LogCubic> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="FRITSCHBUTLANDCUBIC") { \
                return boost::dynamic_pointer_cast<InterpolatedZeroCurve<QuantLib::Cubic> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="FRITSCHBUTLANDLOGCUBIC") { \
                return boost::dynamic_pointer_cast<InterpolatedZeroCurve<QuantLib::LogCubic> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="PARABOLIC") { \
                return boost::dynamic_pointer_cast<InterpolatedZeroCurve<QuantLib::Cubic> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="LOGPARABOLIC") { \
                return boost::dynamic_pointer_cast<InterpolatedZeroCurve<QuantLib::LogCubic> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="MONOTONICPARABOLIC") { \
                return boost::dynamic_pointer_cast<InterpolatedZeroCurve<QuantLib::Cubic> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="MONOTONICLOGPARABOLIC") { \
                return boost::dynamic_pointer_cast<InterpolatedZeroCurve<QuantLib::LogCubic> >(libraryObject_)->NAME(); \
            } else \
                QL_FAIL("unknown interpolatorID: " << interpolatorID_); \
        } else if (traitsID_=="FORWARDRATE") { \
            if (interpolatorID_=="BACKWARDFLAT") { \
                return boost::dynamic_pointer_cast<InterpolatedForwardCurve<QuantLib::BackwardFlat> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="FORWARDFLAT") { \
                return boost::dynamic_pointer_cast<InterpolatedForwardCurve<QuantLib::ForwardFlat> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="LINEAR") { \
                return boost::dynamic_pointer_cast<InterpolatedForwardCurve<QuantLib::Linear> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="LOGLINEAR") { \
                return boost::dynamic_pointer_cast<InterpolatedForwardCurve<QuantLib::LogLinear> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="CUBICNATURALSPLINE") { \
                return boost::dynamic_pointer_cast<InterpolatedForwardCurve<QuantLib::Cubic> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="LOGCUBICNATURALSPLINE") { \
                return boost::dynamic_pointer_cast<InterpolatedForwardCurve<QuantLib::LogCubic> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="MONOTONICCUBICNATURALSPLINE") { \
                return boost::dynamic_pointer_cast<InterpolatedForwardCurve<QuantLib::Cubic> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="MONOTONICLOGCUBICNATURALSPLINE") { \
                return boost::dynamic_pointer_cast<InterpolatedForwardCurve<QuantLib::LogCubic> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="KRUGERCUBIC") { \
                return boost::dynamic_pointer_cast<InterpolatedForwardCurve<QuantLib::Cubic> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="KRUGERLOGCUBIC") { \
                return boost::dynamic_pointer_cast<InterpolatedForwardCurve<QuantLib::LogCubic> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="FRITSCHBUTLANDCUBIC") { \
                return boost::dynamic_pointer_cast<InterpolatedForwardCurve<QuantLib::Cubic> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="FRITSCHBUTLANDLOGCUBIC") { \
                return boost::dynamic_pointer_cast<InterpolatedForwardCurve<QuantLib::LogCubic> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="PARABOLIC") { \
                return boost::dynamic_pointer_cast<InterpolatedForwardCurve<QuantLib::Cubic> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="LOGPARABOLIC") { \
                return boost::dynamic_pointer_cast<InterpolatedForwardCurve<QuantLib::LogCubic> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="MONOTONICPARABOLIC") { \
                return boost::dynamic_pointer_cast<InterpolatedForwardCurve<QuantLib::Cubic> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="MONOTONICLOGPARABOLIC") { \
                return boost::dynamic_pointer_cast<InterpolatedForwardCurve<QuantLib::LogCubic> >(libraryObject_)->NAME(); \
            } else \
                QL_FAIL("unknown interpolatorID: " << interpolatorID_); \
        } else \
            QL_FAIL("unknown traitsID: " << traitsID_);

    const std::vector<QuantLib::Time>& InterpolatedYieldCurve::times() const {
        RESOLVE_TEMPLATE(times)
    }

    const std::vector<QuantLib::Date>& InterpolatedYieldCurve::dates() const {
        RESOLVE_TEMPLATE(dates)
    }

    const std::vector<QuantLib::Real>& InterpolatedYieldCurve::data() const {
        RESOLVE_TEMPLATE(data)
    }

    const std::vector<QuantLib::Time>& InterpolatedYieldCurve::jumpTimes() const {
        RESOLVE_TEMPLATE(jumpTimes)
    }

    const std::vector<QuantLib::Date>& InterpolatedYieldCurve::jumpDates() const {
        RESOLVE_TEMPLATE(jumpDates)
    }

    InterpolatedYieldCurve::InterpolatedYieldCurve(
            const shared_ptr<ValueObject>& prop,
            const std::string& traitsID,
            const std::string& interpolatorID,
            bool perm)
    : YieldTermStructure(prop, perm),
      traitsID_(to_upper_copy(traitsID)),
      interpolatorID_(to_upper_copy(interpolatorID))
    {
    }

    // Stream operator to write a InterpolatedYieldCurvePair to a stream - for logging / error handling.
    std::ostream &operator<<(std::ostream &out,
                             InterpolatedYieldCurvePair tokenPair)
    {
        out << "InterpolatedYieldCurve<";

        switch (tokenPair.first) {
            case InterpolatedYieldCurve::Discount:
                out << "<Discount, ";
                break;
            case InterpolatedYieldCurve::ForwardRate:
                out << "<ForwardRate, ";
                break;
            case InterpolatedYieldCurve::ZeroYield:
                out << "<ZeroYield, ";
                break;
            default:
                OH_FAIL("Unknown value for enumeration QuantLibAddin::InterpolatedYieldCurve::Traits");
        }

        switch (tokenPair.second) {
            case InterpolatedYieldCurve::BackwardFlat:
                out << "BackwardFlat>";
                break;
            case InterpolatedYieldCurve::ForwardFlat:
                out << "ForwardFlat>";
                break;
            case InterpolatedYieldCurve::Linear:
                out << "Linear>";
                break;
            case InterpolatedYieldCurve::LogLinear:
                out << "LogLinear>";
                break;
            case InterpolatedYieldCurve::CubicNaturalSpline:
                out << "CubicNaturalSpline>";
                break;
            case InterpolatedYieldCurve::LogCubicNaturalSpline:
                out << "LogCubicNaturalSpline>";
                break;
            case InterpolatedYieldCurve::MonotonicCubicNaturalSpline:
                out << "MonotonicCubicNaturalSpline>";
                break;
            case InterpolatedYieldCurve::MonotonicLogCubicNaturalSpline:
                out << "MonotonicLogCubicNaturalSpline>";
                break;
            case InterpolatedYieldCurve::KrugerCubic:
                out << "KrugerCubic>";
                break;
            case InterpolatedYieldCurve::KrugerLogCubic:
                out << "KrugerLogCubic>";
                break;
            case InterpolatedYieldCurve::FritschButlandCubic:
                out << "FritschButlandCubic>";
                break;
            case InterpolatedYieldCurve::FritschButlandLogCubic:
                out << "FritschButlandLogCubic>";
                break;
            case InterpolatedYieldCurve::Parabolic:
                out << "Parabolic>";
                break;
            case InterpolatedYieldCurve::LogParabolic:
                out << "LogParabolic>";
                break;
            case InterpolatedYieldCurve::MonotonicParabolic:
                out << "MonotonicParabolic>";
                break;
            case InterpolatedYieldCurve::MonotonicLogParabolic:
                out << "MonotonicLogParabolic>";
                break;
            default:
                OH_FAIL("Unknown value for enumeration QuantLibAddin::InterpolatedYieldCurve::Interpolator");
        }

        return out;
    }

}

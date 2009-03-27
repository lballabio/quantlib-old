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
#include <ql/math/interpolations/all.hpp>

#include <boost/algorithm/string/case_conv.hpp>

using boost::algorithm::to_upper_copy;

namespace QuantLibAddin {

    DiscountCurve::DiscountCurve(
        const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
        const std::vector<QuantLib::Date>& dates,
        const std::vector<QuantLib::DiscountFactor>& dfs,
        const QuantLib::DayCounter& dayCounter,
        bool permanent) : YieldTermStructure(properties, permanent)
    {
        QL_REQUIRE(!dates.empty(), "no input dates given");
        libraryObject_ = boost::shared_ptr<QuantLib::Extrapolator>(new
            QuantLib::DiscountCurve(dates, dfs, dayCounter));
    }

    ZeroCurve::ZeroCurve(const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
                         const std::vector<QuantLib::Date>& dates,
                         const std::vector<QuantLib::Rate>& zeroRates,
                         const QuantLib::DayCounter& dayCounter,
                         bool permanent) : YieldTermStructure(properties, permanent)
    {
        QL_REQUIRE(!dates.empty(), "no input dates given");
        libraryObject_ = boost::shared_ptr<QuantLib::Extrapolator>(new
            QuantLib::ZeroCurve(dates, zeroRates, dayCounter));
    }

    ForwardCurve::ForwardCurve(const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
                               const std::vector<QuantLib::Date>& dates,
                               const std::vector<QuantLib::Rate>& fwdRates,
                               const QuantLib::DayCounter& dayCounter,
                               bool permanent) : YieldTermStructure(properties, permanent)
    {
        QL_REQUIRE(!dates.empty(), "no input dates given");
        libraryObject_ = boost::shared_ptr<QuantLib::Extrapolator>(new
            QuantLib::ForwardCurve(dates, fwdRates, dayCounter));
    }

    FlatForward::FlatForward(const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
                             QuantLib::Natural nDays,
                             const QuantLib::Calendar& calendar,
                             const QuantLib::Handle<QuantLib::Quote>& forward,
                             const QuantLib::DayCounter& dayCounter,
                             QuantLib::Compounding compounding,
                             QuantLib::Frequency frequency,
                             bool permanent)
    : YieldTermStructure(properties, permanent)
    {
        libraryObject_ = boost::shared_ptr<QuantLib::Extrapolator>(new
            QuantLib::FlatForward(nDays, calendar, forward, dayCounter,
                                  compounding, frequency));
    }

    ForwardSpreadedTermStructure::ForwardSpreadedTermStructure(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const QuantLib::Handle<QuantLib::YieldTermStructure>& hYTS,
            const QuantLib::Handle<QuantLib::Quote>& spread,
            bool permanent) : YieldTermStructure(properties, permanent) {

        libraryObject_ = boost::shared_ptr<QuantLib::Extrapolator>(new
            QuantLib::ForwardSpreadedTermStructure(hYTS, spread));
    }


    ImpliedTermStructure::ImpliedTermStructure(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const QuantLib::Handle<QuantLib::YieldTermStructure>& hYTS,
            const QuantLib::Date& referenceDate,
            bool permanent)
    : YieldTermStructure(properties, permanent)
    {
        libraryObject_ = boost::shared_ptr<QuantLib::Extrapolator>(new
            QuantLib::ImpliedTermStructure(hYTS, referenceDate));
    }


    InterpolatedYieldCurve::InterpolatedYieldCurve(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const std::vector<QuantLib::Date>& dates,
            const std::vector<QuantLib::Real>& data,
            const QuantLib::Calendar& calendar,
            const QuantLib::DayCounter& dayCounter,
            const std::vector<QuantLib::Handle<QuantLib::Quote> >& jumps,
            const std::vector<QuantLib::Date>& jumpDates,
            const std::string& traitsID,
            const std::string& interpolatorID,
            bool permanent)
    : YieldTermStructure(properties, permanent),
      traitsID_(to_upper_copy(traitsID)),
      interpolatorID_(to_upper_copy(interpolatorID))
    {
        if (traitsID_=="DISCOUNT") {
            if (interpolatorID_=="BACKWARDFLAT") {
                libraryObject_ = boost::shared_ptr<QuantLib::Extrapolator>(new
                    QuantLib::InterpolatedDiscountCurve<QuantLib::BackwardFlat>(
                        dates, data, dayCounter, calendar));
            } else if (interpolatorID_=="FORWARDFLAT") {
                libraryObject_ = boost::shared_ptr<QuantLib::Extrapolator>(new
                    QuantLib::InterpolatedDiscountCurve<QuantLib::ForwardFlat>(
                        dates, data, dayCounter, calendar));
            } else if (interpolatorID_=="LINEAR") {
                libraryObject_ = boost::shared_ptr<QuantLib::Extrapolator>(new
                    QuantLib::InterpolatedDiscountCurve<QuantLib::Linear>(
                        dates, data, dayCounter, calendar));
            } else if (interpolatorID_=="LOGLINEAR") {
                libraryObject_ = boost::shared_ptr<QuantLib::Extrapolator>(new
                    QuantLib::InterpolatedDiscountCurve<QuantLib::LogLinear>(
                        dates, data, dayCounter, calendar));
            } else if (interpolatorID_=="CUBICNATURALSPLINE") {
                libraryObject_ = boost::shared_ptr<QuantLib::Extrapolator>(new
                    QuantLib::InterpolatedDiscountCurve<QuantLib::Cubic>(
                        dates, data, dayCounter, calendar,
                        QuantLib::Cubic(QuantLib::CubicInterpolation::Spline, false,
                                        QuantLib::CubicInterpolation::SecondDerivative, 0.0,
                                        QuantLib::CubicInterpolation::SecondDerivative, 0.0)));
            } else if (interpolatorID_=="LOGCUBICNATURALSPLINE") {
                libraryObject_ = boost::shared_ptr<QuantLib::Extrapolator>(new
                    QuantLib::InterpolatedDiscountCurve<QuantLib::LogCubic>(
                        dates, data, dayCounter, calendar,
                        QuantLib::LogCubic(QuantLib::CubicInterpolation::Spline, false,
                                           QuantLib::CubicInterpolation::SecondDerivative, 0.0,
                                           QuantLib::CubicInterpolation::SecondDerivative, 0.0)));
            } else if (interpolatorID_=="MONOTONICCUBICNATURALSPLINE") {
                libraryObject_ = boost::shared_ptr<QuantLib::Extrapolator>(new
                    QuantLib::InterpolatedDiscountCurve<QuantLib::Cubic>(
                        dates, data, dayCounter, calendar,
                        QuantLib::Cubic(QuantLib::CubicInterpolation::Spline, true,
                                        QuantLib::CubicInterpolation::SecondDerivative, 0.0,
                                        QuantLib::CubicInterpolation::SecondDerivative, 0.0)));
            } else if (interpolatorID_=="MONOTONICLOGCUBICNATURALSPLINE") {
                libraryObject_ = boost::shared_ptr<QuantLib::Extrapolator>(new
                    QuantLib::InterpolatedDiscountCurve<QuantLib::LogCubic>(
                        dates, data, dayCounter, calendar,
                        QuantLib::LogCubic(QuantLib::CubicInterpolation::Spline, true,
                                           QuantLib::CubicInterpolation::SecondDerivative, 0.0,
                                           QuantLib::CubicInterpolation::SecondDerivative, 0.0)));
            } else if (interpolatorID_=="KRUGERCUBIC") {
                libraryObject_ = boost::shared_ptr<QuantLib::Extrapolator>(new
                    QuantLib::InterpolatedDiscountCurve<QuantLib::Cubic>(
                        dates, data, dayCounter, calendar,
                        QuantLib::Cubic(QuantLib::CubicInterpolation::Kruger)));
            } else if (interpolatorID_=="KRUGERLOGCUBIC") {
                libraryObject_ = boost::shared_ptr<QuantLib::Extrapolator>(new
                    QuantLib::InterpolatedDiscountCurve<QuantLib::LogCubic>(
                        dates, data, dayCounter, calendar,
                        QuantLib::LogCubic(QuantLib::CubicInterpolation::Kruger)));
            } else if (interpolatorID_=="FRITSCHBUTLANDCUBIC") {
                libraryObject_ = boost::shared_ptr<QuantLib::Extrapolator>(new
                    QuantLib::InterpolatedDiscountCurve<QuantLib::Cubic>(
                        dates, data, dayCounter, calendar,
                        QuantLib::Cubic(QuantLib::CubicInterpolation::FritschButland)));
            } else if (interpolatorID_=="FRITSCHBUTLANDLOGCUBIC") {
                libraryObject_ = boost::shared_ptr<QuantLib::Extrapolator>(new
                    QuantLib::InterpolatedDiscountCurve<QuantLib::LogCubic>(
                        dates, data, dayCounter, calendar,
                        QuantLib::LogCubic(QuantLib::CubicInterpolation::FritschButland)));
            } else if (interpolatorID_=="PARABOLIC") {
                libraryObject_ = boost::shared_ptr<QuantLib::Extrapolator>(new
                    QuantLib::InterpolatedDiscountCurve<QuantLib::Cubic>(
                        dates, data, dayCounter, calendar,
                        QuantLib::Cubic(QuantLib::CubicInterpolation::Parabolic, false)));
            } else if (interpolatorID_=="LOGPARABOLIC") {
                libraryObject_ = boost::shared_ptr<QuantLib::Extrapolator>(new
                    QuantLib::InterpolatedDiscountCurve<QuantLib::LogCubic>(
                        dates, data, dayCounter, calendar,
                        QuantLib::LogCubic(QuantLib::CubicInterpolation::Parabolic, false)));
            } else if (interpolatorID_=="MONOTONICPARABOLIC") {
                libraryObject_ = boost::shared_ptr<QuantLib::Extrapolator>(new
                    QuantLib::InterpolatedDiscountCurve<QuantLib::Cubic>(
                        dates, data, dayCounter, calendar,
                        QuantLib::Cubic(QuantLib::CubicInterpolation::Parabolic, true)));
            } else if (interpolatorID_=="MONOTONICLOGPARABOLIC") {
                libraryObject_ = boost::shared_ptr<QuantLib::Extrapolator>(new
                    QuantLib::InterpolatedDiscountCurve<QuantLib::LogCubic>(
                        dates, data, dayCounter, calendar,
                        QuantLib::LogCubic(QuantLib::CubicInterpolation::Parabolic, true)));
            } else
                QL_FAIL("unknown interpolatorID: " << interpolatorID_);
        } else if (traitsID_=="ZEROYIELD") {
            if (interpolatorID_=="BACKWARDFLAT") {
                libraryObject_ = boost::shared_ptr<QuantLib::Extrapolator>(new
                    QuantLib::InterpolatedZeroCurve<QuantLib::BackwardFlat>(
                        dates, data, dayCounter, calendar));
            } else if (interpolatorID_=="FORWARDFLAT") {
                libraryObject_ = boost::shared_ptr<QuantLib::Extrapolator>(new
                    QuantLib::InterpolatedZeroCurve<QuantLib::ForwardFlat>(
                        dates, data, dayCounter, calendar));
            } else if (interpolatorID_=="LINEAR") {
                libraryObject_ = boost::shared_ptr<QuantLib::Extrapolator>(new
                    QuantLib::InterpolatedZeroCurve<QuantLib::Linear>(
                        dates, data, dayCounter, calendar));
            } else if (interpolatorID_=="LOGLINEAR") {
                libraryObject_ = boost::shared_ptr<QuantLib::Extrapolator>(new
                    QuantLib::InterpolatedZeroCurve<QuantLib::LogLinear>(
                        dates, data, dayCounter, calendar));
            } else if (interpolatorID_=="CUBICNATURALSPLINE") {
                libraryObject_ = boost::shared_ptr<QuantLib::Extrapolator>(new
                    QuantLib::InterpolatedZeroCurve<QuantLib::Cubic>(
                        dates, data, dayCounter, calendar,
                        QuantLib::Cubic(QuantLib::CubicInterpolation::Spline, false,
                                        QuantLib::CubicInterpolation::SecondDerivative, 0.0,
                                        QuantLib::CubicInterpolation::SecondDerivative, 0.0)));
            } else if (interpolatorID_=="LOGCUBICNATURALSPLINE") {
                libraryObject_ = boost::shared_ptr<QuantLib::Extrapolator>(new
                    QuantLib::InterpolatedZeroCurve<QuantLib::LogCubic>(
                        dates, data, dayCounter, calendar,
                        QuantLib::LogCubic(QuantLib::CubicInterpolation::Spline, false,
                                           QuantLib::CubicInterpolation::SecondDerivative, 0.0,
                                           QuantLib::CubicInterpolation::SecondDerivative, 0.0)));
            } else if (interpolatorID_=="MONOTONICCUBICNATURALSPLINE") {
                libraryObject_ = boost::shared_ptr<QuantLib::Extrapolator>(new
                    QuantLib::InterpolatedZeroCurve<QuantLib::Cubic>(
                        dates, data, dayCounter, calendar,
                        QuantLib::Cubic(QuantLib::CubicInterpolation::Spline, true,
                                        QuantLib::CubicInterpolation::SecondDerivative, 0.0,
                                        QuantLib::CubicInterpolation::SecondDerivative, 0.0)));
            } else if (interpolatorID_=="MONOTONICLOGCUBICNATURALSPLINE") {
                libraryObject_ = boost::shared_ptr<QuantLib::Extrapolator>(new
                    QuantLib::InterpolatedZeroCurve<QuantLib::LogCubic>(
                        dates, data, dayCounter, calendar,
                        QuantLib::LogCubic(QuantLib::CubicInterpolation::Spline, true,
                                           QuantLib::CubicInterpolation::SecondDerivative, 0.0,
                                           QuantLib::CubicInterpolation::SecondDerivative, 0.0)));
            } else if (interpolatorID_=="KRUGERCUBIC") {
                libraryObject_ = boost::shared_ptr<QuantLib::Extrapolator>(new
                    QuantLib::InterpolatedZeroCurve<QuantLib::Cubic>(
                        dates, data, dayCounter, calendar,
                        QuantLib::Cubic(QuantLib::CubicInterpolation::Kruger)));
            } else if (interpolatorID_=="KRUGERLOGCUBIC") {
                libraryObject_ = boost::shared_ptr<QuantLib::Extrapolator>(new
                    QuantLib::InterpolatedZeroCurve<QuantLib::LogCubic>(
                        dates, data, dayCounter, calendar,
                        QuantLib::LogCubic(QuantLib::CubicInterpolation::Kruger)));
            } else if (interpolatorID_=="FRITSCHBUTLANDCUBIC") {
                libraryObject_ = boost::shared_ptr<QuantLib::Extrapolator>(new
                    QuantLib::InterpolatedZeroCurve<QuantLib::Cubic>(
                        dates, data, dayCounter, calendar,
                        QuantLib::Cubic(QuantLib::CubicInterpolation::FritschButland)));
            } else if (interpolatorID_=="FRITSCHBUTLANDLOGCUBIC") {
                libraryObject_ = boost::shared_ptr<QuantLib::Extrapolator>(new
                    QuantLib::InterpolatedZeroCurve<QuantLib::LogCubic>(
                        dates, data, dayCounter, calendar,
                        QuantLib::LogCubic(QuantLib::CubicInterpolation::FritschButland)));
            } else if (interpolatorID_=="PARABOLIC") {
                libraryObject_ = boost::shared_ptr<QuantLib::Extrapolator>(new
                    QuantLib::InterpolatedZeroCurve<QuantLib::Cubic>(
                        dates, data, dayCounter, calendar,
                        QuantLib::Cubic(QuantLib::CubicInterpolation::Parabolic, false)));
            } else if (interpolatorID_=="LOGPARABOLIC") {
                libraryObject_ = boost::shared_ptr<QuantLib::Extrapolator>(new
                    QuantLib::InterpolatedZeroCurve<QuantLib::LogCubic>(
                        dates, data, dayCounter, calendar,
                        QuantLib::LogCubic(QuantLib::CubicInterpolation::Parabolic, false)));
            } else if (interpolatorID_=="MONOTONICPARABOLIC") {
                libraryObject_ = boost::shared_ptr<QuantLib::Extrapolator>(new
                    QuantLib::InterpolatedZeroCurve<QuantLib::Cubic>(
                        dates, data, dayCounter, calendar,
                        QuantLib::Cubic(QuantLib::CubicInterpolation::Parabolic, true)));
            } else if (interpolatorID_=="MONOTONICLOGPARABOLIC") {
                libraryObject_ = boost::shared_ptr<QuantLib::Extrapolator>(new
                    QuantLib::InterpolatedZeroCurve<QuantLib::LogCubic>(
                        dates, data, dayCounter, calendar,
                        QuantLib::LogCubic(QuantLib::CubicInterpolation::Parabolic, true)));
            } else
                QL_FAIL("unknown interpolatorID: " << interpolatorID_);
        } else if (traitsID_=="FORWARDRATE") {
            if (interpolatorID_=="BACKWARDFLAT") {
                libraryObject_ = boost::shared_ptr<QuantLib::Extrapolator>(new
                    QuantLib::InterpolatedForwardCurve<QuantLib::BackwardFlat>(
                        dates, data, dayCounter, calendar));
            } else if (interpolatorID_=="FORWARDFLAT") {
                libraryObject_ = boost::shared_ptr<QuantLib::Extrapolator>(new
                    QuantLib::InterpolatedForwardCurve<QuantLib::ForwardFlat>(
                        dates, data, dayCounter, calendar));
            } else if (interpolatorID_=="LINEAR") {
                libraryObject_ = boost::shared_ptr<QuantLib::Extrapolator>(new
                    QuantLib::InterpolatedForwardCurve<QuantLib::Linear>(
                        dates, data, dayCounter, calendar));
            } else if (interpolatorID_=="LOGLINEAR") {
                libraryObject_ = boost::shared_ptr<QuantLib::Extrapolator>(new
                    QuantLib::InterpolatedForwardCurve<QuantLib::LogLinear>(
                        dates, data, dayCounter, calendar));
            } else if (interpolatorID_=="CUBICNATURALSPLINE") {
                libraryObject_ = boost::shared_ptr<QuantLib::Extrapolator>(new
                    QuantLib::InterpolatedForwardCurve<QuantLib::Cubic>(
                        dates, data, dayCounter, calendar,
                        QuantLib::Cubic(QuantLib::CubicInterpolation::Spline, false,
                                        QuantLib::CubicInterpolation::SecondDerivative, 0.0,
                                        QuantLib::CubicInterpolation::SecondDerivative, 0.0)));
            } else if (interpolatorID_=="LOGCUBICNATURALSPLINE") {
                libraryObject_ = boost::shared_ptr<QuantLib::Extrapolator>(new
                    QuantLib::InterpolatedForwardCurve<QuantLib::LogCubic>(
                        dates, data, dayCounter, calendar,
                        QuantLib::LogCubic(QuantLib::CubicInterpolation::Spline, false,
                                           QuantLib::CubicInterpolation::SecondDerivative, 0.0,
                                           QuantLib::CubicInterpolation::SecondDerivative, 0.0)));
            } else if (interpolatorID_=="MONOTONICCUBICNATURALSPLINE") {
                libraryObject_ = boost::shared_ptr<QuantLib::Extrapolator>(new
                    QuantLib::InterpolatedForwardCurve<QuantLib::Cubic>(
                        dates, data, dayCounter, calendar,
                        QuantLib::Cubic(QuantLib::CubicInterpolation::Spline, true,
                                        QuantLib::CubicInterpolation::SecondDerivative, 0.0,
                                        QuantLib::CubicInterpolation::SecondDerivative, 0.0)));
            } else if (interpolatorID_=="MONOTONICLOGCUBICNATURALSPLINE") {
                libraryObject_ = boost::shared_ptr<QuantLib::Extrapolator>(new
                    QuantLib::InterpolatedForwardCurve<QuantLib::LogCubic>(
                        dates, data, dayCounter, calendar,
                        QuantLib::LogCubic(QuantLib::CubicInterpolation::Spline, true,
                                           QuantLib::CubicInterpolation::SecondDerivative, 0.0,
                                           QuantLib::CubicInterpolation::SecondDerivative, 0.0)));
            } else if (interpolatorID_=="KRUGERCUBIC") {
                libraryObject_ = boost::shared_ptr<QuantLib::Extrapolator>(new
                    QuantLib::InterpolatedForwardCurve<QuantLib::Cubic>(
                        dates, data, dayCounter, calendar,
                        QuantLib::Cubic(QuantLib::CubicInterpolation::Kruger)));
            } else if (interpolatorID_=="KRUGERLOGCUBIC") {
                libraryObject_ = boost::shared_ptr<QuantLib::Extrapolator>(new
                    QuantLib::InterpolatedForwardCurve<QuantLib::LogCubic>(
                        dates, data, dayCounter, calendar,
                        QuantLib::LogCubic(QuantLib::CubicInterpolation::Kruger)));
            } else if (interpolatorID_=="FRITSCHBUTLANDCUBIC") {
                libraryObject_ = boost::shared_ptr<QuantLib::Extrapolator>(new
                    QuantLib::InterpolatedForwardCurve<QuantLib::Cubic>(
                        dates, data, dayCounter, calendar,
                        QuantLib::Cubic(QuantLib::CubicInterpolation::FritschButland)));
            } else if (interpolatorID_=="FRITSCHBUTLANDLOGCUBIC") {
                libraryObject_ = boost::shared_ptr<QuantLib::Extrapolator>(new
                    QuantLib::InterpolatedForwardCurve<QuantLib::LogCubic>(
                        dates, data, dayCounter, calendar,
                        QuantLib::LogCubic(QuantLib::CubicInterpolation::FritschButland)));
            } else if (interpolatorID_=="PARABOLIC") {
                libraryObject_ = boost::shared_ptr<QuantLib::Extrapolator>(new
                    QuantLib::InterpolatedForwardCurve<QuantLib::Cubic>(
                        dates, data, dayCounter, calendar,
                        QuantLib::Cubic(QuantLib::CubicInterpolation::Parabolic)));
            } else if (interpolatorID_=="LOGPARABOLIC") {
                libraryObject_ = boost::shared_ptr<QuantLib::Extrapolator>(new
                    QuantLib::InterpolatedForwardCurve<QuantLib::LogCubic>(
                        dates, data, dayCounter, calendar,
                        QuantLib::LogCubic(QuantLib::CubicInterpolation::Parabolic, false)));
            } else if (interpolatorID_=="MONOTONICPARABOLIC") {
                libraryObject_ = boost::shared_ptr<QuantLib::Extrapolator>(new
                    QuantLib::InterpolatedForwardCurve<QuantLib::Cubic>(
                        dates, data, dayCounter, calendar,
                        QuantLib::Cubic(QuantLib::CubicInterpolation::Parabolic, true)));
            } else if (interpolatorID_=="MONOTONICLOGPARABOLIC") {
                libraryObject_ = boost::shared_ptr<QuantLib::Extrapolator>(new
                    QuantLib::InterpolatedForwardCurve<QuantLib::LogCubic>(
                        dates, data, dayCounter, calendar,
                        QuantLib::LogCubic(QuantLib::CubicInterpolation::Parabolic, true)));
            } else
                QL_FAIL("unknown interpolatorID: " << interpolatorID_);
        } else
            QL_FAIL("unknown traitsID: " << traitsID_);
 
    }

    #define RESOLVE_TEMPLATE(NAME) \
        if (traitsID_=="DISCOUNT") { \
            if (interpolatorID_=="BACKWARDFLAT") { \
                return boost::dynamic_pointer_cast<QuantLib::InterpolatedDiscountCurve<QuantLib::BackwardFlat> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="FORWARDFLAT") { \
                return boost::dynamic_pointer_cast<QuantLib::InterpolatedDiscountCurve<QuantLib::ForwardFlat> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="LINEAR") { \
                return boost::dynamic_pointer_cast<QuantLib::InterpolatedDiscountCurve<QuantLib::Linear> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="LOGLINEAR") { \
                return boost::dynamic_pointer_cast<QuantLib::InterpolatedDiscountCurve<QuantLib::LogLinear> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="CUBICNATURALSPLINE") { \
                return boost::dynamic_pointer_cast<QuantLib::InterpolatedDiscountCurve<QuantLib::Cubic> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="LOGCUBICNATURALSPLINE") { \
                return boost::dynamic_pointer_cast<QuantLib::InterpolatedDiscountCurve<QuantLib::Cubic> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="MONOTONICCUBICNATURALSPLINE") { \
                return boost::dynamic_pointer_cast<QuantLib::InterpolatedDiscountCurve<QuantLib::Cubic> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="MONOTONICLOGCUBICNATURALSPLINE") { \
                return boost::dynamic_pointer_cast<QuantLib::InterpolatedDiscountCurve<QuantLib::Cubic> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="KRUGERCUBIC") { \
                return boost::dynamic_pointer_cast<QuantLib::InterpolatedDiscountCurve<QuantLib::Cubic> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="KRUGERLOGCUBIC") { \
                return boost::dynamic_pointer_cast<QuantLib::InterpolatedDiscountCurve<QuantLib::Cubic> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="FRITSCHBUTLANDCUBIC") { \
                return boost::dynamic_pointer_cast<QuantLib::InterpolatedDiscountCurve<QuantLib::Cubic> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="FRITSCHBUTLANDLOGCUBIC") { \
                return boost::dynamic_pointer_cast<QuantLib::InterpolatedDiscountCurve<QuantLib::Cubic> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="PARABOLIC") { \
                return boost::dynamic_pointer_cast<QuantLib::InterpolatedDiscountCurve<QuantLib::Cubic> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="LOGPARABOLIC") { \
                return boost::dynamic_pointer_cast<QuantLib::InterpolatedDiscountCurve<QuantLib::Cubic> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="MONOTONICPARABOLIC") { \
                return boost::dynamic_pointer_cast<QuantLib::InterpolatedDiscountCurve<QuantLib::Cubic> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="MONOTONICLOGPARABOLIC") { \
                return boost::dynamic_pointer_cast<QuantLib::InterpolatedDiscountCurve<QuantLib::Cubic> >(libraryObject_)->NAME(); \
            } else \
                QL_FAIL("unknown interpolatorID: " << interpolatorID_); \
        } else if (traitsID_=="ZEROYIELD") { \
            if (interpolatorID_=="BACKWARDFLAT") { \
                return boost::dynamic_pointer_cast<QuantLib::InterpolatedZeroCurve<QuantLib::BackwardFlat> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="FORWARDFLAT") { \
                return boost::dynamic_pointer_cast<QuantLib::InterpolatedZeroCurve<QuantLib::ForwardFlat> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="LINEAR") { \
                return boost::dynamic_pointer_cast<QuantLib::InterpolatedZeroCurve<QuantLib::Linear> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="LOGLINEAR") { \
                return boost::dynamic_pointer_cast<QuantLib::InterpolatedZeroCurve<QuantLib::LogLinear> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="CUBICNATURALSPLINE") { \
                return boost::dynamic_pointer_cast<QuantLib::InterpolatedZeroCurve<QuantLib::Cubic> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="LOGCUBICNATURALSPLINE") { \
                return boost::dynamic_pointer_cast<QuantLib::InterpolatedZeroCurve<QuantLib::Cubic> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="MONOTONICCUBICNATURALSPLINE") { \
                return boost::dynamic_pointer_cast<QuantLib::InterpolatedZeroCurve<QuantLib::Cubic> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="MONOTONICLOGCUBICNATURALSPLINE") { \
                return boost::dynamic_pointer_cast<QuantLib::InterpolatedZeroCurve<QuantLib::Cubic> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="KRUGERCUBIC") { \
                return boost::dynamic_pointer_cast<QuantLib::InterpolatedZeroCurve<QuantLib::Cubic> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="KRUGERLOGCUBIC") { \
                return boost::dynamic_pointer_cast<QuantLib::InterpolatedZeroCurve<QuantLib::Cubic> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="FRITSCHBUTLANDCUBIC") { \
                return boost::dynamic_pointer_cast<QuantLib::InterpolatedZeroCurve<QuantLib::Cubic> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="FRITSCHBUTLANDLOGCUBIC") { \
                return boost::dynamic_pointer_cast<QuantLib::InterpolatedZeroCurve<QuantLib::Cubic> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="PARABOLIC") { \
                return boost::dynamic_pointer_cast<QuantLib::InterpolatedZeroCurve<QuantLib::Cubic> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="LOGPARABOLIC") { \
                return boost::dynamic_pointer_cast<QuantLib::InterpolatedZeroCurve<QuantLib::Cubic> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="MONOTONICPARABOLIC") { \
                return boost::dynamic_pointer_cast<QuantLib::InterpolatedZeroCurve<QuantLib::Cubic> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="MONOTONICLOGPARABOLIC") { \
                return boost::dynamic_pointer_cast<QuantLib::InterpolatedZeroCurve<QuantLib::Cubic> >(libraryObject_)->NAME(); \
            } else \
                QL_FAIL("unknown interpolatorID: " << interpolatorID_); \
        } else if (traitsID_=="FORWARDRATE") { \
            if (interpolatorID_=="BACKWARDFLAT") { \
                return boost::dynamic_pointer_cast<QuantLib::InterpolatedForwardCurve<QuantLib::BackwardFlat> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="FORWARDFLAT") { \
                return boost::dynamic_pointer_cast<QuantLib::InterpolatedForwardCurve<QuantLib::ForwardFlat> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="LINEAR") { \
                return boost::dynamic_pointer_cast<QuantLib::InterpolatedForwardCurve<QuantLib::Linear> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="LOGLINEAR") { \
                return boost::dynamic_pointer_cast<QuantLib::InterpolatedForwardCurve<QuantLib::LogLinear> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="CUBICNATURALSPLINE") { \
                return boost::dynamic_pointer_cast<QuantLib::InterpolatedForwardCurve<QuantLib::Cubic> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="LOGCUBICNATURALSPLINE") { \
                return boost::dynamic_pointer_cast<QuantLib::InterpolatedForwardCurve<QuantLib::Cubic> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="MONOTONICCUBICNATURALSPLINE") { \
                return boost::dynamic_pointer_cast<QuantLib::InterpolatedForwardCurve<QuantLib::Cubic> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="MONOTONICLOGCUBICNATURALSPLINE") { \
                return boost::dynamic_pointer_cast<QuantLib::InterpolatedForwardCurve<QuantLib::Cubic> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="KRUGERCUBIC") { \
                return boost::dynamic_pointer_cast<QuantLib::InterpolatedForwardCurve<QuantLib::Cubic> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="KRUGERLOGCUBIC") { \
                return boost::dynamic_pointer_cast<QuantLib::InterpolatedForwardCurve<QuantLib::Cubic> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="FRITSCHBUTLANDCUBIC") { \
                return boost::dynamic_pointer_cast<QuantLib::InterpolatedForwardCurve<QuantLib::Cubic> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="FRITSCHBUTLANDLOGCUBIC") { \
                return boost::dynamic_pointer_cast<QuantLib::InterpolatedForwardCurve<QuantLib::Cubic> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="PARABOLIC") { \
                return boost::dynamic_pointer_cast<QuantLib::InterpolatedForwardCurve<QuantLib::Cubic> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="LOGPARABOLIC") { \
                return boost::dynamic_pointer_cast<QuantLib::InterpolatedForwardCurve<QuantLib::Cubic> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="MONOTONICPARABOLIC") { \
                return boost::dynamic_pointer_cast<QuantLib::InterpolatedForwardCurve<QuantLib::Cubic> >(libraryObject_)->NAME(); \
            } else if (interpolatorID_=="MONOTONICLOGPARABOLIC") { \
                return boost::dynamic_pointer_cast<QuantLib::InterpolatedForwardCurve<QuantLib::Cubic> >(libraryObject_)->NAME(); \
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

    //const std::vector<QuantLib::Time>& InterpolatedYieldCurve::jumpTimes() const {
    //    RESOLVE_TEMPLATE(jumpTimes)
    //}

    //const std::vector<QuantLib::Date>& InterpolatedYieldCurve::jumpDates() const {
    //    RESOLVE_TEMPLATE(jumpDates)
    //}

}

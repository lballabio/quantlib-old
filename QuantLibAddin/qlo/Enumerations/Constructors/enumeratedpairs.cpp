
/*
 Copyright (C) 2007 Ferdinando Ametrano
 Copyright (C) 2006 Marco Bianchetti
 Copyright (C) 2006, 2007 Eric Ehlers
 Copyright (C) 2006 Giorgio Facchinetti
 Copyright (C) 2006 Chiara Fornarola
 Copyright (C) 2007 Katiuscia Manzoni
 Copyright (C) 2005 Plamen Neykov

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

#include <qlo/Enumerations/Constructors/enumeratedpairs.hpp>
#include <qlo/Conversions/conversions.hpp>
#include <qlo/termstructures.hpp>
#include <ql/yieldtermstructures/piecewiseyieldcurve.hpp>
#include <ql/math/interpolations/forwardflatinterpolation.hpp>
#include <ql/math/interpolations/backwardflatinterpolation.hpp>

namespace QuantLibAddin {

    /* *** YieldTermStructures *** */

    /* *** Discount based *** */
    boost::shared_ptr<QuantLib::YieldTermStructure> DISCOUNT_BACKWARDFLAT_PiecewiseYieldCurve(
            QuantLib::Natural nDays,
            const QuantLib::Calendar& calendar,
            const std::vector<boost::shared_ptr<QuantLib::RateHelper> >& rateHelpers,
            const QuantLib::DayCounter& dayCounter,
            QuantLib::Real accuracy) {
        return boost::shared_ptr<QuantLib::YieldTermStructure>(new
            QuantLib::PiecewiseYieldCurve<QuantLib::Discount,
                                          QuantLib::BackwardFlat>(nDays,
                                                                  calendar,
                                                                  rateHelpers,
                                                                  dayCounter,
                                                                  accuracy));
    }
    boost::shared_ptr<QuantLib::YieldTermStructure> DISCOUNT_FORWARDFLAT_PiecewiseYieldCurve(
            QuantLib::Natural nDays,
            const QuantLib::Calendar& calendar,
            const std::vector<boost::shared_ptr<QuantLib::RateHelper> >& rateHelpers,
            const QuantLib::DayCounter& dayCounter,
            QuantLib::Real accuracy) {
        return boost::shared_ptr<QuantLib::YieldTermStructure>(new
            QuantLib::PiecewiseYieldCurve<QuantLib::Discount,
                                          QuantLib::ForwardFlat>(nDays,
                                                                 calendar,
                                                                 rateHelpers,
                                                                 dayCounter,
                                                                 accuracy));
    }
    boost::shared_ptr<QuantLib::YieldTermStructure> DISCOUNT_LINEAR_PiecewiseYieldCurve(
            QuantLib::Natural nDays,
            const QuantLib::Calendar& calendar,
            const std::vector<boost::shared_ptr<QuantLib::RateHelper> >& rateHelpers,
            const QuantLib::DayCounter& dayCounter,
            QuantLib::Real accuracy) {
        return boost::shared_ptr<QuantLib::YieldTermStructure>(new
            QuantLib::PiecewiseYieldCurve<QuantLib::Discount,
                                          QuantLib::Linear>(nDays,
                                                            calendar,
                                                            rateHelpers,
                                                            dayCounter,
                                                            accuracy));
    }
    boost::shared_ptr<QuantLib::YieldTermStructure> DISCOUNT_LOGLINEAR_PiecewiseYieldCurve(
            QuantLib::Natural nDays,
            const QuantLib::Calendar& calendar,
            const std::vector<boost::shared_ptr<QuantLib::RateHelper> >& rateHelpers,
            const QuantLib::DayCounter& dayCounter,
            QuantLib::Real accuracy) {
        return boost::shared_ptr<QuantLib::YieldTermStructure>(new
            QuantLib::PiecewiseYieldCurve<QuantLib::Discount,
                                          QuantLib::LogLinear>(nDays,
                                                               calendar,
                                                               rateHelpers,
                                                               dayCounter,
                                                               accuracy));
    }
    boost::shared_ptr<QuantLib::YieldTermStructure> DISCOUNT_CUBIC_PiecewiseYieldCurve(
            QuantLib::Natural nDays,
            const QuantLib::Calendar& calendar,
            const std::vector<boost::shared_ptr<QuantLib::RateHelper> >& rateHelpers,
            const QuantLib::DayCounter& dayCounter,
            QuantLib::Real accuracy) {
        return boost::shared_ptr<QuantLib::YieldTermStructure>(new
            QuantLib::PiecewiseYieldCurve<QuantLib::Discount,
                                          QuantLib::Cubic>(nDays,
                                                           calendar,
                                                           rateHelpers,
                                                           dayCounter,
                                                           accuracy));
    }

    /* *** ZeroYield based *** */
    boost::shared_ptr<QuantLib::YieldTermStructure> ZEROYIELD_BACKWARDFLAT_PiecewiseYieldCurve(
            QuantLib::Natural nDays,
            const QuantLib::Calendar& calendar,
            const std::vector<boost::shared_ptr<QuantLib::RateHelper> >& rateHelpers,
            const QuantLib::DayCounter& dayCounter,
            QuantLib::Real accuracy) {
        return boost::shared_ptr<QuantLib::YieldTermStructure>(new
            QuantLib::PiecewiseYieldCurve<QuantLib::ZeroYield,
                                          QuantLib::BackwardFlat>(nDays,
                                                                  calendar,
                                                                  rateHelpers,
                                                                  dayCounter,
                                                                  accuracy));
    }
    boost::shared_ptr<QuantLib::YieldTermStructure> ZEROYIELD_FORWARDFLAT_PiecewiseYieldCurve(
            QuantLib::Natural nDays,
            const QuantLib::Calendar& calendar,
            const std::vector<boost::shared_ptr<QuantLib::RateHelper> >& rateHelpers,
            const QuantLib::DayCounter& dayCounter,
            QuantLib::Real accuracy) {
        return boost::shared_ptr<QuantLib::YieldTermStructure>(new
            QuantLib::PiecewiseYieldCurve<QuantLib::ZeroYield,
                                          QuantLib::ForwardFlat>(nDays,
                                                                 calendar,
                                                                 rateHelpers,
                                                                 dayCounter,
                                                                 accuracy));
    }
    boost::shared_ptr<QuantLib::YieldTermStructure> ZEROYIELD_LINEAR_PiecewiseYieldCurve(
            QuantLib::Natural nDays,
            const QuantLib::Calendar& calendar,
            const std::vector<boost::shared_ptr<QuantLib::RateHelper> >& rateHelpers,
            const QuantLib::DayCounter& dayCounter,
            QuantLib::Real accuracy) {
        return boost::shared_ptr<QuantLib::YieldTermStructure>(new
            QuantLib::PiecewiseYieldCurve<QuantLib::ZeroYield,
                                          QuantLib::Linear>(nDays,
                                                            calendar,
                                                            rateHelpers,
                                                            dayCounter,
                                                            accuracy));
    }
    boost::shared_ptr<QuantLib::YieldTermStructure> ZEROYIELD_LOGLINEAR_PiecewiseYieldCurve(
            QuantLib::Natural nDays,
            const QuantLib::Calendar& calendar,
            const std::vector<boost::shared_ptr<QuantLib::RateHelper> >& rateHelpers,
            const QuantLib::DayCounter& dayCounter,
            QuantLib::Real accuracy) {
        return boost::shared_ptr<QuantLib::YieldTermStructure>(new
            QuantLib::PiecewiseYieldCurve<QuantLib::ZeroYield,
                                          QuantLib::LogLinear>(nDays, 
                                                               calendar,
                                                               rateHelpers,
                                                               dayCounter,
                                                               accuracy));
    }
    boost::shared_ptr<QuantLib::YieldTermStructure> ZEROYIELD_CUBIC_PiecewiseYieldCurve(
            QuantLib::Natural nDays,
            const QuantLib::Calendar& calendar,
            const std::vector<boost::shared_ptr<QuantLib::RateHelper> >& rateHelpers,
            const QuantLib::DayCounter& dayCounter,
            QuantLib::Real accuracy) {
        return boost::shared_ptr<QuantLib::YieldTermStructure>(new
            QuantLib::PiecewiseYieldCurve<QuantLib::ZeroYield,
                                          QuantLib::Cubic>(nDays,
                                                           calendar,
                                                           rateHelpers,
                                                           dayCounter,
                                                           accuracy));
    }

    /* *** ForwardRate based *** */
    boost::shared_ptr<QuantLib::YieldTermStructure> FORWARDRATE_BACKWARDFLAT_PiecewiseYieldCurve(
            QuantLib::Natural nDays,
            const QuantLib::Calendar& calendar,
            const std::vector<boost::shared_ptr<QuantLib::RateHelper> >& rateHelpers,
            const QuantLib::DayCounter& dayCounter,
            QuantLib::Real accuracy) {
        return boost::shared_ptr<QuantLib::YieldTermStructure>(new
            QuantLib::PiecewiseYieldCurve<QuantLib::ForwardRate,
                                          QuantLib::BackwardFlat>(nDays,
                                                                  calendar,
                                                                  rateHelpers,
                                                                  dayCounter,
                                                                  accuracy));
    }
    boost::shared_ptr<QuantLib::YieldTermStructure> FORWARDRATE_FORWARDFLAT_PiecewiseYieldCurve(
            QuantLib::Natural nDays,
            const QuantLib::Calendar& calendar,
            const std::vector<boost::shared_ptr<QuantLib::RateHelper> >& rateHelpers,
            const QuantLib::DayCounter& dayCounter,
            QuantLib::Real accuracy) {
        return boost::shared_ptr<QuantLib::YieldTermStructure>(new
            QuantLib::PiecewiseYieldCurve<QuantLib::ForwardRate,
                                          QuantLib::ForwardFlat>(nDays,
                                                                 calendar,
                                                                 rateHelpers,
                                                                 dayCounter,
                                                                 accuracy));
    }
    boost::shared_ptr<QuantLib::YieldTermStructure> FORWARDRATE_LINEAR_PiecewiseYieldCurve(
            QuantLib::Natural nDays,
            const QuantLib::Calendar& calendar,
            const std::vector<boost::shared_ptr<QuantLib::RateHelper> >& rateHelpers,
            const QuantLib::DayCounter& dayCounter,
            QuantLib::Real accuracy) {
        return boost::shared_ptr<QuantLib::YieldTermStructure>(new
            QuantLib::PiecewiseYieldCurve<QuantLib::ForwardRate,
                                          QuantLib::Linear>(nDays,
                                                            calendar,
                                                            rateHelpers,
                                                            dayCounter,
                                                            accuracy));
    }
    boost::shared_ptr<QuantLib::YieldTermStructure> FORWARDRATE_LOGLINEAR_PiecewiseYieldCurve(
            QuantLib::Natural nDays,
            const QuantLib::Calendar& calendar,
            const std::vector<boost::shared_ptr<QuantLib::RateHelper> >& rateHelpers,
            const QuantLib::DayCounter& dayCounter,
            QuantLib::Real accuracy) {
        return boost::shared_ptr<QuantLib::YieldTermStructure>(new
            QuantLib::PiecewiseYieldCurve<QuantLib::ForwardRate,
                                          QuantLib::LogLinear>(nDays,
                                                               calendar,
                                                               rateHelpers,
                                                               dayCounter,
                                                               accuracy));
    }
    boost::shared_ptr<QuantLib::YieldTermStructure> FORWARDRATE_CUBIC_PiecewiseYieldCurve(
            QuantLib::Natural nDays,
            const QuantLib::Calendar& calendar,
            const std::vector<boost::shared_ptr<QuantLib::RateHelper> >& rateHelpers,
            const QuantLib::DayCounter& dayCounter,
            QuantLib::Real accuracy) {
        return boost::shared_ptr<QuantLib::YieldTermStructure>(new
            QuantLib::PiecewiseYieldCurve<QuantLib::ForwardRate,
                                          QuantLib::Cubic>(nDays,
                                                           calendar,
                                                           rateHelpers,
                                                           dayCounter,
                                                           accuracy));
    }

    boost::shared_ptr<QuantLib::HistoricalForwardRatesAnalysis> DISCOUNT_CUBIC_HistoricalForwardRatesAnalysis(
        const boost::shared_ptr<QuantLib::SequenceStatistics>& stats,
        const QuantLib::Date& startDate,
        const QuantLib::Date& endDate,
        const QuantLib::Period& step,
        const boost::shared_ptr<QuantLib::InterestRateIndex>& fwdIndex,
        const QuantLib::Period& initialGap,
        const QuantLib::Period& horizon,
        const std::vector<boost::shared_ptr<QuantLib::IborIndex> >& iborIndexes,
        const std::vector<boost::shared_ptr<QuantLib::SwapIndex> >& swapIndexes,
        const QuantLib::DayCounter& yieldCurveDayCounter,
        QuantLib::Real yieldCurveAccuracy){
            return boost::shared_ptr<QuantLib::HistoricalForwardRatesAnalysis>(new
                QuantLib::HistoricalForwardRatesAnalysisImpl<QuantLib::Discount,
                QuantLib::Cubic>(
                stats,
                startDate,
                endDate,
                step,
                fwdIndex,
                initialGap,
                horizon,
                iborIndexes,
                swapIndexes,
                yieldCurveDayCounter,
                yieldCurveAccuracy)); 
    }

    boost::shared_ptr<QuantLib::HistoricalForwardRatesAnalysis> ZEROYIELD_LINEAR_HistoricalForwardRatesAnalysis(
        const boost::shared_ptr<QuantLib::SequenceStatistics>& stats,
        const QuantLib::Date& startDate,
        const QuantLib::Date& endDate,
        const QuantLib::Period& step,
        const boost::shared_ptr<QuantLib::InterestRateIndex>& fwdIndex,
        const QuantLib::Period& initialGap,
        const QuantLib::Period& horizon,
        const std::vector<boost::shared_ptr<QuantLib::IborIndex> >& iborIndexes,
        const std::vector<boost::shared_ptr<QuantLib::SwapIndex> >& swapIndexes,
        const QuantLib::DayCounter& yieldCurveDayCounter,
        QuantLib::Real yieldCurveAccuracy){
            return boost::shared_ptr<QuantLib::HistoricalForwardRatesAnalysis>(new
                QuantLib::HistoricalForwardRatesAnalysisImpl<QuantLib::ZeroYield,
                QuantLib::Linear>(
                stats,
                startDate,
                endDate,
                step,
                fwdIndex,
                initialGap,
                horizon,
                iborIndexes,
                swapIndexes,
                yieldCurveDayCounter,
                yieldCurveAccuracy));

    }
}

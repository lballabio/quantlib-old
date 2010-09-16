/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2010 Roland Lichters

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

#include <qlo/qladdindefines.hpp>
#include <qlo/credit.hpp>
#include <qlo/enumerations/factories/termstructuresfactory.hpp>

#include <ql/instruments/stock.hpp>
#include <ql/quote.hpp>
#include <ql/time/daycounter.hpp>
#include <ql/termstructures/credit/interpolatedhazardratecurve.hpp>
#include <ql/termstructures/yield/piecewiseyieldcurve.hpp>
#include <ql/termstructures/credit/piecewisedefaultcurve.hpp>
#include <ql/math/interpolations/backwardflatinterpolation.hpp>
#include <ql/math/interpolations/loginterpolation.hpp>
#include <ql/pricingengines/credit/midpointcdsengine.hpp>

#include <boost/algorithm/string/case_conv.hpp>

#include <ql/settings.hpp>

using boost::algorithm::to_upper_copy;

namespace QuantLibAddin {

    CreditDefaultSwap::CreditDefaultSwap(
              const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
              QuantLib::Protection::Side side,
              QuantLib::Real notional,
              QuantLib::Rate upfront,
              QuantLib::Rate spread,
              const boost::shared_ptr<QuantLib::Schedule>& schedule,
              QuantLib::BusinessDayConvention paymentConvention,
              const QuantLib::DayCounter& dayCounter,
              bool settlesAccrual,
              bool paysAtDefaultTime,
              const QuantLib::Date& protectionStart,
              const QuantLib::Date& upfrontDate,
              bool permanent)
        : Instrument(properties, permanent) {
        libraryObject_ = boost::shared_ptr<QuantLib::CreditDefaultSwap>(
                    new QuantLib::CreditDefaultSwap(side,
                                                    notional,
                                                    upfront,
                                                    spread,
                                                    *schedule,
                                                    paymentConvention,
                                                    dayCounter,
                                                    settlesAccrual,
                                                    paysAtDefaultTime,
                                                    protectionStart,
                                                    upfrontDate));
    }
    
    MidPointCdsEngine::MidPointCdsEngine(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const QuantLib::Handle<QuantLib::DefaultProbabilityTermStructure>& defaultTS,
            QuantLib::Real recoveryRate,
            const QuantLib::Handle<QuantLib::YieldTermStructure>& yieldTS,
            bool permanent) 
        : PricingEngine(properties, permanent) {
        libraryObject_ = boost::shared_ptr<QuantLib::PricingEngine>(new
              QuantLib::MidPointCdsEngine(defaultTS, recoveryRate, yieldTS));
    }



    SpreadCdsHelper::SpreadCdsHelper(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const QuantLib::Handle<QuantLib::Quote>& quote,
            const QuantLib::Period& period,
            QuantLib::Natural settlementDays,
            const QuantLib::Calendar& calendar,
            QuantLib::Frequency frequency,
            QuantLib::BusinessDayConvention paymentConvention,
            QuantLib::DateGeneration::Rule rule,
            const QuantLib::DayCounter& dayCounter,
            QuantLib::Real recoveryRate,
            const QuantLib::Handle<QuantLib::YieldTermStructure>& yieldTS,
            bool settlesAccrual,
            bool paysAtDefaultTime,
            bool permanent) : DefaultProbabilityHelper(properties, permanent) {

        libraryObject_ = boost::shared_ptr<QuantLib::DefaultProbabilityHelper>(new
		       QuantLib::SpreadCdsHelper(quote,
						 period,
						 settlementDays,
						 calendar,
						 frequency,
						 paymentConvention,
						 rule,
						 dayCounter,
						 recoveryRate,
						 yieldTS,
						 settlesAccrual,
						 paysAtDefaultTime));
    }

    UpfrontCdsHelper::UpfrontCdsHelper(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const QuantLib::Handle<QuantLib::Quote>& quote,
            QuantLib::Rate runningSpread,
            const QuantLib::Period& period,
            QuantLib::Natural settlementDays,
            const QuantLib::Calendar& calendar,
            QuantLib::Frequency frequency,
            QuantLib::BusinessDayConvention paymentConvention,
            QuantLib::DateGeneration::Rule rule,
            const QuantLib::DayCounter& dayCounter,
            QuantLib::Real recoveryRate,
            const QuantLib::Handle<QuantLib::YieldTermStructure>& yieldTS,
            QuantLib::Natural upfrontSettlementDays,
            bool settlesAccrual,
            bool paysAtDefaultTime,
            bool permanent) : DefaultProbabilityHelper(properties, permanent) {
        libraryObject_ = boost::shared_ptr<QuantLib::DefaultProbabilityHelper>(new
		       QuantLib::UpfrontCdsHelper(quote,
                                          runningSpread,
                                          period,
                                          settlementDays,
                                          calendar,
                                          frequency,
                                          paymentConvention,
                                          rule,
                                          dayCounter,
                                          recoveryRate,
                                          yieldTS,
                                          upfrontSettlementDays,
                                          settlesAccrual,
                                          paysAtDefaultTime));
    }

    HazardRateCurve::HazardRateCurve(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const std::vector<QuantLib::Date>& dates,
            const std::vector<QuantLib::Rate>& hazardRates,
            const QuantLib::DayCounter& dayCounter,
            bool permanent) 
        : DefaultProbabilityTermStructure(properties, permanent) {
        QL_REQUIRE(!dates.empty(), "no input dates given");
        QL_REQUIRE(dates.size() == hazardRates.size(), 
                   "vector sizes differ");
        libraryObject_ = boost::shared_ptr<QuantLib::Extrapolator>(
        new QuantLib::InterpolatedHazardRateCurve<QuantLib::BackwardFlat>(
 				 dates, hazardRates, dayCounter));
    }

    PiecewiseFlatHazardRateCurve::PiecewiseFlatHazardRateCurve(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const QuantLib::Date& referenceDate,
            const std::vector<boost::shared_ptr<QuantLib::DefaultProbabilityHelper> >& helpers,
            const QuantLib::DayCounter& dayCounter,
            QuantLib::Real accuracy,
            bool permanent) 
        : DefaultProbabilityTermStructure(properties, permanent) {
        libraryObject_ = boost::shared_ptr<QuantLib::Extrapolator>(new
               QuantLib::PiecewiseDefaultCurve<QuantLib::HazardRate,QuantLib::BackwardFlat>(referenceDate, helpers, dayCounter));
    }

    PiecewiseFlatForwardCurve::PiecewiseFlatForwardCurve(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const QuantLib::Date& referenceDate,
            const std::vector<boost::shared_ptr<QuantLib::RateHelper> >& helpers,
            const QuantLib::DayCounter& dayCounter,
            QuantLib::Real accuracy,
            bool permanent)
        : YieldTermStructure(properties, permanent) {
        libraryObject_ = boost::shared_ptr<QuantLib::Extrapolator>(new
               QuantLib::PiecewiseYieldCurve<QuantLib::Discount,QuantLib::LogLinear>(referenceDate, helpers, dayCounter));
    }

}

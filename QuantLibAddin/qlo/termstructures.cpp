
/*
 Copyright (C) 2005, 2006 Eric Ehlers
 Copyright (C) 2006, 2007 Ferdinando Ametrano
 Copyright (C) 2005 Plamen Neykov
 Copyright (C) 2005 Aurelien Chanudet

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

#include <qlo/termstructures.hpp>
#include <qlo/ratehelpers.hpp>
#include <qlo/Enumerations/Factories/termstructuresfactory.hpp>

#include <ql/time/date.hpp>
#include <ql/math/interpolations/cubicspline.hpp>
#include <ql/termstructures/yieldcurves/discountcurve.hpp>
#include <ql/termstructures/yieldcurves/forwardcurve.hpp>
#include <ql/termstructures/yieldcurves/zerocurve.hpp>
#include <ql/termstructures/yieldcurves/impliedtermstructure.hpp>
#include <ql/termstructures/yieldcurves/flatforward.hpp>
#include <ql/termstructures/yieldcurves/forwardspreadedtermstructure.hpp>
#include <ql/math/interpolations/backwardflatinterpolation.hpp>

#include <oh/repository.hpp>

namespace QuantLibAddin {

    PiecewiseYieldCurve::PiecewiseYieldCurve(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            QuantLib::Natural nDays,
            const QuantLib::Calendar& calendar,
            const std::vector<std::string>& handlesRateHelper,
            const QuantLib::DayCounter& dayCounter,
            const std::string& traitsID, 
            const std::string& interpolatorID,
            QuantLib::Real accuracy,
            bool permanent) : YieldTermStructure(properties, permanent)
    {
        std::vector<boost::shared_ptr<QuantLib::RateHelper> > rateHelpersQL;
        rateHelpersQL.reserve(handlesRateHelper.size());
        std::vector<std::string>::const_iterator i;
        for (i=handlesRateHelper.begin() ; i!=handlesRateHelper.end() ; ++i) {
            OH_GET_REFERENCE(rateHelper, *i, RateHelper, QuantLib::RateHelper);
            rateHelpersQL.push_back(rateHelper);
        }

        //QuantLib::Cubic naturalCubic(
        //    QuantLib::CubicSpline::SecondDerivative, 0.0,
        //    QuantLib::CubicSpline::SecondDerivative, 0.0,
        //    false);

        //QuantLib::Cubic cubic1(
        //    QuantLib::CubicSpline::SecondDerivative, 0.0,
        //    QuantLib::CubicSpline::FirstDerivative, 0.0,
        //    false);

        //QuantLib::Cubic monotoneCubic(
        //    QuantLib::CubicSpline::SecondDerivative, 0.0,
        //    QuantLib::CubicSpline::FirstDerivative, 0.0,
        //    true);

        libraryObject_ = ObjectHandler::Create<boost::shared_ptr<
            QuantLib::YieldTermStructure> >()(traitsID,
                                              interpolatorID,
                                              nDays,
                                              calendar,
                                              rateHelpersQL,
                                              dayCounter,
                                              accuracy);

        //libraryObject_ = boost::shared_ptr<QuantLib::YieldTermStructure>(new
        //    QuantLib::PiecewiseYieldCurve<QuantLib::ForwardRate,
        //                                  QuantLib::Cubic>(nDays,
        //                                                   calendar,
        //                                                   rateHelpersQL,
        //                                                   dayCounter,
        //                                                   accuracy,
        //                                                   monotoneCubic));

    }

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
                             QuantLib::Size nDays, 
                             const QuantLib::Calendar& calendar,
                             QuantLib::Rate forward,
                             const QuantLib::DayCounter& dayCounter,
                             QuantLib::Compounding compounding,
                             QuantLib::Frequency frequency,
                             bool permanent) : YieldTermStructure(properties, permanent)
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
            bool permanent) : YieldTermStructure(properties, permanent)
    {
        libraryObject_ = boost::shared_ptr<QuantLib::Extrapolator>(new
            QuantLib::ImpliedTermStructure(hYTS, referenceDate));
    }

}


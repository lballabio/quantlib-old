/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2007 Cristina Duminuco
 Copyright (C) 2006, 2007 Ferdinando Ametrano
 Copyright (C) 2006 Eric Ehlers
 Copyright (C) 2006 Giorgio Facchinetti
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

#if defined(HAVE_CONFIG_H)     // Dynamically created by configure
    #include <qlo/config.hpp>
#endif

#include <qlo/qladdindefines.hpp>
#include <qlo/enumerations/factories/conundrumpricerfactory.hpp>
#include <qlo/conundrumpricer.hpp>

using boost::shared_ptr;
using ObjectHandler::Create;

namespace QuantLibAddin {

   CmsCouponPricer::CmsCouponPricer(
            const shared_ptr<ObjectHandler::ValueObject>& properties,
            const QuantLib::Handle<QuantLib::SwaptionVolatilityStructure>& v,
            const std::string& cmsPricerType,
            QuantLib::GFunctionFactory::YieldCurveModel modelOfYieldCurve,
            const QuantLib::Handle<QuantLib::Quote>& meanReversion,
            bool permanent)
    : FloatingRateCouponPricer(properties, permanent) {
        libraryObject_ =
            Create<shared_ptr<QuantLib::CmsCouponPricer> >()(cmsPricerType,
                                                             v,
                                                             modelOfYieldCurve,
                                                             meanReversion);
    }

    NumericHaganPricer::NumericHaganPricer(
            const shared_ptr<ObjectHandler::ValueObject>& properties,
            const QuantLib::Handle<QuantLib::SwaptionVolatilityStructure>& swaptionVol,
            QuantLib::GFunctionFactory::YieldCurveModel modelOfYieldCurve,
            const QuantLib::Handle<QuantLib::Quote>& meanReversion,
            QuantLib::Rate lowerLimit,
            QuantLib::Rate upperLimit,
            QuantLib::Real precision,
            bool permanent)
    : CmsCouponPricer(properties, permanent) {
        //QuantLib::GFunctionFactory::YieldCurveModel modelOfYieldCurve =
        //    QuantLib::GFunctionFactory::YieldCurveModel(2);
        libraryObject_ = 
            shared_ptr<QuantLib::NumericHaganPricer>(new
                QuantLib::NumericHaganPricer(swaptionVol,
                                                                modelOfYieldCurve,
                                                                meanReversion,
                                                                lowerLimit,
                                                                upperLimit,
                                                                precision));
    };
}

/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2005 Plamen Neykov
 Copyright (C) 2006, 2007 Eric Ehlers

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

#ifndef qla_conundrumpricerfactory_hpp
#define qla_conundrumpricerfactory_hpp

#include <oh/Enumerations/typefactory.hpp>
#include <ql/cashflows/conundrumpricer.hpp>

namespace ObjectHandler {

    typedef boost::shared_ptr<QuantLib::CmsCouponPricer>(*CmsCouponPricerConstructor)(
            const QuantLib::Handle<QuantLib::SwaptionVolatilityStructure>& swaptionVol,
            const QuantLib::GFunctionFactory::ModelOfYieldCurve modelOfYieldCurve,
            const QuantLib::Handle<QuantLib::Quote>& meanReversion);

    template<>
    class Create<boost::shared_ptr<QuantLib::CmsCouponPricer> > :
        private RegistryManager<QuantLib::CmsCouponPricer, EnumClassRegistry> {
    public:
        boost::shared_ptr<QuantLib::CmsCouponPricer> operator() (
                const std::string& CmsCouponPricerID,
                const QuantLib::Handle<QuantLib::SwaptionVolatilityStructure>& swaptionVol,
                const QuantLib::GFunctionFactory::ModelOfYieldCurve modelOfYieldCurve,
                const QuantLib::Handle<QuantLib::Quote>& meanReversion) {
            CmsCouponPricerConstructor cmsCouponPricerConstructor =
                reinterpret_cast<CmsCouponPricerConstructor>(getType(CmsCouponPricerID));
            return cmsCouponPricerConstructor(swaptionVol,modelOfYieldCurve, meanReversion);
        }
        using RegistryManager<QuantLib::CmsCouponPricer, EnumClassRegistry>::registerType;
    };
 }

#endif


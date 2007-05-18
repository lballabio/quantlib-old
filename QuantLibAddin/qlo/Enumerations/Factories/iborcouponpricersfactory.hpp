
/*
 Copyright (C) 2005 Plamen Neykov
 Copyright (C) 2006, 2007 Eric Ehlers

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it under the
 terms of the QuantLib license.  You should have received a copy of the
 license along with this program; if not, please email quantlib-dev@lists.sf.net
 The license is also available online at http://quantlib.org/html/license.html

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
*/

#ifndef qla_iborcouponpricerfactory_hpp
#define qla_iborcouponpricerfactory_hpp

#include <oh/Enumerations/typefactory.hpp>
#include <ql/cashflows/couponpricer.hpp> 

namespace ObjectHandler {

    typedef boost::shared_ptr<QuantLib::IborCouponPricer>(*IborCouponPricerConstructor)( 
            const QuantLib::Handle<QuantLib::CapletVolatilityStructure>& capletVol);

    template<>
    class Create<boost::shared_ptr<QuantLib::IborCouponPricer> > :
        private RegistryManager<QuantLib::IborCouponPricer, EnumClassRegistry> {
    public:
        boost::shared_ptr<QuantLib::IborCouponPricer> operator() (
                const std::string& IborCouponPricerID,
                const QuantLib::Handle<QuantLib::CapletVolatilityStructure>& capletVol) {
            IborCouponPricerConstructor iborCouponPricerConstructor =
                reinterpret_cast<IborCouponPricerConstructor>(getType(IborCouponPricerID));
            return iborCouponPricerConstructor(capletVol);
        }
        using RegistryManager<QuantLib::IborCouponPricer, EnumClassRegistry>::registerType;
    };

 }

#endif


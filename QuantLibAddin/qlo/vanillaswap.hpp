/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2005, 2006 Eric Ehlers
 Copyright (C) 2005 Plamen Neykov
 Copyright (C) 2005 Aurelien Chanudet
 Copyright (C) 2011, 2015 Ferdinando Ametrano

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

#ifndef qla_simpleswap_hpp
#define qla_simpleswap_hpp

#include <qlo/swap.hpp>

#include <ql/instruments/vanillaswap.hpp>

namespace QuantLib {
    class SwapRateHelper;
}

namespace QuantLibAddin {

    class VanillaSwap : public Swap {
    public:
        VanillaSwap(
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            QuantLib::VanillaSwap::Type type,
            QuantLib::Real nominal,
            const boost::shared_ptr<QuantLib::Schedule>& fixedSchedule,
            QuantLib::Rate fixedRate,
            const QuantLib::DayCounter& fixLegDayCounter,
            const boost::shared_ptr<QuantLib::Schedule>& floatSchedule,
            const boost::shared_ptr<QuantLib::IborIndex>& iborIndex,
            QuantLib::Spread floatingLegSpread,
            const QuantLib::DayCounter& floatDayCounter,
            QuantLib::BusinessDayConvention paymentConvention,
            bool permanent);
        VanillaSwap( // MakeVanillaSwap
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            QuantLib::Natural settlDays,
            const QuantLib::Period& swapTenor, 
            const boost::shared_ptr<QuantLib::IborIndex>& iborIndex,
            QuantLib::Rate fixedRate,
            const QuantLib::Period& forwardStart,
            const QuantLib::DayCounter& fixLegDayCounter,
            QuantLib::Spread floatingLegSpread,
            const boost::shared_ptr<QuantLib::PricingEngine>& engine,
            bool permanent);
        VanillaSwap( // MakeIMMSwap
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const QuantLib::Period& swapTenor, 
            const boost::shared_ptr<QuantLib::IborIndex>& iborIndex,
            QuantLib::Rate fixedRate,
            const QuantLib::Date& immDate,
            const QuantLib::DayCounter& fixLegDayCounter,
            QuantLib::Spread floatingLegSpread,
            const boost::shared_ptr<QuantLib::PricingEngine>& engine,
            bool permanent);
        VanillaSwap( // SwapIndex->underlyingSwap()
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const boost::shared_ptr<QuantLib::SwapIndex>& swapIndex,
            const QuantLib::Date& fixingDate,
            bool permanent);
        VanillaSwap( // SwapRateHelper->swap()
            const boost::shared_ptr<ObjectHandler::ValueObject>& properties,
            const boost::shared_ptr<QuantLib::SwapRateHelper>& swapRH,
            bool permanent);
        std::vector<std::vector<ObjectHandler::property_t> >
                                    fixedLegAnalysis(const QuantLib::Date& d);
        std::vector<std::vector<ObjectHandler::property_t> >
                                    floatingLegAnalysis(const QuantLib::Date&);
    };

}

#endif
